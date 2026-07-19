# =====================
# scripts/update_players.R (STRICT SCHEMA CLEAN VERSION)
# =====================

# Ensure required packages are available even if the CI environment hasn't
# pre-installed them (avoids "there is no package called X" hard failures).
required_packages <- c("httr", "jsonlite", "dplyr", "nflreadr")
missing_packages  <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(httr)
library(jsonlite)
library(dplyr)
library(nflreadr)

# =====================
# CONFIG
# =====================

OUTPUT_DIR <- "data/sleeperAPI"

PLAYERS_FILE <- file.path(OUTPUT_DIR, "sleeper_players.json")
TRENDING_ADDS_FILE <- file.path(OUTPUT_DIR, "trending_adds.json")
TRENDING_DROPS_FILE <- file.path(OUTPUT_DIR, "trending_drops.json")

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# =====================
# HELPERS
# =====================

fetch_json <- function(url) {

  res <- tryCatch(GET(url), error = function(e) NULL)

  if (is.null(res) || status_code(res) != 200) {
    stop(paste("Failed request:", url))
  }

  content(res, as = "text", encoding = "UTF-8")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =====================
# POSITION NORMALIZATION (SAFE) — Sleeper's own tag
# =====================

normalize_position <- function(pos) {

  pos <- toupper(pos %||% "")

  if (pos %in% c("DE", "DT", "LDT", "RDT")) return("DL")
  if (pos == "LB") return("LB")
  if (pos == "CB") return("CB")
  if (pos == "S") return("S")
  if (pos == "DB") return("DB")

  return(pos)
}

# =====================
# NFLREADR POSITION LOOKUP (gsis_id -> position)
# =====================

message("Loading nflreadr player positions...")

nflreadr_positions <- tryCatch(
  nflreadr::load_players() %>%
    filter(!is.na(gsis_id), gsis_id != "") %>%
    transmute(gsis_id = as.character(gsis_id), nflreadr_position = position) %>%
    distinct(gsis_id, .keep_all = TRUE),
  error = function(e) {
    message("Failed to load nflreadr player positions: ", e$message)
    tibble(gsis_id = character(0), nflreadr_position = character(0))
  }
)

# Named vector for fast lookup: gsis_id -> position
nflreadr_position_map <- setNames(
  nflreadr_positions$nflreadr_position,
  nflreadr_positions$gsis_id
)

message("nflreadr positions loaded for ", length(nflreadr_position_map), " players")

# =====================
# ESPN POSITION OVERRIDE (same approach as generate_weekly_stats.R)
# =====================
# ESPN's roster API tags positions more specifically than nflreadr's own
# load_players() (e.g. distinguishes CB/S/FS/SS rather than lumping them),
# so we prefer it when available and fall back to nflreadr's raw tag.

message("Fetching ESPN team list...")

espn_teams_json <- tryCatch(
  jsonlite::fromJSON(
    "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams?limit=32",
    simplifyVector = FALSE
  ),
  error = function(e) {
    message("ESPN team list request failed: ", e$message, " — position override disabled")
    NULL
  }
)

espn_team_ids     <- character(0)
espn_team_abbrevs <- character(0)  # named vector: team_id -> abbreviation
if (!is.null(espn_teams_json)) {
  espn_team_ids <- tryCatch(
    sapply(espn_teams_json$sports[[1]]$leagues[[1]]$teams, function(t) t$team$id),
    error = function(e) character(0)
  )
  espn_team_abbrevs <- tryCatch(
    sapply(espn_teams_json$sports[[1]]$leagues[[1]]$teams, function(t) t$team$abbreviation),
    error = function(e) character(0)
  )
  if (length(espn_team_abbrevs) == length(espn_team_ids)) {
    names(espn_team_abbrevs) <- espn_team_ids
  }
}

fetch_espn_roster <- function(team_id, team_abbr = NA_character_) {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams/%s/roster", team_id)
  parsed <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) {
      message("Failed to fetch ESPN roster for team ", team_id, ": ", e$message)
      NULL
    }
  )
  if (is.null(parsed) || is.null(parsed$athletes)) return(NULL)

  athletes <- unlist(lapply(parsed$athletes, function(group) group$items), recursive = FALSE)

  rows <- lapply(athletes, function(a) {
    tryCatch(
      data.frame(
        espn_id       = as.character(a$id),
        espn_position = a$position$abbreviation %||% NA_character_,
        espn_team     = team_abbr,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
  })
  bind_rows(Filter(Negate(is.null), rows))
}

espn_roster_all <- if (length(espn_team_ids) > 0) {
  bind_rows(lapply(espn_team_ids, function(tid) {
    Sys.sleep(0.15)  # polite pacing against ESPN's unofficial API
    fetch_espn_roster(tid, team_abbr = unname(espn_team_abbrevs[as.character(tid)]))
  })) %>% distinct(espn_id, .keep_all = TRUE)
} else {
  message("No ESPN team IDs available — position override disabled")
  tibble(espn_id = character(0), espn_position = character(0), espn_team = character(0))
}

message("ESPN roster players fetched: ", nrow(espn_roster_all))

# Direct lookup keyed by ESPN's own id, so we can use Sleeper's p$espn_id
# straight away without needing to round-trip through gsis_id first.
espn_position_by_espn_id <- setNames(espn_roster_all$espn_position, espn_roster_all$espn_id)

# Sleeper's own gsis_id field is frequently blank for recent rookies (their
# crosswalk lags), even when nflreadr already has the player. To close that
# gap, also build fallback crosswalks that resolve gsis_id via Sleeper's own
# player_id or espn_id, using nflreadr's broader ID crosswalk table.
message("Loading ID crosswalk for gsis_id fallback resolution...")

id_crosswalk_full <- tryCatch(
  nflreadr::load_ff_playerids(),
  error = function(e) {
    message("Failed to load ff_playerids crosswalk: ", e$message)
    tibble()
  }
)

sleeper_to_gsis_map <- if (all(c("sleeper_id", "gsis_id") %in% names(id_crosswalk_full))) {
  xwalk <- id_crosswalk_full %>%
    filter(!is.na(sleeper_id), sleeper_id != "", !is.na(gsis_id), gsis_id != "") %>%
    transmute(sleeper_id = as.character(sleeper_id), gsis_id = as.character(gsis_id)) %>%
    distinct(sleeper_id, .keep_all = TRUE)
  setNames(xwalk$gsis_id, xwalk$sleeper_id)
} else {
  character(0)
}

espn_to_gsis_map <- if (all(c("espn_id", "gsis_id") %in% names(id_crosswalk_full))) {
  xwalk <- id_crosswalk_full %>%
    filter(!is.na(espn_id), espn_id != "", !is.na(gsis_id), gsis_id != "") %>%
    transmute(espn_id = as.character(espn_id), gsis_id = as.character(gsis_id)) %>%
    distinct(espn_id, .keep_all = TRUE)
  setNames(xwalk$gsis_id, xwalk$espn_id)
} else {
  character(0)
}

# gsis_id -> espn_position, for cases where we only resolved a gsis_id
# (e.g. via the sleeper_id crosswalk) and need to check ESPN's tag too.
espn_position_by_gsis <- if (length(espn_to_gsis_map) > 0 && nrow(espn_roster_all) > 0) {
  gsis_to_espn_id <- setNames(names(espn_to_gsis_map), unname(espn_to_gsis_map))
  espn_pos_lookup <- espn_position_by_espn_id[gsis_to_espn_id]
  names(espn_pos_lookup) <- names(gsis_to_espn_id)
  espn_pos_lookup[!is.na(espn_pos_lookup)]
} else {
  character(0)
}

message("Sleeper-id -> gsis_id fallback entries: ", length(sleeper_to_gsis_map))
message("ESPN-id -> gsis_id fallback entries: ", length(espn_to_gsis_map))

# Resolves a player's nflreadr-sourced position the same way
# generate_weekly_stats.R does: prefer ESPN's tag (more specific), fall back
# to nflreadr's raw load_players() tag. Tries gsis_id first, then falls back
# through Sleeper's player_id and espn_id if gsis_id is missing/unmatched.
resolve_nflreadr_position <- function(gsis_id, sleeper_id, espn_id) {

  # Defensive coercion — these lookups use named-vector `[[`, which does
  # POSITIONAL indexing (not name lookup) if given a numeric argument.
  gsis_id    <- as.character(gsis_id)
  sleeper_id <- as.character(sleeper_id)
  espn_id    <- as.character(espn_id)

  # Try ESPN directly via Sleeper's own espn_id first — no round-trip needed
  if (!is.na(espn_id) && espn_id != "" && espn_id %in% names(espn_position_by_espn_id)) {
    espn_pos <- espn_position_by_espn_id[[espn_id]]
    if (!is.na(espn_pos) && espn_pos != "") return(espn_pos)
  }

  # Resolve an effective gsis_id via the fallback chain
  resolved_gsis <- NA_character_
  if (!is.na(gsis_id) && gsis_id != "") {
    resolved_gsis <- gsis_id
  } else if (!is.na(sleeper_id) && sleeper_id != "" && sleeper_id %in% names(sleeper_to_gsis_map)) {
    resolved_gsis <- as.character(sleeper_to_gsis_map[[sleeper_id]])
  } else if (!is.na(espn_id) && espn_id != "" && espn_id %in% names(espn_to_gsis_map)) {
    resolved_gsis <- as.character(espn_to_gsis_map[[espn_id]])
  }

  if (!is.na(resolved_gsis) && resolved_gsis %in% names(espn_position_by_gsis)) {
    return(espn_position_by_gsis[[resolved_gsis]])
  }

  if (!is.na(resolved_gsis) && resolved_gsis %in% names(nflreadr_position_map)) {
    return(nflreadr_position_map[[resolved_gsis]])
  }

  NA_character_
}

# =====================
# DOWNLOAD RAW DATA
# =====================

message("Downloading Sleeper player database...")

players_json <- fetch_json(
  "https://api.sleeper.app/v1/players/nfl"
)

writeLines(players_json, PLAYERS_FILE)

message("Saved raw sleeper_players.json")

# =====================
# PARSE RAW
# =====================

players_raw <- fromJSON(
  PLAYERS_FILE,
  simplifyVector = FALSE
)

# =====================
# BUILD CLEAN PLAYER OBJECTS (STRICT SCHEMA)
# =====================

players_clean <- lapply(players_raw, function(p) {

  if (is.null(p$first_name) || is.null(p$last_name)) return(NULL)

  status <- p$status %||% "Active"
  if (!is.na(status) && status != "Active") return(NULL)

  # IMPORTANT: force these to character explicitly. If any of these come back
  # from the JSON parse as numeric, `x[[numeric_id]]` on a named vector does
  # POSITIONAL indexing instead of name lookup, which throws "subscript out
  # of bounds" once the id exceeds the vector's length.
  gsis_id    <- as.character(p$gsis_id %||% NA_character_)
  sleeper_id <- as.character(p$player_id %||% NA_character_)
  espn_id    <- as.character(p$espn_id %||% NA_character_)

  nflreadr_position <- resolve_nflreadr_position(gsis_id, sleeper_id, espn_id)

  list(

    player_id = p$player_id %||% NA,
    first_name = p$first_name,
    last_name = p$last_name,
    full_name = paste(p$first_name, p$last_name),

    position_listed_on_sleeper = normalize_position(p$position),
    position_listed_on_nflreadr = nflreadr_position,
    team = p$team %||% NA,

    depth_chart_order = {
      d <- suppressWarnings(as.numeric(p$depth_chart_order))
      if (is.na(d) || length(d) == 0) 99 else d
    },

    status = status,

    fantasy_positions = p$fantasy_positions %||% NULL
  )
})

players_clean <- Filter(Negate(is.null), players_clean)

# =====================
# FINAL DATAFRAME (SAFE)
# =====================

players_df <- bind_rows(players_clean)

message("Clean players loaded: ", nrow(players_df))

# =====================
# SAVE STRICT JSON (ONLY REQUIRED FIELDS)
# =====================

write_json(
  players_df,
  PLAYERS_FILE,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

message("✅ STRICT player index saved")

# =====================
# LOOKUP TABLE
# =====================

players_lookup <- split(players_df, players_df$player_id)

# =====================
# TRENDING ENRICHMENT (MATCHES SCHEMA)
# =====================

enrich_trending <- function(json_text) {

  trending <- fromJSON(json_text, simplifyDataFrame = TRUE)

  if (is.null(trending) || !is.data.frame(trending) || nrow(trending) == 0) {
    return(list())
  }

  output <- list()

  for (i in seq_len(nrow(trending))) {

    pid <- as.character(trending$player_id[i])
    player <- players_lookup[[pid]]

    if (is.null(player)) next

    output[[length(output) + 1]] <- list(

      player_id = pid,
      count = trending$count[i],

      first_name = player$first_name,
      last_name = player$last_name,
      full_name = player$full_name,

      position_listed_on_sleeper = player$position_listed_on_sleeper,
      position_listed_on_nflreadr = player$position_listed_on_nflreadr,
      team = player$team,

      depth_chart_order = player$depth_chart_order,
      status = player$status,

      fantasy_positions = player$fantasy_positions
    )
  }

  output
}

# =====================
# DOWNLOAD TRENDING
# =====================

download_trending <- function(type, output_file) {

  message(paste("Downloading trending", type, "..."))

  url <- paste0(
    "https://api.sleeper.app/v1/players/nfl/trending/",
    type,
    "?lookback_hours=24&limit=100"
  )

  json_text <- fetch_json(url)
  enriched <- enrich_trending(json_text)

  write_json(
    enriched,
    output_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message(paste("Saved", basename(output_file)))
}

# =====================
# RUN
# =====================

download_trending("add", TRENDING_ADDS_FILE)
download_trending("drop", TRENDING_DROPS_FILE)

message("🎯 All Sleeper files updated successfully.")
