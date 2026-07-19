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

# Named vector for fast lookup inside the per-player loop: gsis_id -> position
nflreadr_position_map <- setNames(
  nflreadr_positions$nflreadr_position,
  nflreadr_positions$gsis_id
)

message("nflreadr positions loaded for ", length(nflreadr_position_map), " players")

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

  gsis_id <- p$gsis_id %||% NA_character_
  nflreadr_position <- if (!is.na(gsis_id) && gsis_id %in% names(nflreadr_position_map)) {
    nflreadr_position_map[[gsis_id]]
  } else {
    NA_character_
  }

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
