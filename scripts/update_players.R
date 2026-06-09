# =====================
# scripts/update_players.R (CLEAN PLAYER INDEX VERSION)
# =====================

library(httr)
library(jsonlite)

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
# POSITION NORMALIZATION
# =====================

normalize_position <- function(pos) {

  pos <- toupper(pos %||% "")

  # Defensive Line grouping
  if (pos %in% c("DE", "DT", "LDT", "RDT")) return("DL")

  # Linebacker
  if (pos == "LB") return("LB")

  # Defensive Back split
  if (pos == "CB") return("CB")
  if (pos == "S") return("S")

  # ambiguous DB → keep but resolve later in downstream logic
  if (pos == "DB") return("DB")

  return(pos)
}

# =====================
# DOWNLOAD PLAYER DATABASE
# =====================

message("Downloading Sleeper player database...")

players_json <- fetch_json(
  "https://api.sleeper.app/v1/players/nfl"
)

writeLines(players_json, PLAYERS_FILE)

message("Saved raw sleeper_players.json")

# =====================
# PARSE + REDUCE PLAYERS
# =====================

players_raw <- fromJSON(
  PLAYERS_FILE,
  simplifyVector = FALSE
)

players_clean <- lapply(players_raw, function(p) {

  if (is.null(p$first_name) || is.null(p$last_name)) return(NULL)

  pos <- normalize_position(p$position)

  # only keep active or relevant players
  status <- p$status %||% "Active"
  if (!is.na(status) && status != "Active") return(NULL)

  list(
    player_id = p$player_id %||% NA,
    full_name = paste(p$first_name, p$last_name),
    first_name = p$first_name,
    last_name = p$last_name,

    position = pos,
    team = p$team %||% NA,

    depth_chart_order = {
      d <- suppressWarnings(as.numeric(p$depth_chart_order))
      if (is.na(d) || length(d) == 0) 99 else d
    },

    fantasy_positions = p$fantasy_positions %||% NULL
  )
})

players_clean <- Filter(Negate(is.null), players_clean)

# =====================
# FINAL CLEAN DATAFRAME
# =====================

players_df <- do.call(rbind, lapply(players_clean, as.data.frame))

message("Clean players loaded: ", nrow(players_df))

# =====================
# SAVE CLEAN PLAYER FILE
# =====================

write_json(
  players_df,
  PLAYERS_FILE,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

message("✅ Clean player index saved")

# =====================
# TRENDING SECTION (UNCHANGED LOGIC, LIGHTLY CLEANED)
# =====================

players_lookup <- setNames(
  split(players_df, players_df$player_id),
  players_df$player_id
)

enrich_trending <- function(json_text) {

  trending <- fromJSON(json_text, simplifyDataFrame = TRUE)

  if (is.null(trending) || nrow(trending) == 0) return(list())

  output <- list()

  for (i in seq_len(nrow(trending))) {

    pid <- as.character(trending$player_id[i])
    player <- players_lookup[[pid]]

    if (is.null(player)) next

    output[[length(output) + 1]] <- list(
      player_id = pid,
      count = trending$count[i],

      full_name = player$full_name,
      position = player$position,
      team = player$team,

      depth_chart_order = player$depth_chart_order,
      fantasy_positions = player$fantasy_positions
    )
  }

  output
}

download_trending <- function(type, output_file) {

  message("Downloading trending ", type, "...")

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

  message("Saved ", basename(output_file))
}

download_trending("add", TRENDING_ADDS_FILE)
download_trending("drop", TRENDING_DROPS_FILE)

message("🎯 All Sleeper files updated successfully.")
