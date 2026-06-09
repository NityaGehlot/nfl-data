# =====================
# scripts/update_players.R (CLEAN + SAFE + NORMALIZED)
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
# HELPER
# =====================

fetch_json <- function(url) {

  res <- tryCatch(GET(url), error = function(e) NULL)

  if (is.null(res)) {
    stop(paste("Failed to connect to", url))
  }

  if (status_code(res) != 200) {
    stop(paste("Request failed:", status_code(res), url))
  }

  content(res, as = "text", encoding = "UTF-8")
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =====================
# POSITION NORMALIZATION (NEW)
# =====================

normalize_position <- function(pos) {

  pos <- toupper(pos %||% "")

  # Defensive Line
  if (pos %in% c("DE", "DT", "LDT", "RDT")) return("DL")

  # Linebacker
  if (pos == "LB") return("LB")

  # Defensive Back split
  if (pos == "CB") return("CB")
  if (pos == "S") return("S")

  # DB is ambiguous → keep but safe
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

message("Saved sleeper_players.json")

# =====================
# LOAD RAW DATA
# =====================

players_lookup <- fromJSON(
  PLAYERS_FILE,
  simplifyVector = FALSE
)

# =====================
# ENRICH TRENDING PLAYERS (CLEANED)
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

    pos <- normalize_position(player$position)

    output[[length(output) + 1]] <- list(

      player_id = pid,
      count = trending$count[i],

      full_name = paste(player$first_name, player$last_name),
      first_name = player$first_name,
      last_name = player$last_name,

      team = player$team,
      position = pos,

      depth_chart_order = {
        d <- suppressWarnings(as.numeric(player$depth_chart_order))
        if (is.na(d) || length(d) == 0) 99 else d
      },

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
