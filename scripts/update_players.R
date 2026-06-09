# =====================
# scripts/update_players.R
# Updates:
#   - sleeper_players.json
#   - trending_adds.json
#   - trending_drops.json
# =====================

library(httr)
library(jsonlite)

# =====================
# CONFIG
# =====================

OUTPUT_DIR <- "data/sleeperAPI"

PLAYERS_FILE <- file.path(
  OUTPUT_DIR,
  "sleeper_players.json"
)

TRENDING_ADDS_FILE <- file.path(
  OUTPUT_DIR,
  "trending_adds.json"
)

TRENDING_DROPS_FILE <- file.path(
  OUTPUT_DIR,
  "trending_drops.json"
)

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

# =====================
# HELPER
# =====================

fetch_json <- function(url) {

  res <- tryCatch(
    GET(url),
    error = function(e) NULL
  )

  if (is.null(res)) {
    stop(paste("Failed to connect to", url))
  }

  if (status_code(res) != 200) {
    stop(
      paste(
        "Request failed:",
        status_code(res),
        url
      )
    )
  }

  content(
    res,
    as = "text",
    encoding = "UTF-8"
  )
}

# =====================
# DOWNLOAD PLAYER DATABASE
# =====================

message("Downloading Sleeper player database...")

players_json <- fetch_json(
  "https://api.sleeper.app/v1/players/nfl"
)

writeLines(
  players_json,
  PLAYERS_FILE
)

message("Saved sleeper_players.json")

# Load lookup table
players_lookup <- fromJSON(
  PLAYERS_FILE,
  simplifyVector = FALSE
)

# =====================
# ENRICH TRENDING PLAYERS
# =====================

enrich_trending <- function(json_text) {

  trending <- fromJSON(
    json_text,
    simplifyDataFrame = TRUE
  )

  if (is.null(trending)) {
    return(list())
  }

  if (!is.data.frame(trending)) {
    return(list())
  }

  if (nrow(trending) == 0) {
    return(list())
  }

  output <- vector(
    "list",
    nrow(trending)
  )

  for (i in seq_len(nrow(trending))) {

    pid <- as.character(
      trending$player_id[i]
    )

    player <- players_lookup[[pid]]

    if (is.null(player)) {
      next
    }

    output[[i]] <- list(

      player_id = pid,
      count = trending$count[i],

      first_name = player$first_name,
      last_name = player$last_name,
      full_name = paste(
        player$first_name,
        player$last_name
      ),

      team = player$team,
      position = player$position,

      status = player$status,
      injury_status = player$injury_status,

      fantasy_positions = player$fantasy_positions,

      age = player$age,

      years_exp = player$years_exp,

      college = player$college,

      number = player$number,

      height = player$height,

      weight = player$weight,

      depth_chart_order = player$depth_chart_order,
      depth_chart_position = player$depth_chart_position
    )
  }

  Filter(
    Negate(is.null),
    output
  )
}

# =====================
# DOWNLOAD TRENDING
# =====================

download_trending <- function(type, output_file) {

  message(
    paste(
      "Downloading trending",
      type,
      "..."
    )
  )

  url <- paste0(
    "https://api.sleeper.app/v1/players/nfl/trending/",
    type,
    "?lookback_hours=24&limit=100"
  )

  json_text <- fetch_json(url)

  enriched <- enrich_trending(
    json_text
  )

  write_json(
    enriched,
    output_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message(
    paste(
      "Saved",
      basename(output_file)
    )
  )
}

# =====================
# RUN
# =====================

download_trending(
  "add",
  TRENDING_ADDS_FILE
)

download_trending(
  "drop",
  TRENDING_DROPS_FILE
)

message("All Sleeper files updated successfully.")
