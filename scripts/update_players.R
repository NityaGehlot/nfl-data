# =====================
# scripts/update_players.R
# DAILY CACHE VERSION
# =====================

library(httr)
library(jsonlite)

OUTPUT_FILE <- "data/sleeperAPI/sleeper_players.json"
TRENDING_ADDS_FILE <- "data/sleeperAPI/trending_adds.json"
TRENDING_DROPS_FILE <- "data/sleeperAPI/trending_drops.json"

# =====================
# SKIP IF UPDATED < 24 HOURS AGO
# =====================
if (file.exists(OUTPUT_FILE)) {

  file_info <- file.info(OUTPUT_FILE)
  last_modified <- file_info$mtime

  hours_since_update <- as.numeric(
    difftime(
      Sys.time(),
      last_modified,
      units = "hours"
    )
  )

  if (!is.na(hours_since_update) &&
      hours_since_update < 24) {

    message(
      "Skipping Sleeper update (last updated ",
      round(hours_since_update, 2),
      " hours ago)"
    )

    quit(save = "no", status = 0)
  }
}

# =====================
# CREATE OUTPUT DIRECTORY
# =====================
output_dir <- dirname(OUTPUT_FILE)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# =====================
# FETCH PLAYER DATABASE
# =====================
message("Fetching Sleeper players...")

players_res <- tryCatch(
  GET("https://api.sleeper.app/v1/players/nfl"),
  error = function(e) NULL
)

if (is.null(players_res) || status_code(players_res) != 200) {
  stop("Failed to fetch Sleeper players")
}

players_json <- content(
  players_res,
  as = "text",
  encoding = "UTF-8"
)

writeLines(players_json, OUTPUT_FILE)

message("Saved sleeper_players.json")

# =====================
# LOAD PLAYER LOOKUP
# =====================
players_lookup <- fromJSON(
  OUTPUT_FILE,
  simplifyDataFrame = FALSE
)

# =====================
# HELPER FUNCTION
# =====================
enrich_trending <- function(trending_json, players_lookup) {

  trending <- fromJSON(
    trending_json,
    simplifyDataFrame = TRUE
  )

  if (length(trending) == 0 || nrow(trending) == 0) {
    return(list())
  }

  results <- lapply(seq_len(nrow(trending)), function(i) {

    pid <- as.character(trending$player_id[i])

    player <- players_lookup[[pid]]

    if (is.null(player)) {
      return(NULL)
    }

    list(
      player_id = pid,
      count = trending$count[i],

      first_name = player$first_name,
      last_name = player$last_name,
      full_name = paste(
        player$first_name,
        player$last_name
      ),

      position = player$position,
      team = player$team,

      age = player$age,
      status = player$status,
      injury_status = player$injury_status,

      fantasy_positions = player$fantasy_positions,

      depth_chart_order = player$depth_chart_order,
      depth_chart_position = player$depth_chart_position,

      years_exp = player$years_exp,
      college = player$college,

      height = player$height,
      weight = player$weight,

      number = player$number
    )
  })

  Filter(Negate(is.null), results)
}

# =====================
# FETCH TRENDING ADDS
# =====================
message("Fetching trending adds...")

adds_res <- tryCatch(
  GET(
    "https://api.sleeper.app/v1/players/nfl/trending/add?lookback_hours=24&limit=100"
  ),
  error = function(e) NULL
)

if (!is.null(adds_res) &&
    status_code(adds_res) == 200) {

  adds_json <- content(
    adds_res,
    as = "text",
    encoding = "UTF-8"
  )

  adds_enriched <- enrich_trending(
    adds_json,
    players_lookup
  )

  write_json(
    adds_enriched,
    TRENDING_ADDS_FILE,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Saved trending_adds.json")

} else {

  warning("Failed to fetch trending adds")

}

# =====================
# FETCH TRENDING DROPS
# =====================
message("Fetching trending drops...")

drops_res <- tryCatch(
  GET(
    "https://api.sleeper.app/v1/players/nfl/trending/drop?lookback_hours=24&limit=100"
  ),
  error = function(e) NULL
)

if (!is.null(drops_res) &&
    status_code(drops_res) == 200) {

  drops_json <- content(
    drops_res,
    as = "text",
    encoding = "UTF-8"
  )

  drops_enriched <- enrich_trending(
    drops_json,
    players_lookup
  )

  write_json(
    drops_enriched,
    TRENDING_DROPS_FILE,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Saved trending_drops.json")

} else {

  warning("Failed to fetch trending drops")

}

message("All Sleeper files updated successfully")
