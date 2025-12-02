# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr

library(nflreadr)
library(dplyr)
library(jsonlite)
library(data.table)

# === CONFIG ===
season <- 2025   # <-- always fetch current 2025 NFL season
out_path <- "data/weekly_stats.json"

message("Loading official weekly player stats for season: ", season)

# nflreadr provides built-in weekly stats (clean weekly aggregates)
weekly <- tryCatch(
  nflreadr::load_player_stats(seasons = season),
  error = function(e) {
    stop("Failed to load weekly stats: ", e$message)
  }
)

# Keep only useful stat fields
weekly_clean <- weekly %>%
  select(
    season,
    week,
    player_id,
    player_name,
    position,
    team,
    completions,
    attempts,
    passing_yards,
    passing_tds,
    interceptions,
    carries,
    rushing_yards,
    rushing_tds,
    receptions,
    receiving_yards,
    receiving_tds,
    fumbles,
    fantasy_points_ppr
  )

# Convert to data.table
weekly_dt <- as.data.table(weekly_clean)

# Ensure output folder exists
if (!dir.exists("data")) dir.create("data")

# Write JSON
json_string <- jsonlite::toJSON(
  weekly_dt,
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)

write(json_string, file = out_path)

message("Wrote weekly stats to: ", out_path)
