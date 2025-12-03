# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# Compatible for any season, safely handles missing columns

library(nflreadr)
library(dplyr)
library(jsonlite)
library(data.table)

# === CONFIG ===
season <- 2025   # always fetch the current NFL season; can also use Sys.getenv("SEASON")
out_path <- "data/weekly_stats.json"

message("Loading official weekly player stats for season: ", season)

# Load weekly player stats
weekly <- tryCatch(
  nflreadr::load_player_stats(seasons = season),
  error = function(e) {
    stop("Failed to load weekly stats: ", e$message)
  }
)

# Define columns you want to keep
desired_cols <- c(
  "season", "week", "player_id", "player_name", "position", "team",
  "completions", "attempts", "passing_yards", "passing_tds",
  "interceptions", "carries", "rushing_yards", "rushing_tds",
  "receptions", "receiving_yards", "receiving_tds",
  "fumbles", "fantasy_points_ppr"
)

# Only select columns that actually exist in the data
existing_cols <- intersect(desired_cols, colnames(weekly))
weekly_clean <- weekly[, existing_cols, with = FALSE]

# Convert to data.table for efficiency
weekly_dt <- as.data.table(weekly_clean)

# Ensure output folder exists
if (!dir.exists("data")) dir.create("data")

# Write JSON file
json_string <- jsonlite::toJSON(
  weekly_dt,
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)
write(json_string, file = out_path)

message("Wrote weekly stats to: ", out_path)
