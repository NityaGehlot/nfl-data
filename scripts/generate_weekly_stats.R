# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# Future-proof & season-flexible JSON export for React Native usage

library(nflreadr)
library(dplyr)
library(jsonlite)
library(data.table)

# === CONFIG ===
current_year <- as.numeric(format(Sys.Date(), "%Y"))
latest_season <- nflreadr::most_recent_season()
season <- min(current_year, latest_season)

output_name <- paste0("player_stats_", season, ".json")
out_path <- file.path("data", output_name)

message("Loading official weekly player stats for season: ", season)

weekly <- tryCatch(
  nflreadr::load_player_stats(seasons = season),
  error = function(e) stop("Failed to load weekly stats: ", e$message)
)

# Columns to keep (future-proof — selects only those that exist)
desired_cols <- c(
  "season", "week", "player_id", "player_name", "position", "team",
  "completions", "attempts", "passing_yards", "passing_tds",
  "interceptions", "carries", "rushing_yards", "rushing_tds", "targets",
  "receptions", "receiving_yards", "receiving_tds",
  "fumbles", "fantasy_points_ppr", "headshot_url"
)

existing_cols <- intersect(desired_cols, colnames(weekly))
weekly_clean <- weekly[, existing_cols, drop = FALSE]

# Convert to data.table
weekly_dt <- as.data.table(weekly_clean)

# Create output folder if needed
if (!dir.exists("data")) dir.create("data")

# Write JSON — pretty & compatible with JavaScript import
jsonlite::write_json(
  weekly_dt,
  out_path,
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)

message("Success! JSON exported → ", out_path)
