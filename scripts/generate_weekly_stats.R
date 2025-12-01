# scripts/generate_weekly_stats.R
# Generate per-player weekly stats JSON using nflreadr

library(nflreadr)
library(dplyr)
library(jsonlite)
library(lubridate)
library(data.table)

# === CONFIG ===
season <- as.integer(Sys.getenv("SEASON", "2024"))  # default season; change if needed
out_path <- "data/weekly_stats.json"

message("Loading play-by-play for season: ", season)
pbp <- tryCatch(
  nflreadr::load_pbp(season),
  error = function(e) {
    stop("Failed to load pbp: ", e$message)
  }
)

# Choose which basic stat columns you want; this list is a sensible start
weekly_stats <- pbp %>%
  mutate(
    player_id = coalesce(player_id, NA_character_)   # ensure column exists
  ) %>%
  # Gather passing/rushing/receiving stats into per-player-per-week rows
  group_by(week, player_id) %>%
  summarise(
    player_name = first(na.omit(c(player_name, receiver_player_name, rusher_player_name))),
    position = first(na.omit(position)),
    team = first(na.omit(posteam)),
    passing_att = sum(pass_attempt, na.rm = TRUE),
    passing_cmp = sum(complete_pass, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_td, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    rushing_att = sum(rush_attempt, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_td, na.rm = TRUE),
    receptions = sum(complete_pass * !is.na(receiver_player_id), na.rm=TRUE), # approximate receptions
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_td, na.rm = TRUE),
    fumbles = sum(fumble, na.rm = TRUE),
    fantasy_points = sum(fantasy, na.rm = TRUE), # nflreadr includes fantasy calculation
    .groups = "drop"
  )

# Convert to data.table (faster) then JSON
weekly_stats_dt <- as.data.table(weekly_stats)

# Ensure output folder exists
if (!dir.exists("data")) dir.create("data")

# Write JSON (records array)
json_string <- jsonlite::toJSON(weekly_stats_dt, pretty = TRUE, na = "null", auto_unbox = TRUE)
write(json_string, file = out_path)

message("Wrote weekly stats to: ", out_path)
