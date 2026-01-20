# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# Includes player stats + team DEF stats
# Season-flexible & React Native friendly

library(nflreadr)
library(dplyr)
library(jsonlite)
library(data.table)

# =====================
# CONFIG
# =====================
current_year <- as.numeric(format(Sys.Date(), "%Y"))
latest_season <- nflreadr::most_recent_season()
season <- min(current_year, latest_season)

output_name <- paste0("player_stats_", season, ".json")
out_path <- file.path("data", output_name)

# =====================
# LOAD PLAYER STATS
# =====================
message("Loading official weekly PLAYER stats for season: ", season)

weekly <- tryCatch(
  nflreadr::load_player_stats(seasons = season),
  error = function(e) stop("Failed to load weekly stats: ", e$message)
)

# Columns to keep (future-proof)
desired_cols <- c(
  "season", "week", "player_id", "player_name", "position", "team", "opponent_team",
  "completions", "attempts", "passing_yards", "passing_tds",
  "passing_interceptions", "passing_cpoe", "passing_epa",
  "carries", "rushing_yards", "rushing_tds", "rushing_epa",
  "targets", "receptions", "receiving_yards", "receiving_tds",
  "receiving_fumbles", "receiving_epa", "target_share",
  "fumbles", "fantasy_points_ppr", "headshot_url",
  "fg_made", "fg_att", "fg_missed", "fg_blocked", "fg_long",
  "fg_pct", "pat_made", "pat_att", "pat_missed", "pat_blocked", "pat_pct"
)

existing_cols <- intersect(desired_cols, colnames(weekly))
weekly_clean <- weekly[, existing_cols, drop = FALSE]
weekly_clean <- as.data.frame(weekly_clean)

# =====================
# POSITION-BASED FILTERING
# =====================
position_cols <- list(
  QB = c("completions","attempts","passing_yards","passing_tds",
         "passing_interceptions","passing_cpoe","passing_epa",
         "carries","rushing_yards","rushing_tds","rushing_epa","fumbles"),

  RB = c("carries","rushing_yards","rushing_tds","rushing_epa",
         "receptions","receiving_yards","receiving_tds",
         "targets","receiving_fumbles","receiving_epa","target_share","fumbles"),

  WR = c("receptions","receiving_yards","receiving_tds",
         "targets","receiving_fumbles","receiving_epa","target_share",
         "carries","rushing_yards","rushing_tds","rushing_epa","fumbles"),

  TE = c("receptions","receiving_yards","receiving_tds",
         "targets","receiving_fumbles","receiving_epa","target_share",
         "carries","rushing_yards","rushing_tds","rushing_epa","fumbles"),

  K  = c("fg_made","fg_att","fg_missed","fg_blocked","fg_long",
         "fg_pct","pat_made","pat_att","pat_missed","pat_blocked","pat_pct",)
)

base_cols <- c(
  "season","week","player_id","player_name",
  "position","team","opponent_team",
  "headshot_url","fantasy_points_ppr"
)

player_list <- apply(weekly_clean, 1, function(row) {
  pos <- row[["position"]]
  pos_stats <- position_cols[[pos]]
  if (is.null(pos_stats)) pos_stats <- character(0)

  keep <- unique(c(base_cols, pos_stats))
  keep <- intersect(keep, names(row))

  as.list(row[keep])
})

# =====================
# LOAD TEAM DEF STATS
# =====================
message("Loading official weekly TEAM DEF stats")

team_weekly <- tryCatch(
  nflreadr::load_team_stats(seasons = season),
  error = function(e) stop("Failed to load team stats: ", e$message)
)

team_def <- team_weekly %>%
  filter(!is.na(week)) %>%
  transmute(
    season = season,
    week = week,
    player_id = paste0("DEF_", team),
    player_name = paste(team, "DEF"),
    position = "DEF",
    team = team,
    opponent_team = opponent_team,

    sacks = def_sacks,
    interceptions = def_interceptions,
    fumbles_forced = def_fumbles_forced,
    fumbles_recovered = fumble_recovery_own,
    defensive_tds = def_tds + special_teams_tds,
    safeties = def_safeties,
    
    # points_allowed = points_allowed,
    # yards_allowed = yards_allowed,
    # fantasy_points_ppr = fantasy_points
    
  )

def_list <- apply(as.data.frame(team_def), 1, function(row) {
  as.list(row)
})

# =====================
# EXPORT JSON
# =====================
all_players <- c(player_list, def_list)

if (!dir.exists("data")) dir.create("data")

jsonlite::write_json(
  all_players,
  out_path,
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)

message("Success! JSON exported → ", out_path)
