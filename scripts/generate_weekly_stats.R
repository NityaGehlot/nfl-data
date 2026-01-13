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
  "season", "week", "player_id", "player_name", "position", "team", "opponent_team",
  "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions", "passing_cpoe", "passing_epa",
  "carries", "rushing_yards", "rushing_tds", "rushing_epa", 
  "targets", "receptions", "receiving_yards", "receiving_tds", "receiving_fumbles", "receiving_epa", "target_share",
  "fumbles", "fantasy_points_ppr", "headshot_url",
  "special_teams_tds", "fumble_recovery_tds",
  "fg_made", "fg_att", "fg_missed", "fg_blocked", "fg_long", "fg_pct", "pat_made", "pat_att", "pat_missed", "pat_blocked","pat_pct"
)

existing_cols <- intersect(desired_cols, colnames(weekly))
weekly_clean <- weekly[, existing_cols, drop = FALSE]

# =========================
# Position-based filtering
# =========================

# Which stats each position should keep
position_cols <- list(
  QB = c("completions","attempts","passing_yards","passing_tds","passing_interceptions","passing_cpoe","passing_epa","carries",
         "rushing_yards","rushing_tds","fumbles","rushing_epa"),
  RB = c("carries","rushing_yards","rushing_tds","fumbles","rushing_epa","target_share"),
  WR = c("receptions","receiving_yards","receiving_tds","targets","receiving_fumbles","receiving_epa","target_share"),
  TE = c("receptions","receiving_yards","receiving_tds","targets","receiving_fumbles","receiving_epa","target_share"),
  DEF = c("fantasy_points_ppr","special_teams_tds","fumble_recovery_tds",),
  K = c("fantasy_points_ppr","fg_made","fg_att","fg_missed","fg_blocked","fg_long","fg_pct","pat_made","pat_att","pat_missed",
        "pat_blocked","pat_pct")
)

# Columns always kept
base_cols <- c(
  "season","week","player_id","player_name",
  "position","team","opponent_team",
  "headshot_url","fantasy_points_ppr"
)

weekly_clean <- as.data.frame(weekly_clean)

player_list <- apply(weekly_clean, 1, function(row) {
  pos <- row[["position"]]
  pos_stats <- position_cols[[pos]]

  if (is.null(pos_stats)) pos_stats <- character(0)

  keep <- unique(c(base_cols, pos_stats))
  keep <- intersect(keep, names(row))

  as.list(row[keep])
})

# =========================
# Export JSON
# =========================

# Create output folder if needed
if (!dir.exists("data")) dir.create("data")

jsonlite::write_json(
  player_list,
  out_path,
  pretty = TRUE,
  na = "null",
  auto_unbox = TRUE
)

message("Success! JSON exported → ", out_path)
