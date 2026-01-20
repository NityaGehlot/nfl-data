# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# Includes Sleeper-accurate K + DEF fantasy scoring

library(nflreadr)
library(dplyr)
library(jsonlite)

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
weekly <- nflreadr::load_player_stats(seasons = season)

# =====================
# SLEEPER KICKER SCORING
# =====================
weekly <- weekly %>%
  mutate(
    # Fantasy points for kickers based on distance buckets
    fantasy_points_ppr = ifelse(
      position == "K",
      (fg_made_0_19 * 3) +
      (fg_made_20_29 * 3) +
      (fg_made_30_39 * 3) +
      (fg_made_40_49 * 4) +
      (fg_made_50_59 * 5) +
      (fg_made_60_ * 5) +
      (pat_made * 1) -
      (fg_missed * 1) -
      (pat_missed * 1),
      fantasy_points_ppr  # leave others unchanged
    )
  )

# =====================
# SELECT PLAYER COLUMNS
# =====================
desired_cols <- c(
  "season","week","player_id","player_name","position","team","opponent_team",
  "completions","attempts","passing_yards","passing_tds","passing_interceptions",
  "carries","rushing_yards","rushing_tds",
  "targets","receptions","receiving_yards","receiving_tds",
  "fumbles","fantasy_points_ppr","headshot_url",
  "fg_made","fg_att","fg_missed",
  "fg_made_0_19","fg_made_20_29","fg_made_30_39",
  "fg_made_40_49","fg_made_50_59","fg_made_60_",
  "pat_made","pat_att","pat_missed"
)

weekly_clean <- weekly %>%
  select(any_of(desired_cols))

# =====================
# POSITION-BASED FILTERING
# =====================
position_cols <- list(
  QB = c("completions","attempts","passing_yards","passing_tds",
         "passing_interceptions","carries","rushing_yards","rushing_tds","fumbles"),

  RB = c("carries","rushing_yards","rushing_tds",
         "receptions","targets","receiving_yards","receiving_tds","fumbles"),

  WR = c("receptions","targets","receiving_yards","receiving_tds",
         "carries","rushing_yards","rushing_tds","fumbles"),

  TE = c("receptions","targets","receiving_yards","receiving_tds",
         "carries","rushing_yards","rushing_tds","fumbles"),

  K  = c("fg_made","fg_att","fg_missed",
         "fg_made_0_19","fg_made_20_29","fg_made_30_39",
         "fg_made_40_49","fg_made_50_59","fg_made_60_",
         "pat_made","pat_att","pat_missed")
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

  keep <- intersect(c(base_cols, pos_stats), names(row))
  as.list(row[keep])
})

# =====================
# LOAD TEAM DEF STATS
# =====================
message("Loading official weekly TEAM DEF stats")
team_weekly <- nflreadr::load_team_stats(seasons = season)

team_def <- team_weekly %>%
  filter(!is.na(week)) %>%
  mutate(
    # Sleeper fantasy scoring for defenses
    fantasy_points_ppr =
      (def_sacks * 1) +
      (def_interceptions * 2) +
      (fumble_recovery_own * 2) +
      ((def_tds + special_teams_tds) * 6) +
      (def_safeties * 2)
  ) %>%
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
    fumbles_recovered = fumble_recovery_own,
    defensive_tds = def_tds + special_teams_tds,
    safeties = def_safeties,
    fantasy_points_ppr = fantasy_points_ppr
  )

def_list <- apply(as.data.frame(team_def), 1, function(row) as.list(row))

# =====================
# EXPORT JSON
# =====================
all_players <- c(player_list, def_list)

if (!dir.exists("data")) dir.create("data")

write_json(
  all_players,
  out_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

message("✅ Success! JSON exported → ", out_path)
