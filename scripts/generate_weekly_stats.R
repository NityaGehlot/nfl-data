# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# Includes Sleeper-accurate K + DEF fantasy scoring
# DEF points allowed derived from SCHEDULES (official scores)

library(nflreadr)
library(dplyr)
library(jsonlite)
library(tidyr)

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
# KICKER FANTASY SCORING (Sleeper)
# =====================
weekly <- weekly %>%
  mutate(
    fg_0_19  = coalesce(fg_made_0_19, 0),
    fg_20_29 = coalesce(fg_made_20_29, 0),
    fg_30_39 = coalesce(fg_made_30_39, 0),
    fg_40_49 = coalesce(fg_made_40_49, 0),
    fg_50_59 = coalesce(fg_made_50_59, 0),
    fg_60p   = coalesce(fg_made_60_, 0),

    fantasy_points_ppr = ifelse(
      position == "K",
      (fg_0_19 * 3) +
      (fg_20_29 * 3) +
      (fg_30_39 * 3) +
      (fg_40_49 * 4) +
      (fg_50_59 * 5) +
      (fg_60p   * 5) +
      (pat_made * 1) -
      (fg_missed * 1) -
      (pat_missed * 1),
      fantasy_points_ppr
    )
  )

# =====================
# PLAYER COLUMN SELECTION
# =====================
desired_cols <- c(
  "season","week","player_id","player_name","position","team","opponent_team",
  "completions","attempts","passing_yards","passing_tds","passing_interceptions",
  "carries","rushing_yards","rushing_tds",
  "targets","receptions","receiving_yards","receiving_tds",
  "fumbles","fantasy_points_ppr","headshot_url",
  "fg_made","fg_att","fg_missed",
  "fg_0_19","fg_20_29","fg_30_39","fg_40_49","fg_50_59","fg_60p",
  "pat_made","pat_att","pat_missed"
)

weekly_clean <- weekly %>%
  select(any_of(desired_cols))

# =====================
# POSITION FILTERING
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

  K  = c(
    "fg_made","fg_att","fg_missed",
    "fg_0_19","fg_20_29","fg_30_39","fg_40_49","fg_50_59","fg_60p",
    "pat_made","pat_att","pat_missed"
  )
)

base_cols <- c(
  "season","week","player_id","player_name",
  "position","team","opponent_team",
  "headshot_url","fantasy_points_ppr"
)

player_list <- apply(weekly_clean, 1, function(row) {
  pos <- row[["position"]]
  keep <- intersect(c(base_cols, position_cols[[pos]]), names(row))
  as.list(row[keep])
})

# =====================
# DEF POINTS ALLOWED (FROM SCHEDULES)
# =====================
message("Loading schedules for DEF points allowed")

schedules <- nflreadr::load_schedules(seasons = season)

def_points_allowed <- schedules %>%
  filter(
    game_type == "REG",
    !is.na(home_score),
    !is.na(away_score)
  ) %>%
  transmute(
    season,
    week,
    home_team,
    away_team,
    home_points_allowed = away_score,
    away_points_allowed = home_score
  ) %>%
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = "side",
    values_to = "team"
  ) %>%
  mutate(
    points_allowed = ifelse(
      side == "home_team",
      home_points_allowed,
      away_points_allowed
    )
  ) %>%
  select(season, week, team, points_allowed)

# =====================
# LOAD TEAM DEF STATS
# =====================
message("Loading official weekly TEAM DEF stats")

team_weekly <- nflreadr::load_team_stats(seasons = season)

team_def <- team_weekly %>%
  filter(!is.na(week)) %>%
  left_join(
    def_points_allowed,
    by = c("season","week","team")
  ) %>%
  mutate(
    fantasy_points_ppr =
      (def_sacks * 1) +
      (def_interceptions * 2) +
      (fumble_recovery_own * 2) +
      ((def_tds + special_teams_tds) * 6) +
      (def_safeties * 2) +
      case_when(
        points_allowed == 0  ~ 10,
        points_allowed <= 6  ~ 7,
        points_allowed <= 13 ~ 4,
        points_allowed <= 20 ~ 1,
        points_allowed <= 27 ~ 0,
        points_allowed <= 34 ~ -1,
        TRUE ~ -4
      )
  ) %>%
  transmute(
    season,
    week,
    player_id = paste0("DEF_", team),
    player_name = paste(team, "DEF"),
    position = "DEF",
    team,
    opponent_team,
    sacks = def_sacks,
    interceptions = def_interceptions,
    fumbles_recovered = fumble_recovery_own,
    defensive_tds = def_tds + special_teams_tds,
    safeties = def_safeties,
    points_allowed,
    fantasy_points_ppr
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
