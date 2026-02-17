# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# ✅ All players every week
# ✅ Sleeper-accurate K + DEF fantasy scoring
# ❌ No injuries (handled elsewhere)

library(nflreadr)
library(dplyr)
library(jsonlite)

# =====================
# CONFIG
# =====================
current_year <- as.numeric(format(Sys.Date(), "%Y"))
latest_season <- nflreadr::most_recent_season()
season <- min(current_year, latest_season)
weeks <- 1:18

output_name <- paste0("player_stats_", season, ".json")
out_path <- file.path("data", output_name)

# =====================
# LOAD ROSTERS
# =====================
message("Loading NFL rosters")

rosters <- nflreadr::load_rosters(seasons = season) %>%
  filter(position %in% c("QB","RB","WR","TE","K")) %>%
  transmute(
    player_id = gsis_id,
    player_name = full_name,
    team,
    position,
    headshot_url
  )

# =====================
# CREATE FULL PLAYER × WEEK GRID
# =====================
player_weeks <- expand.grid(
  player_id = rosters$player_id,
  week = weeks
) %>%
  left_join(rosters, by = "player_id") %>%
  mutate(season = season)

# =====================
# LOAD WEEKLY PLAYER STATS
# =====================
message("Loading weekly player stats")

weekly_stats <- nflreadr::load_player_stats(seasons = season)

player_weeks <- player_weeks %>%
  left_join(
    weekly_stats,
    by = c("player_id", "season", "week")
  )

# =====================
# FORCE REQUIRED COLUMNS TO EXIST
# =====================
required_cols <- c(
  "fantasy_points_ppr",
  "completions","attempts","passing_yards","passing_tds","passing_interceptions",
  "carries","rushing_yards","rushing_tds",
  "targets","receptions","receiving_yards","receiving_tds",
  "fumbles_lost",
  "fg_made_0_19","fg_made_20_29","fg_made_30_39",
  "fg_made_40_49","fg_made_50_59","fg_made_60_",
  "fg_att","fg_missed",
  "pat_made","pat_att","pat_missed"
)

missing_cols <- setdiff(required_cols, names(player_weeks))

if (length(missing_cols) > 0) {
  player_weeks[missing_cols] <- 0
}

# =====================
# NORMALIZE ALL STATS
# =====================
player_weeks <- player_weeks %>%
  mutate(across(all_of(required_cols), ~coalesce(.x, 0)))

# =====================
# KICKER FANTASY SCORING (Sleeper)
# =====================
player_weeks <- player_weeks %>%
  mutate(
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
      fantasy_points_ppr
    )
  )

# =====================
# DEF POINTS ALLOWED
# =====================
message("Loading DEF points allowed")

schedules <- nflreadr::load_schedules(seasons = season) %>%
  filter(game_type == "REG", !is.na(home_score))

home_def <- schedules %>%
  transmute(season, week, team = home_team, points_allowed = away_score)

away_def <- schedules %>%
  transmute(season, week, team = away_team, points_allowed = home_score)

def_points_allowed <- bind_rows(home_def, away_def)

# =====================
# TEAM DEF STATS
# =====================
team_weekly <- nflreadr::load_team_stats(seasons = season)

team_def <- team_weekly %>%
  filter(!is.na(week)) %>%
  left_join(def_points_allowed, by = c("season","week","team")) %>%
  mutate(
    fantasy_points_ppr =
      (coalesce(def_sacks,0) * 1) +
      (coalesce(def_interceptions,0) * 2) +
      (coalesce(def_fumbles_forced,0) * 1) +
      (coalesce(fumble_recovery_opp,0) * 2) +
      ((coalesce(def_tds,0) + coalesce(special_teams_tds,0)) * 6) +
      (coalesce(def_safeties,0) * 2) +
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
    fantasy_points_ppr
  )

# =====================
# EXPORT JSON
# =====================
final_players <- bind_rows(
  player_weeks %>%
    transmute(
      season,
      week,
      player_id,
      player_name,
      position,
      team,
      opponent_team,
      headshot_url,
      fantasy_points_ppr,

      completions,
      attempts,
      passing_yards,
      passing_tds,
      passing_interceptions,

      carries,
      rushing_yards,
      rushing_tds,

      targets,
      receptions,
      receiving_yards,
      receiving_tds,

      fumbles_lost,

      fg_att,
      fg_made_0_19,
      fg_made_20_29,
      fg_made_30_39,
      fg_made_40_49,
      fg_made_50_59,
      fg_made_60_,
      pat_att,
      pat_made
    ),
  team_def
)

if (!dir.exists("data")) dir.create("data")

write_json(
  final_players,
  out_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

message("✅ Success! JSON exported → ", out_path)
