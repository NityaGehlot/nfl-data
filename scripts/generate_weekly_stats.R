# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# ✅ Includes ALL players every week
# ✅ Includes injury status
# ✅ Sleeper-accurate K + DEF fantasy scoring

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
# LOAD BASE PLAYER UNIVERSE
# =====================
message("Loading NFL rosters")

rosters <- nflreadr::load_rosters(seasons = season) %>%
  filter(position %in% c("QB","RB","WR","TE","K")) %>%
  select(
    gsis_id,
    player_name = full_name,
    team,
    position,
    headshot_url
  )

player_weeks <- expand.grid(
  gsis_id = rosters$gsis_id,
  week = weeks
) %>%
  left_join(rosters, by = "gsis_id") %>%
  mutate(season = season)

# =====================
# LOAD PLAYER STATS
# =====================
message("Loading weekly player stats")

weekly_stats <- nflreadr::load_player_stats(seasons = season)

player_weeks <- player_weeks %>%
  left_join(
    weekly_stats,
    by = c(
      "gsis_id" = "player_id",
      "season",
      "week"
    )
  )

# =====================
# LOAD INJURY DATA (SAFE)
# =====================
message("Loading injury reports")

injuries <- tryCatch(
  nflreadr::load_injuries(seasons = season) %>%
    filter(!is.na(week)) %>%
    select(
      gsis_id,
      week,
      report_status,
      practice_status
    ),
  error = function(e) {
    tibble(
      gsis_id = character(),
      week = integer(),
      report_status = character(),
      practice_status = character()
    )
  }
)

player_weeks <- player_weeks %>%
  left_join(injuries, by = c("gsis_id", "week")) %>%
  mutate(
    report_status = ifelse(is.na(report_status), "Healthy", report_status),
    practice_status = ifelse(is.na(practice_status), "Full", practice_status)
  )

# =====================
# NORMALIZE STATS (CRITICAL)
# =====================
player_weeks <- player_weeks %>%
  mutate(
    fantasy_points_ppr = coalesce(fantasy_points_ppr, 0),
    completions = coalesce(completions, 0),
    attempts = coalesce(attempts, 0),
    passing_yards = coalesce(passing_yards, 0),
    passing_tds = coalesce(passing_tds, 0),
    passing_interceptions = coalesce(passing_interceptions, 0),
    carries = coalesce(carries, 0),
    rushing_yards = coalesce(rushing_yards, 0),
    rushing_tds = coalesce(rushing_tds, 0),
    targets = coalesce(targets, 0),
    receptions = coalesce(receptions, 0),
    receiving_yards = coalesce(receiving_yards, 0),
    receiving_tds = coalesce(receiving_tds, 0),
    fumbles = coalesce(fumbles, 0),

    fg_made_0_19 = coalesce(fg_made_0_19, 0),
    fg_made_20_29 = coalesce(fg_made_20_29, 0),
    fg_made_30_39 = coalesce(fg_made_30_39, 0),
    fg_made_40_49 = coalesce(fg_made_40_49, 0),
    fg_made_50_59 = coalesce(fg_made_50_59, 0),
    fg_made_60_ = coalesce(fg_made_60_, 0),
    fg_att = coalesce(fg_att, 0),
    fg_missed = coalesce(fg_missed, 0),
    pat_made = coalesce(pat_made, 0),
    pat_att = coalesce(pat_att, 0),
    pat_missed = coalesce(pat_missed, 0)
  )

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
# DEF POINTS ALLOWED (SCHEDULES)
# =====================
message("Loading DEF points allowed")

schedules <- nflreadr::load_schedules(seasons = season) %>%
  filter(game_type == "REG", !is.na(home_score))

home_def <- schedules %>%
  transmute(season, week, team = home_team, points_allowed = away_score)

away_def <- schedules %>%
  transmute(season, week, team = away_team, points_allowed = home_score)

def_points_allowed <- bind_rows(home_def, away_def)

team_weekly <- nflreadr::load_team_stats(seasons = season)

team_def <- team_weekly %>%
  filter(!is.na(week)) %>%
  left_join(def_points_allowed, by = c("season","week","team")) %>%
  mutate(
    fantasy_points_ppr =
      (def_sacks * 1) +
      (def_interceptions * 2) +
      (def_fumbles_forced * 1) +
      (fumble_recovery_opp * 2) +
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
    fumbles_forced = def_fumbles_forced,
    fumbles_recovered = fumble_recovery_opp,
    defensive_tds = def_tds + special_teams_tds,
    safeties = def_safeties,
    points_allowed,
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
      player_id = gsis_id,
      player_name,
      position,
      team,
      opponent_team,
      headshot_url,
      fantasy_points_ppr,
      report_status,
      practice_status,
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
      fumbles,
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
