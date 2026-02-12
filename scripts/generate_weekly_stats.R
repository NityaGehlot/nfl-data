# scripts/generate_weekly_stats.R
# Generate official NFL weekly stats JSON using nflreadr
# Includes Sleeper-accurate K + DEF fantasy scoring
# Includes player injury status (report + practice)
# ✅ Includes ALL players every week
# ✅ Includes injury status
# ✅ Sleeper-accurate K + DEF fantasy scoring

library(nflreadr)
library(dplyr)
@@ -14,84 +15,127 @@ current_year <- as.numeric(format(Sys.Date(), "%Y"))
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
message("Loading official weekly PLAYER stats for season: ", season)
weekly <- nflreadr::load_player_stats(seasons = season)
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
message("Loading weekly injury reports (if available)")
message("Loading injury reports")

injuries <- tryCatch(
  {
    nflreadr::load_injuries(seasons = season) %>%
      filter(!is.na(week)) %>%
      transmute(
        season,
        week,
        team,
        position,
        report_status,
        practice_status,
        join_name = tolower(gsub("[^a-z]", "", full_name))
      )
  },
  nflreadr::load_injuries(seasons = season) %>%
    filter(!is.na(week)) %>%
    select(
      gsis_id,
      week,
      report_status,
      practice_status
    ),
  error = function(e) {
    message("⚠️ Injury data not available yet — continuing without injuries")
    tibble(
      season = integer(),
      gsis_id = character(),
      week = integer(),
      team = character(),
      position = character(),
      report_status = character(),
      practice_status = character(),
      join_name = character()
      practice_status = character()
    )
  }
)


# Normalize names in weekly stats and JOIN injuries
weekly <- weekly %>%
  mutate(
    join_name = tolower(gsub("[^a-z]", "", player_name))
  ) %>%
  left_join(
    injuries,
    by = c("season", "week", "team", "position", "join_name")
  ) %>%
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
weekly <- weekly %>%
player_weeks <- player_weeks %>%
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
      (fg_made_0_19 * 3) +
      (fg_made_20_29 * 3) +
      (fg_made_30_39 * 3) +
      (fg_made_40_49 * 4) +
      (fg_made_50_59 * 5) +
      (fg_made_60_ * 5) +
      (pat_made * 1) -
      (fg_missed * 1) -
      (pat_missed * 1),
@@ -100,79 +144,12 @@ weekly <- weekly %>%
  )

# =====================
# PLAYER COLUMN SELECTION
# DEF POINTS ALLOWED (SCHEDULES)
# =====================
desired_cols <- c(
  "season","week","player_id","player_name","position","team","opponent_team",
  "headshot_url","fantasy_points_ppr",

  # Injury info
  "report_status","practice_status",

  # Passing
  "completions","attempts","passing_yards","passing_tds","passing_interceptions",

  # Rushing / Receiving
  "carries","rushing_yards","rushing_tds",
  "targets","receptions","receiving_yards","receiving_tds",
  "fumbles",

  # Kicking
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
  "headshot_url","fantasy_points_ppr",
  "report_status","practice_status"
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
message("Loading DEF points allowed")

schedules <- nflreadr::load_schedules(seasons = season) %>%
  filter(
    game_type == "REG",
    !is.na(home_score),
    !is.na(away_score)
  )
  filter(game_type == "REG", !is.na(home_score))

home_def <- schedules %>%
  transmute(season, week, team = home_team, points_allowed = away_score)
@@ -182,11 +159,6 @@ away_def <- schedules %>%

def_points_allowed <- bind_rows(home_def, away_def)

# =====================
# LOAD TEAM DEF STATS
# =====================
message("Loading official weekly TEAM DEF stats")

team_weekly <- nflreadr::load_team_stats(seasons = season)

team_def <- team_weekly %>%
@@ -228,17 +200,53 @@ team_def <- team_weekly %>%
    fantasy_points_ppr
  )

def_list <- apply(as.data.frame(team_def), 1, function(row) as.list(row))

# =====================
# EXPORT JSON
# =====================
all_players <- c(player_list, def_list)
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
  all_players,
  final_players,
  out_path,
  pretty = TRUE,
  auto_unbox = TRUE,
