# scripts/generate_weekly_stats.R
library(nflreadr)
library(dplyr)
library(jsonlite)

# =====================
# CONFIG
# =====================
current_year <- as.numeric(format(Sys.Date(), "%Y"))
latest_season <- nflreadr::most_recent_season()
season <- min(current_year, latest_season)

message("Generating stats for season: ", season)

# =====================
# LOAD PLAYER DATA
# =====================
message("Loading weekly player stats")
weekly <- nflreadr::load_player_stats(seasons = season)

message("Loading player metadata")
players <- nflreadr::load_players()

if(!"display_name" %in% names(players)) players$display_name <- ""
if(!"position"     %in% names(players)) players$position     <- ""
if(!"headshot_url" %in% names(players)) players$headshot_url <- ""

players <- players %>%
  transmute(
    player_id    = gsis_id,
    player_name  = display_name,
    position,
    headshot_url
  )

# =====================
# LOAD INJURY DATA
# =====================
message("Loading injury data")

injuries <- nflreadr::load_injuries(seasons = season)

safe_col <- function(df, col){
  if(!col %in% names(df)) df[[col]] <- ""
  df
}

injuries <- safe_col(injuries, "gsis_id")
injuries <- safe_col(injuries, "week")
injuries <- safe_col(injuries, "report_status")
injuries <- safe_col(injuries, "practice_status")
injuries <- safe_col(injuries, "report_primary_injury")
injuries <- safe_col(injuries, "report_secondary_injury")
injuries <- safe_col(injuries, "practice_primary_injury")
injuries <- safe_col(injuries, "practice_secondary_injury")

injuries <- injuries %>%
  transmute(
    player_id               = gsis_id,
    week,
    injury_status           = report_status,
    practice_status,
    primary_injury          = report_primary_injury,
    secondary_injury        = report_secondary_injury,
    practice_primary_injury,
    practice_secondary_injury
  )

# =====================
# KEEP ONLY FANTASY POSITIONS
# =====================
fantasy_positions <- c("QB","RB","WR","TE","K")

players <- players %>% filter(position %in% fantasy_positions)
weekly  <- weekly  %>% filter(position %in% fantasy_positions)

# =====================
# ENSURE STAT COLUMNS EXIST
# =====================
stat_cols <- c(
  # Passing
  "completions","attempts","passing_yards","passing_tds","passing_interceptions",
  # Rushing
  "carries","rushing_yards","rushing_tds",
  # Receiving
  "targets","receptions","receiving_yards","receiving_tds",
  # Misc
  "fumbles",
  # Kicker — corrected API names
  "fg_made","fg_att","fg_missed","fg_pct",
  "fg_made_0_19","fg_made_20_29","fg_made_30_39",
  "fg_made_40_49","fg_made_50_59","fg_made_60_",
  "fg_missed_0_19","fg_missed_20_29","fg_missed_30_39",
  "fg_missed_40_49","fg_missed_50_59","fg_missed_60_",
  "fg_made_list","fg_missed_list",
  "pat_made","pat_att","pat_missed","pat_pct"
)

for(col in stat_cols){
  if(!col %in% names(weekly)) weekly[[col]] <- 0
}

# =====================
# KICKER SCORING (corrected column names)
# =====================
weekly <- weekly %>%
  mutate(
    fantasy_points_ppr = ifelse(
      position == "K",
      (coalesce(fg_made_0_19,  0) * 3) +
      (coalesce(fg_made_20_29, 0) * 3) +
      (coalesce(fg_made_30_39, 0) * 3) +
      (coalesce(fg_made_40_49, 0) * 4) +
      (coalesce(fg_made_50_59, 0) * 5) +
      (coalesce(fg_made_60_,   0) * 5) +
      (coalesce(pat_made,      0) * 1) -
      (coalesce(fg_missed,     0) * 1) -
      (coalesce(pat_missed,    0) * 1),
      fantasy_points_ppr
    )
  )

# =====================
# CREATE PLAYER x WEEK GRID
# =====================
all_weeks <- sort(unique(weekly$week))

full_grid <- expand.grid(
  player_id = players$player_id,
  week      = all_weeks,
  stringsAsFactors = FALSE
)

weekly_full <- full_grid %>%
  left_join(players, by = "player_id") %>%
  left_join(weekly,  by = c("player_id","week"))

# =====================
# JOIN INJURY DATA
# =====================
weekly_full <- weekly_full %>%
  left_join(injuries, by = c("player_id","week"))

# =====================
# FILL PLAYER METADATA
# =====================
weekly_full <- weekly_full %>%
  group_by(player_id) %>%
  mutate(
    player_name = coalesce(player_name.x, player_name.y),
    position    = coalesce(position.x,    position.y),
    team        = if(all(is.na(team))) NA_character_ else last(na.omit(team))
  ) %>%
  ungroup()

# =====================
# CLEAN VALUES
# =====================
weekly_full <- weekly_full %>%
  mutate(across(all_of(stat_cols), ~coalesce(.x, 0))) %>%
  mutate(
    fantasy_points_ppr        = coalesce(fantasy_points_ppr, 0),
    opponent_team             = coalesce(opponent_team, ""),
    injury_status             = coalesce(injury_status, "ACTIVE"),
    practice_status           = coalesce(practice_status, ""),
    primary_injury            = coalesce(primary_injury, ""),
    secondary_injury          = coalesce(secondary_injury, ""),
    practice_primary_injury   = coalesce(practice_primary_injury, ""),
    practice_secondary_injury = coalesce(practice_secondary_injury, "")
  )

# =====================
# BASE PLAYER COLUMNS
# =====================
base_cols <- c(
  "season","week","player_id","player_name",
  "position","team","opponent_team",
  "headshot_url","fantasy_points_ppr",
  "injury_status","practice_status",
  "primary_injury","secondary_injury",
  "practice_primary_injury","practice_secondary_injury"
)

weekly_df <- weekly_full %>%
  select(any_of(c(base_cols, stat_cols)))

# =====================
# POSITION FILTERING
# Each position only keeps its relevant stat columns
# =====================
position_cols <- list(
  QB = c(
    "completions","attempts","passing_yards","passing_tds",
    "passing_interceptions","carries","rushing_yards","rushing_tds","fumbles"
  ),
  RB = c(
    "carries","rushing_yards","rushing_tds",
    "receptions","targets","receiving_yards","receiving_tds","fumbles"
  ),
  WR = c(
    "receptions","targets","receiving_yards","receiving_tds",
    "carries","rushing_yards","rushing_tds","fumbles"
  ),
  TE = c(
    "receptions","targets","receiving_yards","receiving_tds",
    "carries","rushing_yards","rushing_tds","fumbles"
  ),
  # Kickers only — corrected API names, no player/def nulls
  K = c(
    "fg_made","fg_att","fg_missed","fg_pct",
    "fg_made_0_19","fg_made_20_29","fg_made_30_39",
    "fg_made_40_49","fg_made_50_59","fg_made_60_",
    "fg_missed_0_19","fg_missed_20_29","fg_missed_30_39",
    "fg_missed_40_49","fg_missed_50_59","fg_missed_60_",
    "fg_made_list","fg_missed_list",
    "pat_made","pat_att","pat_missed","pat_pct"
  )
)

player_list <- apply(weekly_df, 1, function(row) {
  pos <- row[["position"]]
  if(!(pos %in% names(position_cols))) return(NULL)
  keep_cols <- intersect(c(base_cols, position_cols[[pos]]), names(row))
  as.list(row[keep_cols])
})

player_list <- Filter(Negate(is.null), player_list)

# =====================
# DEFENSE SCORING + YARDS ALLOWED
# =====================
message("Loading schedules")
schedules <- nflreadr::load_schedules(seasons = season) %>%
  filter(game_type == "REG")

home_def <- schedules %>%
  transmute(season, week, team = home_team, opponent_team = away_team)

away_def <- schedules %>%
  transmute(season, week, team = away_team, opponent_team = home_team)

def_teams <- bind_rows(home_def, away_def)

message("Loading team stats")
team_weekly <- nflreadr::load_team_stats(seasons = season)

# Safe fallback for any missing defensive columns
safe_def_col <- function(df, col){
  if(!col %in% names(df)){
    message("⚠️ DEF column not found, defaulting to 0: ", col)
    df[[col]] <- 0
  }
  df
}

team_weekly <- safe_def_col(team_weekly, "def_sacks")
team_weekly <- safe_def_col(team_weekly, "def_interceptions")
team_weekly <- safe_def_col(team_weekly, "def_fumbles_forced")
team_weekly <- safe_def_col(team_weekly, "fumble_recovery_opp")
team_weekly <- safe_def_col(team_weekly, "def_tds")
team_weekly <- safe_def_col(team_weekly, "def_safeties")
team_weekly <- safe_def_col(team_weekly, "passing_yards")
team_weekly <- safe_def_col(team_weekly, "passing_tds")
team_weekly <- safe_def_col(team_weekly, "rushing_yards")
team_weekly <- safe_def_col(team_weekly, "rushing_tds")

# Opponent offensive stats (used as yards/tds allowed)
opponent_stats <- team_weekly %>%
  select(season, week, team, passing_yards, passing_tds, rushing_yards, rushing_tds) %>%
  rename(
    opponent_team         = team,
    passing_yards_allowed = passing_yards,
    passing_tds_allowed   = passing_tds,
    rushing_yards_allowed = rushing_yards,
    rushing_tds_allowed   = rushing_tds
  )

team_def <- team_weekly %>%
  select(
    season, week, team,
    def_fumbles_forced, def_sacks, def_interceptions,
    def_tds, def_safeties, fumble_recovery_opp
  ) %>%
  left_join(def_teams,       by = c("season","week","team")) %>%
  left_join(opponent_stats,  by = c("season","week","opponent_team")) %>%
  mutate(
    fantasy_points_ppr =
      (coalesce(def_sacks,          0) * 1) +
      (coalesce(def_interceptions,  0) * 2) +
      (coalesce(def_fumbles_forced, 0) * 1) +
      (coalesce(fumble_recovery_opp,0) * 2) +
      (coalesce(def_tds,            0) * 6) +
      (coalesce(def_safeties,       0) * 2)
  ) %>%
  transmute(
    season,
    week,
    player_id             = paste0("DEF_", team),
    player_name           = paste(team, "DEF"),
    position              = "DEF",
    team,
    opponent_team,
    fantasy_points_ppr,
    def_sacks,
    def_interceptions,
    def_fumbles_forced,
    fumble_recovery_opp,
    def_tds,
    def_safeties,
    passing_yards_allowed,
    passing_tds_allowed,
    rushing_yards_allowed,
    rushing_tds_allowed,
    injury_status             = "N/A",
    practice_status           = "",
    primary_injury            = "",
    secondary_injury          = "",
    practice_primary_injury   = "",
    practice_secondary_injury = ""
  )

def_list <- apply(as.data.frame(team_def), 1, function(row) as.list(row))

# =====================
# EXPORT BY WEEK
# =====================
player_df <- bind_rows(lapply(player_list, as.data.frame))
def_df    <- as.data.frame(team_def)

if(!dir.exists("data")) dir.create("data")

weeks <- sort(unique(player_df$week))

for(w in weeks){

  player_week  <- player_df %>% filter(week == w)
  def_week     <- def_df    %>% filter(week == w)

  # bind_rows handles mismatched columns cleanly —
  # player rows won't have def columns and vice versa,
  # missing values will be NA which write_json exports as null
  combined_week <- bind_rows(player_week, def_week)

  file_name <- paste0(
    "data/player_stats_",
    season,
    "_week",
    w,
    ".json"
  )

  write_json(
    split(combined_week, seq(nrow(combined_week))),
    file_name,
    pretty     = TRUE,
    auto_unbox = TRUE,
    na         = "null"
  )

  message("Exported: ", file_name)
}

message("✅ Weekly JSON files generated successfully.")
