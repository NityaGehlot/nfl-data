# scripts/generate_weekly_stats.R
library(nflreadr)
library(dplyr)
library(jsonlite)

# =====================
# CONFIG
# =====================
current_year  <- as.numeric(format(Sys.Date(), "%Y"))
season        <- min(current_year, nflreadr::most_recent_season())
message("Generating stats for season: ", season)

# =====================
# HELPERS
# =====================
ensure_cols <- function(df, cols, fill = 0) {
  for (col in cols) if (!col %in% names(df)) df[[col]] <- fill
  df
}

coalesce_cols <- function(df, cols, fill = 0) {
  df %>% mutate(across(all_of(cols), ~ coalesce(.x, fill)))
}

stats_dir <- function(type) {
  path <- file.path("data", "Stats", paste0(season, " Season"), paste0(season, " ", type))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

export_week_json <- function(week_df, dir, season, week_num) {
  file_name <- file.path(dir, sprintf("player_stats_%s_week%02d.json", season, as.integer(week_num)))
  rows      <- lapply(split(week_df, seq_len(nrow(week_df))), trim_to_position)
  write_json(rows, file_name, pretty = TRUE, auto_unbox = TRUE, na = "null")
  message("Exported: ", file_name)
}

normalize_def_position <- function(pos) {
  dplyr::case_when(
    pos %in% c("DE", "DT", "NT", "DL")             ~ "DL",
    pos %in% c("ILB", "OLB", "MLB", "LB", "EDGE")  ~ "LB",
    pos == "CB"                                     ~ "CB",
    pos %in% c("FS", "SS", "DB", "S")              ~ "S",
    TRUE                                            ~ pos
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =====================
# POSITION COLUMN SCHEMAS
# =====================
BASE_COLS <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "headshot_url", "fantasy_points_ppr",
  "snap_count",
  "injury_status", "practice_status",
  "primary_injury", "secondary_injury",
  "practice_primary_injury", "practice_secondary_injury"
)

BASE_COLS_DEF_TEAM <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "fantasy_points_ppr",
  "injury_status", "practice_status",
  "primary_injury", "secondary_injury",
  "practice_primary_injury", "practice_secondary_injury"
)

POSITION_COLS <- list(
  QB  = c("completions", "attempts", "passing_yards", "passing_tds",
          "passing_interceptions", "carries", "rushing_yards", "rushing_tds", "fumbles"),
  RB  = c("carries", "rushing_yards", "rushing_tds",
          "receptions", "targets", "receiving_yards", "receiving_tds", "fumbles"),
  WR  = c("receptions", "targets", "receiving_yards", "receiving_tds",
          "carries", "rushing_yards", "rushing_tds", "fumbles"),
  TE  = c("receptions", "targets", "receiving_yards", "receiving_tds",
          "carries", "rushing_yards", "rushing_tds", "fumbles"),
  K   = c("fg_made", "fg_att", "fg_missed", "fg_pct",
          "fg_made_0_19", "fg_made_20_29", "fg_made_30_39", "fg_made_40_49",
          "fg_made_50_59", "fg_made_60_",
          "pat_made", "pat_att", "pat_missed", "pat_pct"),
  DL  = c("def_tackles_solo", "def_tackles_with_assist", "def_tackles_for_loss",
          "def_tackles_for_loss_yards", "def_sacks", "def_sack_yards",
          "def_qb_hits", "def_fumbles_forced", "def_safeties", "def_tds"),
  LB  = c("def_tackles_solo", "def_tackles_with_assist", "def_tackles_for_loss",
          "def_sacks", "def_qb_hits", "def_interceptions", "def_interception_yards",
          "def_pass_defended", "def_fumbles_forced",
          "fumble_recovery_opp", "fumble_recovery_yards_opp", "def_tds"),
  CB  = c("def_interceptions", "def_interception_yards", "def_pass_defended",
          "def_tackles_solo", "def_tackles_with_assist",
          "def_fumbles_forced", "def_tds"),
  S   = c("def_interceptions", "def_interception_yards", "def_pass_defended",
          "def_tackles_solo", "def_tackles_with_assist", "def_tackles_for_loss",
          "def_fumbles_forced", "fumble_recovery_opp", "fumble_recovery_yards_opp", "def_tds"),
  DEF = c("def_fumbles_forced", "def_sacks", "def_interceptions",
          "def_tds", "def_safeties", "fumble_recovery_opp",
          "passing_yards_allowed", "passing_tds_allowed",
          "rushing_yards_allowed", "rushing_tds_allowed")
)

trim_to_position <- function(row) {
  pos  <- row$position
  keep <- if (pos == "DEF") {
    c(BASE_COLS_DEF_TEAM, POSITION_COLS[["DEF"]])
  } else {
    c(BASE_COLS, POSITION_COLS[[pos]] %||% character(0))
  }
  row[intersect(names(row), keep)]
}

# =====================
# LOAD & PREP SHARED DATA
# =====================
message("Loading weekly player stats")
weekly <- nflreadr::load_player_stats(seasons = season)

message("Loading player metadata")
players <- nflreadr::load_players() %>%
  ensure_cols(c("display_name", "position", "headshot_url"), fill = "") %>%
  transmute(player_id = gsis_id, player_name = display_name, position, headshot_url)

message("Loading injury data")
injuries <- nflreadr::load_injuries(seasons = season) %>%
  ensure_cols(c("gsis_id", "week", "report_status", "practice_status",
                "report_primary_injury", "report_secondary_injury",
                "practice_primary_injury", "practice_secondary_injury"), fill = "") %>%
  transmute(
    player_id                 = gsis_id, week,
    injury_status             = report_status,
    practice_status,
    primary_injury            = report_primary_injury,
    secondary_injury          = report_secondary_injury,
    practice_primary_injury, practice_secondary_injury
  )

message("Loading snap counts")
snaps_raw <- nflreadr::load_snap_counts(seasons = season)

# Offense snaps — one row per player per game
offense_snaps <- snaps_raw %>%
  select(pfr_player_id, week, offense_snaps) %>%
  rename(snap_count = offense_snaps) %>%
  filter(!is.na(pfr_player_id))

# Defense snaps — one row per player per game
defense_snaps <- snaps_raw %>%
  select(pfr_player_id, week, defense_snaps) %>%
  rename(snap_count = defense_snaps) %>%
  filter(!is.na(pfr_player_id))

# We need a pfr_player_id -> gsis_id bridge from the players table
# nflreadr::load_players() contains both gsis_id and pfr_id
pfr_bridge <- nflreadr::load_players() %>%
  filter(!is.na(gsis_id), !is.na(pfr_id)) %>%
  select(player_id = gsis_id, pfr_player_id = pfr_id)

offense_snaps <- offense_snaps %>%
  left_join(pfr_bridge, by = "pfr_player_id") %>%
  filter(!is.na(player_id)) %>%
  select(player_id, week, snap_count)

defense_snaps <- defense_snaps %>%
  left_join(pfr_bridge, by = "pfr_player_id") %>%
  filter(!is.na(player_id)) %>%
  select(player_id, week, snap_count)

# =====================
# OFFENSE PIPELINE
# =====================
offense_positions <- c("QB", "RB", "WR", "TE", "K")

off_stat_cols <- c(
  "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
  "carries", "rushing_yards", "rushing_tds",
  "targets", "receptions", "receiving_yards", "receiving_tds", "fumbles",
  "fg_made", "fg_att", "fg_missed", "fg_pct",
  "fg_made_0_19", "fg_made_20_29", "fg_made_30_39", "fg_made_40_49",
  "fg_made_50_59", "fg_made_60_",
  "pat_made", "pat_att", "pat_missed", "pat_pct"
)

players_off <- players %>% filter(position %in% offense_positions)
weekly_off  <- weekly  %>% filter(position %in% offense_positions) %>%
  ensure_cols(off_stat_cols) %>%
  mutate(fantasy_points_ppr = ifelse(
    position == "K",
    (fg_made_0_19 * 3) + (fg_made_20_29 * 3) + (fg_made_30_39 * 3) +
    (fg_made_40_49 * 4) + (fg_made_50_59 * 5) + (fg_made_60_ * 5) +
    (pat_made * 1) - (fg_missed * 1) - (pat_missed * 1),
    fantasy_points_ppr
  ))

all_weeks <- sort(unique(weekly_off$week))

offense_df <- expand.grid(player_id = players_off$player_id, week = all_weeks,
                           stringsAsFactors = FALSE) %>%
  left_join(players_off, by = "player_id") %>%
  left_join(weekly_off %>% select(-any_of(c("player_name", "position", "headshot_url"))),
            by = c("player_id", "week")) %>%
  left_join(injuries, by = c("player_id", "week")) %>%
  left_join(offense_snaps, by = c("player_id", "week")) %>%
  group_by(player_id) %>%
  mutate(team = if (all(is.na(team))) NA_character_ else last(na.omit(team))) %>%
  ungroup() %>%
  coalesce_cols(off_stat_cols) %>%
  mutate(
    fantasy_points_ppr        = coalesce(fantasy_points_ppr, 0),
    snap_count                = coalesce(snap_count, 0L),
    opponent_team             = coalesce(opponent_team, ""),
    injury_status             = coalesce(injury_status, "ACTIVE"),
    practice_status           = coalesce(practice_status, ""),
    primary_injury            = coalesce(primary_injury, ""),
    secondary_injury          = coalesce(secondary_injury, ""),
    practice_primary_injury   = coalesce(practice_primary_injury, ""),
    practice_secondary_injury = coalesce(practice_secondary_injury, "")
  )

offense_combined <- bind_rows(lapply(offense_positions, function(pos) {
  offense_df %>%
    filter(position == pos) %>%
    select(any_of(c(BASE_COLS, POSITION_COLS[[pos]])))
}))

# =====================
# TEAM DEFENSE (DEF) PIPELINE
# =====================
message("Loading schedules")
schedules <- nflreadr::load_schedules(seasons = season) %>% filter(game_type == "REG")

def_teams <- bind_rows(
  schedules %>% transmute(season, week, team = home_team, opponent_team = away_team),
  schedules %>% transmute(season, week, team = away_team, opponent_team = home_team)
)

message("Loading team stats")
team_weekly <- nflreadr::load_team_stats(seasons = season)

opponent_stats <- team_weekly %>%
  select(season, week, team, passing_yards, passing_tds, rushing_yards, rushing_tds) %>%
  rename(opponent_team = team, passing_yards_allowed = passing_yards,
         passing_tds_allowed = passing_tds, rushing_yards_allowed = rushing_yards,
         rushing_tds_allowed = rushing_tds)

team_def <- team_weekly %>%
  select(season, week, team, def_fumbles_forced, def_sacks, def_interceptions,
         def_tds, def_safeties, fumble_recovery_opp) %>%
  left_join(def_teams,      by = c("season", "week", "team")) %>%
  left_join(opponent_stats, by = c("season", "week", "opponent_team")) %>%
  mutate(
    fantasy_points_ppr =
      (def_sacks * 1) + (def_interceptions * 2) + (def_fumbles_forced * 1) +
      (fumble_recovery_opp * 2) + (def_tds * 6) + (def_safeties * 2)
  ) %>%
  transmute(
    season, week,
    player_id    = paste0("DEF_", team),
    player_name  = paste(team, "DEF"),
    position     = "DEF", team, opponent_team,
    fantasy_points_ppr,
    def_fumbles_forced, def_sacks, def_interceptions,
    def_tds, def_safeties, fumble_recovery_opp,
    passing_yards_allowed, passing_tds_allowed,
    rushing_yards_allowed, rushing_tds_allowed,
    injury_status             = "N/A",
    practice_status           = "",
    primary_injury            = "",
    secondary_injury          = "",
    practice_primary_injury   = "",
    practice_secondary_injury = ""
  )

# =====================
# INDIVIDUAL DEFENSIVE PLAYERS PIPELINE
# =====================
message("Loading individual defensive player stats")
def_positions <- c("DL", "LB", "CB", "S")

weekly_def_raw <- nflreadr::load_player_stats(seasons = season, stat_type = "defense")

message("Raw def positions found: ", paste(sort(unique(weekly_def_raw$position)), collapse = ", "))

def_stat_cols <- c(
  "def_tackles_solo", "def_tackles_with_assist", "def_tackles_for_loss",
  "def_tackles_for_loss_yards", "def_sacks", "def_sack_yards",
  "def_qb_hits", "def_interceptions", "def_interception_yards",
  "def_pass_defended", "def_tds", "def_fumbles_forced",
  "def_safeties", "fumble_recovery_opp", "fumble_recovery_yards_opp"
)

players_def <- players %>%
  mutate(position = normalize_def_position(position)) %>%
  filter(position %in% def_positions)

weekly_def <- weekly_def_raw %>%
  mutate(position = normalize_def_position(position)) %>%
  filter(position %in% def_positions) %>%
  ensure_cols(def_stat_cols) %>%
  mutate(fantasy_points_ppr = coalesce(fantasy_points_ppr, 0))

def_weeks <- sort(unique(weekly_def$week))
if (length(def_weeks) == 0) def_weeks <- all_weeks

individual_def_df <- expand.grid(player_id = players_def$player_id, week = def_weeks,
                                  stringsAsFactors = FALSE) %>%
  left_join(players_def, by = "player_id") %>%
  left_join(weekly_def %>% select(-any_of(c("player_name", "position", "headshot_url"))),
            by = c("player_id", "week")) %>%
  left_join(injuries, by = c("player_id", "week")) %>%
  left_join(defense_snaps, by = c("player_id", "week")) %>%
  group_by(player_id) %>%
  mutate(team = if (all(is.na(team))) NA_character_ else last(na.omit(team))) %>%
  ungroup() %>%
  coalesce_cols(def_stat_cols) %>%
  mutate(
    fantasy_points_ppr        = coalesce(fantasy_points_ppr, 0),
    snap_count                = coalesce(snap_count, 0L),
    opponent_team             = coalesce(opponent_team, ""),
    injury_status             = coalesce(injury_status, "ACTIVE"),
    practice_status           = coalesce(practice_status, ""),
    primary_injury            = coalesce(primary_injury, ""),
    secondary_injury          = coalesce(secondary_injury, ""),
    practice_primary_injury   = coalesce(practice_primary_injury, ""),
    practice_secondary_injury = coalesce(practice_secondary_injury, "")
  )

individual_def_combined <- bind_rows(lapply(def_positions, function(pos) {
  individual_def_df %>%
    filter(position == pos) %>%
    select(any_of(c(BASE_COLS, POSITION_COLS[[pos]])))
}))

# Combine individual defensive players + team DEF entries into one defense export
defense_export <- bind_rows(individual_def_combined, team_def)

# =====================
# EXPORT — OFFENSE (QB/RB/WR/TE/K only)
# =====================
off_dir <- stats_dir("Offense")

for (w in sort(unique(offense_combined$week))) {
  file_name <- file.path(off_dir, sprintf("player_stats_%s_week%02d.json", season, as.integer(w)))
  if (file.exists(file_name)) {
    message("Skipping (already exists): ", file_name)
    next
  }
  export_week_json(offense_combined %>% filter(week == w), off_dir, season, w)
}

# =====================
# EXPORT — DEFENSE (DL/LB/CB/S individual players + team DEF)
# =====================
def_dir <- stats_dir("Defense")

for (w in sort(unique(defense_export$week))) {
  file_name <- file.path(def_dir, sprintf("player_stats_%s_week%02d.json", season, as.integer(w)))
  if (file.exists(file_name)) {
    message("Skipping (already exists): ", file_name)
    next
  }
  export_week_json(defense_export %>% filter(week == w), def_dir, season, w)
}

message("✅ All weekly JSON files generated successfully.")
