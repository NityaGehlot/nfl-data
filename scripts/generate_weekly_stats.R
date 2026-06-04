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

if (!"display_name" %in% names(players)) players$display_name <- ""
if (!"position"     %in% names(players)) players$position     <- ""
if (!"headshot_url" %in% names(players)) players$headshot_url <- ""

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

safe_col <- function(df, col) {
  if (!col %in% names(df)) df[[col]] <- ""
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
fantasy_positions <- c("QB", "RB", "WR", "TE", "K")

players <- players %>% filter(position %in% fantasy_positions)
weekly  <- weekly  %>% filter(position %in% fantasy_positions)

# =====================
# ENSURE STAT COLUMNS EXIST
# =====================
stat_cols <- c(
  "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
  "carries", "rushing_yards", "rushing_tds",
  "targets", "receptions", "receiving_yards", "receiving_tds",
  "fumbles",
  "fg_made", "fg_att", "fg_missed", "fg_pct",
  "fg_made_0_19", "fg_made_20_29", "fg_made_30_39", "fg_made_40_49", "fg_made_50_59", "fg_made_60_",
  "pat_made", "pat_att", "pat_missed", "pat_pct"
)

for (col in stat_cols) {
  if (!col %in% names(weekly)) weekly[[col]] <- 0
}

# =====================
# KICKER SCORING
# =====================
weekly <- weekly %>%
  mutate(
    fantasy_points_ppr = ifelse(
      position == "K",
      (fg_made_0_19  * 3) +
      (fg_made_20_29 * 3) +
      (fg_made_30_39 * 3) +
      (fg_made_40_49 * 4) +
      (fg_made_50_59 * 5) +
      (fg_made_60_   * 5) +
      (pat_made      * 1) -
      (fg_missed     * 1) -
      (pat_missed    * 1),
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
  left_join(
    weekly %>% select(-any_of(c("player_name", "position", "headshot_url"))),
    by = c("player_id", "week")
  )

# =====================
# JOIN INJURY DATA
# =====================
weekly_full <- weekly_full %>%
  left_join(injuries, by = c("player_id", "week"))

# =====================
# FILL PLAYER METADATA
# =====================
weekly_full <- weekly_full %>%
  group_by(player_id) %>%
  mutate(
    team = if (all(is.na(team))) NA_character_ else last(na.omit(team))
  ) %>%
  ungroup()

# =====================
# CLEAN VALUES
# =====================
weekly_full <- weekly_full %>%
  mutate(across(all_of(stat_cols), ~ coalesce(.x, 0))) %>%
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
# BASE + POSITION-SPECIFIC COLUMNS
# =====================
base_cols <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "headshot_url", "fantasy_points_ppr",
  "injury_status", "practice_status",
  "primary_injury", "secondary_injury",
  "practice_primary_injury", "practice_secondary_injury"
)

position_stat_cols <- list(
  QB = c("completions", "attempts", "passing_yards", "passing_tds",
         "passing_interceptions", "carries", "rushing_yards", "rushing_tds", "fumbles"),
  RB = c("carries", "rushing_yards", "rushing_tds",
         "receptions", "targets", "receiving_yards", "receiving_tds", "fumbles"),
  WR = c("receptions", "targets", "receiving_yards", "receiving_tds",
         "carries", "rushing_yards", "rushing_tds", "fumbles"),
  TE = c("receptions", "targets", "receiving_yards", "receiving_tds",
         "carries", "rushing_yards", "rushing_tds", "fumbles"),
  K  = c("fg_made", "fg_att", "fg_missed", "fg_pct",
         "fg_made_0_19", "fg_made_20_29", "fg_made_30_39", "fg_made_40_49",
         "fg_made_50_59", "fg_made_60_",
         "pat_made", "pat_att", "pat_missed", "pat_pct")
)

# Build one clean data frame per position, then combine
player_list_df <- bind_rows(lapply(names(position_stat_cols), function(pos) {
  pos_df <- weekly_full %>%
    filter(position == pos) %>%
    select(any_of(c(base_cols, position_stat_cols[[pos]])))
  pos_df
}))

# =====================
# DEFENSE SCORING + OPPONENT YARDS
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
    def_fumbles_forced,
    def_sacks,
    def_interceptions,
    def_tds,
    def_safeties,
    fumble_recovery_opp
  ) %>%
  left_join(def_teams,       by = c("season", "week", "team")) %>%
  left_join(opponent_stats,  by = c("season", "week", "opponent_team")) %>%
  mutate(
    fantasy_points_ppr =
      (def_sacks          * 1) +
      (def_interceptions  * 2) +
      (def_fumbles_forced * 1) +
      (fumble_recovery_opp * 2) +
      (def_tds            * 6) +
      (def_safeties       * 2)
  ) %>%
  transmute(
    season,
    week,
    player_id                 = paste0("DEF_", team),
    player_name               = paste(team, "DEF"),
    position                  = "DEF",
    team,
    opponent_team,
    fantasy_points_ppr,
    def_fumbles_forced,
    def_sacks,
    def_interceptions,
    def_tds,
    def_safeties,
    fumble_recovery_opp,
    passing_yards_allowed,
    passing_tds_allowed,
    rushing_yards_allowed,
    rushing_tds_allowed,
    headshot_url              = "",
    injury_status             = "N/A",
    practice_status           = "",
    primary_injury            = "",
    secondary_injury          = "",
    practice_primary_injury   = "",
    practice_secondary_injury = ""
  )

# =====================
# COMBINE PLAYERS + DEFENSE
# =====================
combined_df <- bind_rows(player_list_df, team_def)

# =====================
# REMOVE IRRELEVANT FIELDS
# =====================
clean_position_row <- function(row){

  pos <- row$position

  base <- c(
    "season","week","player_id","player_name",
    "position","team","opponent_team",
    "headshot_url","fantasy_points_ppr",
    "injury_status","practice_status",
    "primary_injury","secondary_injury",
    "practice_primary_injury","practice_secondary_injury"
  )

  keep <- switch(
    pos,

    "QB" = c(
      base,
      "completions","attempts","passing_yards",
      "passing_tds","passing_interceptions",
      "carries","rushing_yards","rushing_tds",
      "fumbles"
    ),

    "RB" = c(
      base,
      "carries","rushing_yards","rushing_tds",
      "targets","receptions",
      "receiving_yards","receiving_tds",
      "fumbles"
    ),

    "WR" = c(
      base,
      "targets","receptions",
      "receiving_yards","receiving_tds",
      "carries","rushing_yards","rushing_tds",
      "fumbles"
    ),

    "TE" = c(
      base,
      "targets","receptions",
      "receiving_yards","receiving_tds",
      "carries","rushing_yards","rushing_tds",
      "fumbles"
    ),

    "K" = c(
      base,
      "fg_made","fg_att","fg_missed","fg_pct",
      "fg_made_0_19","fg_made_20_29",
      "fg_made_30_39","fg_made_40_49",
      "fg_made_50_59","fg_made_60_",
      "pat_made","pat_att","pat_missed","pat_pct"
    ),

    "DEF" = c(
      "season","week","player_id","player_name",
      "position","team","opponent_team",
      "fantasy_points_ppr",

      "def_fumbles_forced",
      "def_sacks",
      "def_interceptions",
      "def_tds",
      "def_safeties",
      "fumble_recovery_opp",

      "passing_yards_allowed",
      "passing_tds_allowed",
      "rushing_yards_allowed",
      "rushing_tds_allowed",

      "injury_status",
      "practice_status",
      "primary_injury",
      "secondary_injury",
      "practice_primary_injury",
      "practice_secondary_injury"
    ),

    names(row)
  )

  row[intersect(names(row), keep)]
}

# =====================
# EXPORT BY WEEK
# =====================
all_players <- c(player_list, def_list)
combined_df <- bind_rows(lapply(all_players, as.data.frame))

if(!dir.exists("data")) {
  dir.create("data")
}

stats_dir <- file.path("data", "2025 stats")

if(!dir.exists(stats_dir)) {
  dir.create(stats_dir)
}

weeks <- sort(unique(combined_df$week))

for(w in weeks){

  week_data <- combined_df %>%
    filter(week == w)

  week_num <- as.integer(trimws(as.character(w)))

  file_name <- file.path(
  stats_dir,
  paste0(
    "player_stats_",
    season,
    "_week",
    sprintf("%02d", week_num),
    ".json"
  )
)

  json_rows <- lapply(
    split(week_data, seq_len(nrow(week_data))),
    clean_position_row
  )

  write_json(
    json_rows,
    file_name,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Exported: ", file_name)
}

message("✅ Weekly JSON files generated successfully.")
