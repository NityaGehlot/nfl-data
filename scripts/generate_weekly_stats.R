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

output_name <- paste0("player_stats_", season, ".json")
out_path <- file.path("data", output_name)

message("Loading weekly player stats")
weekly <- nflreadr::load_player_stats(seasons = season)

message("Loading player metadata")
players <- nflreadr::load_players()

# Ensure required columns exist
if(!"display_name" %in% names(players)) players$display_name <- ""
if(!"position" %in% names(players)) players$position <- ""
if(!"headshot_url" %in% names(players)) players$headshot_url <- ""

players <- players %>%
  transmute(
    player_id = gsis_id,
    player_name = display_name,
    position,
    headshot_url
  )

# =====================
# KEEP ONLY FANTASY POSITIONS
# =====================
fantasy_positions <- c("QB","RB","WR","TE","K")

players <- players %>%
  filter(position %in% fantasy_positions)

weekly <- weekly %>%
  filter(position %in% fantasy_positions)

# =====================
# ENSURE STAT COLUMNS
# =====================
stat_cols <- c(
  "completions","attempts","passing_yards","passing_tds","passing_interceptions",
  "carries","rushing_yards","rushing_tds",
  "targets","receptions","receiving_yards","receiving_tds",
  "fumbles",
  "fg_made","fg_att","fg_missed","fg_0_19","fg_20_29","fg_30_39",
  "fg_40_49","fg_50_59","fg_60p","pat_made","pat_att","pat_missed"
)

for(col in stat_cols){
  if(!col %in% names(weekly)){
    weekly[[col]] <- 0
  }
}

# =====================
# KICKER SCORING
# =====================
weekly <- weekly %>%
  mutate(
    fantasy_points_ppr = ifelse(
      position == "K",
      (fg_0_19 * 3) +
      (fg_20_29 * 3) +
      (fg_30_39 * 3) +
      (fg_40_49 * 4) +
      (fg_50_59 * 5) +
      (fg_60p * 5) +
      (pat_made * 1) -
      (fg_missed * 1) -
      (pat_missed * 1),
      fantasy_points_ppr
    )
  )

# =====================
# CREATE PLAYER × WEEK GRID
# =====================
all_weeks <- sort(unique(weekly$week))

full_grid <- expand.grid(
  player_id = players$player_id,
  week = all_weeks,
  stringsAsFactors = FALSE
)

# Join metadata
weekly_full <- full_grid %>%
  left_join(players, by = "player_id") %>%
  left_join(weekly, by = c("player_id","week"))

# =====================
# FILL PLAYER METADATA
# =====================
weekly_full <- weekly_full %>%
  group_by(player_id) %>%
  mutate(
    player_name = coalesce(player_name.x, player_name.y),
    position = coalesce(position.x, position.y)
  ) %>%
  ungroup()

# Replace NA stats with 0
weekly_full <- weekly_full %>%
  mutate(across(all_of(stat_cols), ~coalesce(.x,0))) %>%
  mutate(
    fantasy_points_ppr = coalesce(fantasy_points_ppr,0),
    opponent_team = coalesce(opponent_team,"")
  )

# =====================
# SELECT FIELDS
# =====================
base_cols <- c(
  "season","week","player_id","player_name",
  "position","team","opponent_team",
  "headshot_url","fantasy_points_ppr"
)

weekly_list <- weekly_full %>%
  select(any_of(c(base_cols, stat_cols))) %>%
  split(seq(nrow(.))) %>%
  lapply(as.list)

# =====================
# DEFENSE SCORING
# =====================
message("Loading schedules")
schedules <- nflreadr::load_schedules(seasons = season) %>%
  filter(game_type == "REG")

home_def <- schedules %>%
  transmute(season,week,team=home_team,points_allowed=away_score)

away_def <- schedules %>%
  transmute(season,week,team=away_team,points_allowed=home_score)

def_points <- bind_rows(home_def,away_def)

message("Loading team stats")
team_weekly <- nflreadr::load_team_stats(seasons = season)

team_def <- team_weekly %>%
  left_join(def_points,by=c("season","week","team")) %>%
  mutate(
    fantasy_points_ppr =
      (def_sacks*1) +
      (def_interceptions*2) +
      (def_fumbles_forced*1) +
      (fumble_recovery_opp*2) +
      ((def_tds + special_teams_tds)*6) +
      (def_safeties*2) +
      case_when(
        points_allowed==0 ~ 10,
        points_allowed<=6 ~ 7,
        points_allowed<=13 ~ 4,
        points_allowed<=20 ~ 1,
        points_allowed<=27 ~ 0,
        points_allowed<=34 ~ -1,
        TRUE ~ -4
      )
  ) %>%
  transmute(
    season,week,
    player_id=paste0("DEF_",team),
    player_name=paste(team,"DEF"),
    position="DEF",
    team,opponent_team,
    fantasy_points_ppr
  )

def_list <- split(team_def,seq(nrow(team_def))) %>%
  lapply(as.list)

# =====================
# EXPORT
# =====================
all_players <- c(weekly_list, def_list)

if(!dir.exists("data")) dir.create("data")

write_json(
  all_players,
  out_path,
  pretty=TRUE,
  auto_unbox=TRUE,
  na="null"
)

message("✅ Success! JSON exported → ", out_path)
