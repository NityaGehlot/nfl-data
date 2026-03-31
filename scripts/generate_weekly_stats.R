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

# Ensure columns exist
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
# LOAD INJURY DATA
# =====================
message("Loading injury data")

injuries <- nflreadr::load_injuries(seasons = season)

# Ensure columns exist safely
if(!"gsis_id" %in% names(injuries)) injuries$gsis_id <- ""
if(!"week" %in% names(injuries)) injuries$week <- NA
if(!"report_status" %in% names(injuries)) injuries$report_status <- ""
if(!"practice_status" %in% names(injuries)) injuries$practice_status <- ""
if(!"injury_type" %in% names(injuries)) injuries$injury_type <- ""

injuries <- injuries %>%
  transmute(
    player_id = gsis_id,
    week,
    primary_injury = report_primary_injury,
    secondary_injury = report_secondary_injury,
    # injury_status = report_status,
    # practice_primary_injury = practice_primary_injury,
    # practice_secondary_injury = practice_secondary_injury,
    # practice_status = practice_status,
    
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
  "fg_made","fg_att","fg_missed",
  "fg_0_19","fg_20_29","fg_30_39","fg_40_49","fg_50_59","fg_60p",
  "pat_made","pat_att","pat_missed"
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
# CREATE PLAYER x WEEK GRID
# =====================
all_weeks <- sort(unique(weekly$week))

full_grid <- expand.grid(
  player_id = players$player_id,
  week = all_weeks,
  stringsAsFactors = FALSE
)

weekly_full <- full_grid %>%
  left_join(players, by="player_id") %>%
  left_join(weekly, by=c("player_id","week"))

# =====================
# JOIN INJURY DATA
# =====================
weekly_full <- weekly_full %>%
  left_join(injuries, by=c("player_id","week"))

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

# =====================
# CLEAN + FILL VALUES
# =====================
weekly_full <- weekly_full %>%
  mutate(across(all_of(stat_cols), ~coalesce(.x,0))) %>%
  mutate(
    fantasy_points_ppr = coalesce(fantasy_points_ppr,0),
    opponent_team = coalesce(opponent_team,""),
    injury_status = coalesce(injury_status,"ACTIVE"),
    practice_status = coalesce(practice_status,""),
    injury_type = coalesce(injury_type,"")
  )

# =====================
# BASE PLAYER COLUMNS
# =====================
base_cols <- c(
  "season","week","player_id","player_name",
  "position","team","opponent_team",
  "headshot_url","fantasy_points_ppr",
  "injury_status","practice_status","injury_type"
)

weekly_df <- weekly_full %>%
  select(any_of(c(base_cols, stat_cols)))

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
  K = c("fg_made","fg_att","fg_missed",
        "fg_0_19","fg_20_29","fg_30_39","fg_40_49","fg_50_59","fg_60p",
        "pat_made","pat_att","pat_missed")
)

player_list <- apply(weekly_df, 1, function(row) {

  pos <- row[["position"]]

  if(!(pos %in% names(position_cols))) return(NULL)

  keep_cols <- intersect(
    c(base_cols, position_cols[[pos]]),
    names(row)
  )

  as.list(row[keep_cols])
})

player_list <- Filter(Negate(is.null), player_list)

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
    fantasy_points_ppr,
    injury_status="N/A",
    practice_status="",
    injury_type=""
  )

def_list <- apply(as.data.frame(team_def), 1, function(row) as.list(row))

all_players <- c(player_list, def_list)

combined_df <- bind_rows(lapply(all_players, as.data.frame))

# =====================
# EXPORT BY WEEK
# =====================
if(!dir.exists("data")) dir.create("data")

weeks <- sort(unique(combined_df$week))

for(w in weeks){

  week_data <- combined_df %>%
    filter(week == w)

  file_name <- paste0(
    "data/player_stats_",
    season,
    "_week",
    w,
    ".json"
  )

  write_json(
    split(week_data, seq(nrow(week_data))),
    file_name,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Exported: ", file_name)
}

message("✅ Weekly JSON files generated successfully.")
