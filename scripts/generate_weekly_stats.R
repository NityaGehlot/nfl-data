# scripts/generate_weekly_stats.R
library(nflreadr)
library(dplyr)
library(jsonlite)

# =====================
# CONFIG
# =====================
# Season is normally the current/most-recent one, but can be overridden to
# backfill any past season on demand — two ways to do that, either works:
#   1. Command-line arg:   Rscript scripts/generate_weekly_stats.R 2024
#   2. Environment var:    Sys.setenv(SEASON_YEAR = "2024") before sourcing,
#                          or `SEASON_YEAR=2024 Rscript scripts/generate_weekly_stats.R`
# Command-line arg takes priority if both are supplied. Neither supplied ->
# falls back to the original "current year, capped at nflreadr's most recent
# available season" behavior.
args <- commandArgs(trailingOnly = TRUE)

requested_season <- if (length(args) >= 1 && nzchar(args[1])) {
  suppressWarnings(as.numeric(args[1]))
} else if (nzchar(Sys.getenv("SEASON_YEAR"))) {
  suppressWarnings(as.numeric(Sys.getenv("SEASON_YEAR")))
} else {
  NA_real_
}

current_year <- as.numeric(format(Sys.Date(), "%Y"))

season <- if (!is.na(requested_season)) {
  requested_season
} else {
  min(current_year, nflreadr::most_recent_season())
}

# nflreadr/nflfastR play-by-play and weekly stats only go back to 1999;
# also guard against typos producing a season that hasn't happened yet.
if (is.na(season) || season < 1999 || season > current_year) {
  stop(
    "Invalid season requested: ", season,
    " — pass a valid year, e.g. `Rscript scripts/generate_weekly_stats.R 2024` ",
    "or set the SEASON_YEAR environment variable."
  )
}

message("Generating stats for season: ", season)

# =====================
# HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b

ensure_cols <- function(df, cols, fill = 0) {
  for (col in cols) if (!col %in% names(df)) df[[col]] <- fill
  df
}

coalesce_cols <- function(df, cols, fill = 0) {
  df %>% mutate(across(all_of(cols), ~ coalesce(.x, fill)))
}

parse_date_modified <- function(values) {
  values <- trimws(as.character(values))
  values[values == ""] <- NA_character_

  formats <- c(
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y/%m/%d %H:%M:%OS",
    "%m/%d/%Y %I:%M:%OS %p",
    "%m/%d/%Y %H:%M:%OS"
  )

  parse_one <- function(value) {
    if (is.na(value)) {
      return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
    }

    for (fmt in formats) {
      parsed <- suppressWarnings(as.POSIXct(value, format = fmt, tz = "UTC"))
      if (!is.na(parsed)) return(parsed)
    }

    parsed <- suppressWarnings(
      lubridate::parse_date_time(
        value,
        orders = c(
          "Ymd HMS z", "Ymd HMS", "Ymd HM z", "Ymd HM",
          "mdY HMS p", "mdY HM p", "mdY HMS", "mdY HM"
        ),
        tz = "UTC",
        exact = FALSE,
        quiet = TRUE
      )
    )

    if (!is.na(parsed)) return(as.POSIXct(parsed, tz = "UTC"))

    as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  }

  as.POSIXct(unlist(lapply(values, parse_one)), origin = "1970-01-01", tz = "UTC")
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

# Both ESPN's roster feed and nflreadr's own load_players()/load_player_stats()
# inconsistently tag kickers as "K" or "PK" (place kicker). Everything that
# FILTERS on position should treat these as the same player pool; the raw,
# un-normalized tag is preserved separately as "position_on_nflreadr" for K
# rows only (see POSITION_COLS$K) so consumers can see which label was
# actually reported.
normalize_off_position <- function(pos) {
  dplyr::if_else(pos %in% c("K", "PK"), "K", pos)
}

# =====================
# POSITION COLUMN SCHEMAS
# =====================
BASE_COLS <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "headshot_url", "fantasy_points_ppr",
  "snap_count", "game_played", "team_status",
  "injury_status", "practice_status",
  "primary_injury", "secondary_injury",
  "practice_primary_injury", "practice_secondary_injury"
)

BASE_COLS_DEF_TEAM <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "fantasy_points_ppr",
  "team_status",
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
  K   = c("position_on_nflreadr",
          "fg_made", "fg_att", "fg_missed", "fg_pct",
          "fg_made_0_19", "fg_made_20_29", "fg_made_30_39", "fg_made_40_49",
          "fg_made_50_59", "fg_made_60_", "fg_missed_0_19", "fg_missed_20_29",
          "fg_missed_30_39", "fg_missed_40_49", "fg_missed_50_59", "fg_missed_60_",
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
# ESPN POSITION OVERRIDE
# =====================
message("Fetching ESPN team list...")

espn_teams_json <- tryCatch(
  jsonlite::fromJSON(
    "https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams?limit=32",
    simplifyVector = FALSE
  ),
  error = function(e) {
    message("ESPN team list request failed: ", e$message, " — position override disabled")
    NULL
  }
)

espn_team_ids     <- character(0)
espn_team_abbrevs <- character(0)  # named vector: team_id -> abbreviation
if (!is.null(espn_teams_json)) {
  espn_team_ids <- tryCatch(
    sapply(espn_teams_json$sports[[1]]$leagues[[1]]$teams, function(t) t$team$id),
    error = function(e) character(0)
  )
  espn_team_abbrevs <- tryCatch(
    sapply(espn_teams_json$sports[[1]]$leagues[[1]]$teams, function(t) t$team$abbreviation),
    error = function(e) character(0)
  )
  if (length(espn_team_abbrevs) == length(espn_team_ids)) {
    names(espn_team_abbrevs) <- espn_team_ids
  }
}

fetch_espn_roster <- function(team_id, team_abbr = NA_character_) {
  url <- sprintf("https://site.api.espn.com/apis/site/v2/sports/football/nfl/teams/%s/roster", team_id)
  parsed <- tryCatch(
    jsonlite::fromJSON(url, simplifyVector = FALSE),
    error = function(e) {
      message("Failed to fetch ESPN roster for team ", team_id, ": ", e$message)
      NULL
    }
  )
  if (is.null(parsed) || is.null(parsed$athletes)) return(NULL)

  athletes <- unlist(lapply(parsed$athletes, function(group) group$items), recursive = FALSE)

  rows <- lapply(athletes, function(a) {
    tryCatch(
      data.frame(
        espn_id       = as.character(a$id),
        espn_position = a$position$abbreviation %||% NA_character_,
        espn_team     = team_abbr,
        stringsAsFactors = FALSE
      ),
      error = function(e) NULL
    )
  })
  bind_rows(Filter(Negate(is.null), rows))
}

espn_roster_all <- if (length(espn_team_ids) > 0) {
  bind_rows(lapply(espn_team_ids, function(tid) {
    Sys.sleep(0.15)  # polite pacing against ESPN's unofficial API
    fetch_espn_roster(tid, team_abbr = unname(espn_team_abbrevs[as.character(tid)]))
  })) %>% distinct(espn_id, .keep_all = TRUE)
} else {
  message("No ESPN team IDs available — position override disabled")
  tibble(espn_id = character(0), espn_position = character(0), espn_team = character(0))
}

message("ESPN roster players fetched: ", nrow(espn_roster_all))

message("Loading gsis_id <-> espn_id crosswalk")
id_crosswalk <- tryCatch(
  nflreadr::load_ff_playerids() %>%
    filter(!is.na(gsis_id), !is.na(espn_id)) %>%
    transmute(player_id = gsis_id, espn_id = as.character(espn_id)),
  error = function(e) {
    message("Failed to load ff_playerids crosswalk: ", e$message)
    tibble(player_id = character(0), espn_id = character(0))
  }
)

espn_position_lookup <- espn_roster_all %>%
  inner_join(id_crosswalk, by = "espn_id") %>%
  filter(!is.na(espn_position), espn_position != "") %>%
  # NOTE: ESPN's roster API tags kickers as "PK" (place kicker) rather than
  # "K", and this raw tag is intentionally left as-is here — it's kept
  # further down (see `players`) as position_on_nflreadr, and normalized to
  # "K" only for the "position" field everything else filters/groups on.
  distinct(player_id, .keep_all = TRUE) %>%
  select(player_id, espn_position)

message("Players matched to ESPN position: ", nrow(espn_position_lookup))

# Last-resort fallback for team when neither the weekly stat row nor snap
# counts have it for any week (e.g. brand-new rookies, IDs still settling).
espn_team_lookup <- espn_roster_all %>%
  inner_join(id_crosswalk, by = "espn_id") %>%
  filter(!is.na(espn_team), espn_team != "") %>%
  mutate(espn_team = nflreadr::clean_team_abbrs(espn_team)) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  select(player_id, espn_current_team = espn_team)

message("Players matched to ESPN current team: ", nrow(espn_team_lookup))

# =====================
# LOAD & PREP SHARED DATA
# =====================
message("Loading weekly player stats")
weekly <- nflreadr::load_player_stats(seasons = season) %>%
  mutate(position = normalize_off_position(position))

message("Loading player metadata")
players <- nflreadr::load_players() %>%
  ensure_cols(c("display_name", "position", "headshot_url"), fill = "") %>%
  transmute(player_id = gsis_id, player_name = display_name, position, headshot_url) %>%
  left_join(espn_position_lookup, by = "player_id") %>%
  mutate(
    # Same ESPN-priority-then-nflreadr chain used everywhere else, but kept
    # RAW (may be "K" or "PK") before any normalization is applied.
    position_raw = coalesce(espn_position, position),
    # Exposed downstream only for kickers (see POSITION_COLS$K) so the
    # exported JSON shows exactly what nflreadr/ESPN reported.
    position_on_nflreadr = if_else(position_raw %in% c("K", "PK"), position_raw, NA_character_),
    # The "position" field itself always defaults K/PK -> "K" so nothing
    # downstream (filters, grouping, depth caps) has to special-case PK.
    position = normalize_off_position(position_raw)
  ) %>%
  select(-espn_position, -position_raw)

# =====================
# INJURY DATA
# =====================
# Verified live against nflreadr::load_injuries() output (via the underlying
# nflverse-data injuries_<season>.csv release):
#   - gsis_id matches players$gsis_id 1:1 with no format mismatches.
#   - week numbering is CONTINUOUS across game_type (REG 1-18, WC=19, DIV=20,
#     CON=21, SB=22) — same convention used everywhere else in this script —
#     so joining on plain "week" (without game_type) is correct.
#   - report_status/report_primary_injury/etc. are legitimately blank ("")
#     for players who simply weren't given a game-status designation that
#     week (most players, most weeks) — that's not a bug, that's "healthy".
#   - A very small number of players (rare, but real — e.g. a report updated
#     more than once in a week) can have MORE than one row for the same
#     (gsis_id, week). Left un-deduped, that fans out the downstream
#     left_join into duplicate rows. We keep only the most-recently-modified
#     row per player+week so the join is always 1:1.
message("Loading injury data")
injuries_raw <- nflreadr::load_injuries(seasons = season)
message("Injury rows loaded from nflreadr: ", nrow(injuries_raw))

injuries <- injuries_raw %>%
  ensure_cols(c("gsis_id", "week", "report_status", "practice_status",
                "report_primary_injury", "report_secondary_injury",
                "practice_primary_injury", "practice_secondary_injury",
                "date_modified"), fill = "") %>%
  mutate(gsis_id = trimws(as.character(gsis_id))) %>%
  filter(!is.na(gsis_id), gsis_id != "") %>%
  mutate(.date_modified_parsed = parse_date_modified(date_modified)) %>%
  arrange(gsis_id, week, desc(.date_modified_parsed)) %>%
  distinct(gsis_id, week, .keep_all = TRUE) %>%
  select(-.date_modified_parsed) %>%
  transmute(
    player_id                 = gsis_id, week,
    injury_status             = report_status,
    practice_status,
    primary_injury            = report_primary_injury,
    secondary_injury          = report_secondary_injury,
    practice_primary_injury, practice_secondary_injury
  )

message(
  "Injury rows after cleanup/dedup: ", nrow(injuries),
  " (", n_distinct(injuries$player_id), " distinct players) for season ", season
)
non_blank_status <- injuries$injury_status[nzchar(injuries$injury_status)]
message(
  "Players with a real report_status this season: ", length(non_blank_status),
  " — breakdown: ",
  paste(names(table(non_blank_status)), table(non_blank_status), sep = "=", collapse = ", ")
)

message("Loading snap counts")
snaps_raw <- nflreadr::load_snap_counts(seasons = season)

pfr_bridge <- nflreadr::load_players() %>%
  filter(!is.na(gsis_id), !is.na(pfr_id)) %>%
  select(player_id = gsis_id, pfr_player_id = pfr_id)

# Snap counts carry their own team/opponent columns tied directly to game
# participation (this is where "did they actually play" comes from), so we
# capture those here too. They serve as a fallback whenever a player's weekly
# stat row is missing/unmatched but they clearly played (snap_count > 0).
offense_snaps <- snaps_raw %>%
  select(pfr_player_id, week, team, opponent, offense_snaps) %>%
  rename(snap_count = offense_snaps, snap_team = team, snap_opponent = opponent) %>%
  mutate(
    snap_team     = nflreadr::clean_team_abbrs(snap_team),
    snap_opponent = nflreadr::clean_team_abbrs(snap_opponent)
  ) %>%
  filter(!is.na(pfr_player_id)) %>%
  left_join(pfr_bridge, by = "pfr_player_id") %>%
  filter(!is.na(player_id)) %>%
  select(player_id, week, snap_count, snap_team, snap_opponent)

defense_snaps <- snaps_raw %>%
  select(pfr_player_id, week, team, opponent, defense_snaps) %>%
  rename(snap_count = defense_snaps, snap_team = team, snap_opponent = opponent) %>%
  mutate(
    snap_team     = nflreadr::clean_team_abbrs(snap_team),
    snap_opponent = nflreadr::clean_team_abbrs(snap_opponent)
  ) %>%
  filter(!is.na(pfr_player_id)) %>%
  left_join(pfr_bridge, by = "pfr_player_id") %>%
  filter(!is.na(player_id)) %>%
  select(player_id, week, snap_count, snap_team, snap_opponent)

# =====================
# TEAM STATUS LOOKUP (bye-week / played / eliminated)
# =====================
message("Loading schedules for team status")
schedules_all <- nflreadr::load_schedules(seasons = season)

reg_season_weeks  <- 1:18
playoff_weeks     <- 19:22

# All teams that appear in the schedule this season
all_teams <- sort(unique(c(schedules_all$home_team, schedules_all$away_team)))
all_weeks_sched   <- sort(unique(schedules_all$week))

# Build a flat table: every team x every scheduled week -> did they play?
team_played <- bind_rows(
  schedules_all %>% transmute(week, team = home_team),
  schedules_all %>% transmute(week, team = away_team)
) %>% distinct() %>% mutate(played = TRUE)

# For each team, find the last playoff week they appeared in (NA if never)
team_last_playoff_week <- team_played %>%
  filter(week %in% playoff_weeks) %>%
  group_by(team) %>%
  summarise(last_playoff_week = max(week), .groups = "drop")

# Build full grid: every team x every week that exists in the schedule
team_week_grid <- expand.grid(
  team = all_teams,
  week = all_weeks_sched,
  stringsAsFactors = FALSE
)

team_status_lookup <- team_week_grid %>%
  left_join(team_played, by = c("team", "week")) %>%
  left_join(team_last_playoff_week, by = "team") %>%
  mutate(
    played = coalesce(played, FALSE),
    team_status = case_when(
      played                          ~ "played",
      week %in% reg_season_weeks      ~ "bye-week",
      # Playoff week, team did not play:
      # If they never made playoffs at all, or this week is beyond their last appearance
      week %in% playoff_weeks & (is.na(last_playoff_week) | week > last_playoff_week) ~ "eliminated",
      TRUE                            ~ "bye-week"   # fallback (shouldn't hit)
    )
  ) %>%
  select(team, week, team_status)

# Authoritative team -> opponent mapping straight from the schedule (covers
# reg season + playoffs). Used as the last-resort fallback for opponent_team
# whenever neither the weekly stat row nor snap-count data has it.
team_week_opponent <- bind_rows(
  schedules_all %>% transmute(week, team = home_team, sched_opponent = away_team),
  schedules_all %>% transmute(week, team = away_team, sched_opponent = home_team)
) %>% distinct()

# =====================
# WEEK COMPLETION LOOKUP (for safe re-export of in-progress weeks)
# =====================
# Box-score stats (yards, TDs, etc.) are immutable once a game ends, so
# skipping re-export of an already-written week's JSON file is a safe and
# desirable optimization once that week is over (it also avoids re-hitting
# the ESPN roster endpoints on every run).
#
# Injury/practice designations are NOT immutable in the same way: the
# official report gets updated multiple times across the week (an initial
# estimate on Wednesday, practice participation Thursday/Friday, a final
# Friday designation) right up until kickoff. If a week's JSON file already
# got written earlier in that week — e.g. the pipeline runs right after the
# previous week's Monday night game, before Wednesday's first injury report
# for the upcoming week even exists — the old "skip if file exists" logic
# would leave that file frozen with blank/stale injury data forever, even
# though `nflreadr::load_injuries()` picks up real designations a day or two
# later. This is what was producing "injury data is always blank" even for
# players who were genuinely listed as questionable/doubtful/out.
#
# Fix: only treat a week's existing file as safe to skip once EVERY game
# scheduled for that week has a final score. Otherwise, always regenerate so
# the latest injury report gets baked in.
week_completion <- schedules_all %>%
  group_by(week) %>%
  summarise(
    all_games_final = all(!is.na(home_score) & !is.na(away_score)),
    .groups = "drop"
  )

week_is_final <- function(w) {
  hit <- week_completion$all_games_final[week_completion$week == w]
  # No schedule info at all for this week -> don't risk silently freezing
  # incomplete data; treat it as not-final so it always regenerates.
  if (length(hit) == 0) return(FALSE)
  isTRUE(hit[1])
}

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
  "fg_made_50_59", "fg_made_60_", "fg_missed_0_19", "fg_missed_20_29",
  "fg_missed_30_39", "fg_missed_40_49", "fg_missed_50_59", "fg_missed_60_",
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
  left_join(injuries,      by = c("player_id", "week")) %>%
  left_join(offense_snaps, by = c("player_id", "week")) %>%
  left_join(espn_team_lookup, by = "player_id") %>%
  mutate(
    team          = nflreadr::clean_team_abbrs(team),
    team          = coalesce(team, snap_team),
    opponent_team = nflreadr::clean_team_abbrs(opponent_team),
    opponent_team = coalesce(opponent_team, snap_opponent)
  ) %>%
  group_by(player_id) %>%
  mutate(
    # Season-long team assumption (unchanged from before): once we know a
    # player's team from ANY week/source, apply it everywhere so one missing
    # stat row doesn't null out an otherwise-known team.
    team = if (all(is.na(team))) NA_character_ else last(na.omit(team))
  ) %>%
  ungroup() %>%
  mutate(team = coalesce(team, espn_current_team)) %>%
  left_join(team_status_lookup, by = c("team", "week")) %>%
  left_join(team_week_opponent, by = c("team", "week")) %>%
  mutate(opponent_team = coalesce(opponent_team, sched_opponent)) %>%
  select(-snap_team, -snap_opponent, -sched_opponent, -espn_current_team) %>%
  coalesce_cols(off_stat_cols) %>%
  mutate(
    fantasy_points_ppr        = coalesce(fantasy_points_ppr, 0),
    snap_count                = coalesce(snap_count, 0L),
    game_played               = snap_count > 0,
    team_status               = coalesce(team_status, "played"),
    opponent_team             = coalesce(opponent_team, ""),
    injury_status             = coalesce(injury_status, "ACTIVE"),
    practice_status           = coalesce(practice_status, ""),
    primary_injury            = coalesce(primary_injury, ""),
    secondary_injury          = coalesce(secondary_injury, ""),
    practice_primary_injury   = coalesce(practice_primary_injury, ""),
    practice_secondary_injury = coalesce(practice_secondary_injury, "")
  )

# Keep only players who recorded at least 1 snap in any week this season.
# Uses two sources to avoid dropping players with broken pfr_id bridges:
#   1. offense_snaps (snap count data via pfr bridge)
#   2. weekly_off (direct appearance in offensive play-by-play stats)
active_offense_ids <- bind_rows(
  offense_snaps %>% filter(snap_count > 0) %>% select(player_id),
  weekly_off    %>% filter(!is.na(player_id), player_id != "") %>% select(player_id)
) %>% distinct(player_id)

# =====================
# KICKER PER-TEAM CAP (min 1, max 2 kickers per team)
# =====================
# Kicker snap counts are frequently 0/unreliable (kickers barely register in
# offensive-snap data), so relying on active_offense_ids alone can either
# admit every K ever rostered by a team (preseason adds, practice-squad
# tryouts, etc.) or miss the actual kicker if their snap rows are sparse.
# Instead: rank each team's kickers by real season-long kicking activity
# (FG/PAT attempts first, then weeks with a recorded game, then snaps as a
# tiebreaker) and keep at most the top 2 per team. If a team only has one
# kicker with any activity at all, that one kicker is kept — we never
# fabricate a second kicker just to hit the cap, and we never drop a team's
# lone active kicker.
kicker_activity <- offense_df %>%
  filter(position == "K", !is.na(team), team != "") %>%
  group_by(player_id, team) %>%
  summarise(
    total_kick_att = sum(coalesce(fg_att, 0) + coalesce(pat_att, 0), na.rm = TRUE),
    weeks_active   = sum(game_played, na.rm = TRUE),
    total_snaps    = sum(snap_count, na.rm = TRUE),
    .groups = "drop"
  )

top_kickers_per_team <- kicker_activity %>%
  group_by(team) %>%
  arrange(desc(total_kick_att), desc(weeks_active), desc(total_snaps), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  ungroup() %>%
  distinct(player_id)

message("Kickers kept after per-team cap (max 2): ", nrow(top_kickers_per_team),
        " across ", n_distinct(kicker_activity$team), " teams")

offense_combined <- bind_rows(lapply(offense_positions, function(pos) {
  pos_df <- offense_df %>%
    filter(position == pos) %>%
    semi_join(active_offense_ids, by = "player_id")

  if (pos == "K") {
    pos_df <- pos_df %>% semi_join(top_kickers_per_team, by = "player_id")
  }

  pos_df %>% select(any_of(c(BASE_COLS, POSITION_COLS[[pos]])))
}))

# =====================
# TEAM DEFENSE (DEF) PIPELINE
# =====================
message("Loading schedules for team defense")
schedules <- schedules_all %>% filter(game_type == "REG")

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

# Teams that actually played each week (have real stats)
team_def_played <- team_weekly %>%
  select(season, week, team, def_fumbles_forced, def_sacks, def_interceptions,
         def_tds, def_safeties, fumble_recovery_opp) %>%
  left_join(def_teams,      by = c("season", "week", "team")) %>%
  left_join(opponent_stats, by = c("season", "week", "opponent_team")) %>%
  left_join(team_status_lookup, by = c("team", "week")) %>%
  mutate(
    fantasy_points_ppr =
      (def_sacks * 1) + (def_interceptions * 2) + (def_fumbles_forced * 1) +
      (fumble_recovery_opp * 2) + (def_tds * 6) + (def_safeties * 2),
    team_status = coalesce(team_status, "played")
  ) %>%
  transmute(
    season, week,
    player_id    = paste0("DEF_", team),
    player_name  = paste(team, "DEF"),
    position     = "DEF", team, opponent_team,
    fantasy_points_ppr,
    team_status,
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

# Synthetic rows for any team x week combination that has no real stats entry
# Covers both reg season bye weeks AND playoff weeks where the team was eliminated
teams_with_played_entry <- team_def_played %>%
  select(team, week) %>%
  distinct()

# All weeks across the entire season (reg + playoffs)
all_season_weeks <- sort(unique(schedules_all$week))

team_def_missing <- expand.grid(
  team = all_teams,
  week = all_season_weeks,
  stringsAsFactors = FALSE
) %>%
  anti_join(teams_with_played_entry, by = c("team", "week")) %>%
  # Pull the correct label (bye-week or eliminated) from the lookup we already built
  left_join(team_status_lookup, by = c("team", "week")) %>%
  mutate(
    season                    = season,
    player_id                 = paste0("DEF_", team),
    player_name               = paste(team, "DEF"),
    position                  = "DEF",
    opponent_team             = "",
    fantasy_points_ppr        = 0,
    team_status               = coalesce(team_status, "bye-week"),
    def_fumbles_forced        = 0,
    def_sacks                 = 0,
    def_interceptions         = 0,
    def_tds                   = 0,
    def_safeties              = 0,
    fumble_recovery_opp       = 0,
    passing_yards_allowed     = 0,
    passing_tds_allowed       = 0,
    rushing_yards_allowed     = 0,
    rushing_tds_allowed       = 0,
    injury_status             = "N/A",
    practice_status           = "",
    primary_injury            = "",
    secondary_injury          = "",
    practice_primary_injury   = "",
    practice_secondary_injury = ""
  ) %>%
  select(any_of(names(team_def_played)))

team_def <- bind_rows(team_def_played, team_def_missing) %>%
  arrange(week, team)

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

# =====================
# DEFENSIVE STAT FALLBACK (PLAY-BY-PLAY)
# =====================
# nflreadr::load_player_stats(stat_type = "defense") can lag official box
# scores for very recent games and rookies. A player can have confirmed snap
# participation (via defense_snaps) with NO matching row in weekly_def at
# all — in which case every stat column silently zero-fills later even
# though the player genuinely recorded tackles/sacks/INTs that week.
#
# Fix: identify exactly those gaps (snap_count > 0, no weekly_def row) and
# backfill counting stats directly from play-by-play, which tags individual
# players on each tackle/sack/INT/etc. independently of the box-score table.
# Yardage-based fields (interception yards, defensive TDs, fumble recovery
# yards, tackle-for-loss yards, sack yards) are left at 0 for these fallback
# rows — they're harder to reconstruct reliably from PBP, and the counting
# stats below are what people actually track week to week.
players_with_snaps <- defense_snaps %>%
  filter(snap_count > 0) %>%
  distinct(player_id, week)

weekly_def_present <- weekly_def %>% distinct(player_id, week)

missing_def_keys <- players_with_snaps %>%
  anti_join(weekly_def_present, by = c("player_id", "week")) %>%
  semi_join(players_def, by = "player_id")

pbp_def_stats <- tibble(
  player_id                = character(0),
  week                     = integer(0),
  def_tackles_solo         = integer(0),
  def_tackles_with_assist  = integer(0),
  def_tackles_for_loss     = integer(0),
  def_sacks                = integer(0),
  def_qb_hits              = integer(0),
  def_interceptions        = integer(0),
  def_pass_defended        = integer(0),
  def_fumbles_forced       = integer(0),
  fumble_recovery_opp      = integer(0)
)

if (nrow(missing_def_keys) > 0) {
  message("Backfilling ", nrow(missing_def_keys),
          " defensive stat row(s) from play-by-play (missing from primary source)")

  pbp_weeks_needed <- sort(unique(missing_def_keys$week))

  pbp <- tryCatch(
    nflreadr::load_pbp(seasons = season) %>% filter(week %in% pbp_weeks_needed),
    error = function(e) {
      message("Failed to load play-by-play for defensive stat fallback: ", e$message)
      NULL
    }
  )

  if (!is.null(pbp) && nrow(pbp) > 0) {

    # Collapses one or more "<stat>_N_player_id" columns into per-player,
    # per-week counts (e.g. 2 assist-tackle columns both count toward
    # def_tackles_with_assist).
    count_stat_from_cols <- function(pbp_df, cols, stat_name) {
      empty_result <- tibble(player_id = character(0), week = integer(0)) %>%
        mutate(!!stat_name := integer(0))

      cols <- intersect(cols, names(pbp_df))
      if (length(cols) == 0) return(empty_result)

      long_df <- bind_rows(lapply(cols, function(col) {
        pbp_df %>% transmute(player_id = as.character(.data[[col]]), week)
      }))

      result <- long_df %>%
        filter(!is.na(player_id), player_id != "") %>%
        count(player_id, week, name = stat_name)

      if (nrow(result) == 0) return(empty_result)
      result
    }

    stat_specs <- list(
      def_tackles_solo         = c("solo_tackle_1_player_id"),
      def_tackles_with_assist  = c("assist_tackle_1_player_id", "assist_tackle_2_player_id",
                                    "assist_tackle_3_player_id", "assist_tackle_4_player_id"),
      def_tackles_for_loss     = c("tackle_for_loss_1_player_id", "tackle_for_loss_2_player_id"),
      def_sacks                = c("sack_player_id", "half_sack_1_player_id", "half_sack_2_player_id"),
      def_qb_hits               = c("qb_hit_1_player_id", "qb_hit_2_player_id"),
      def_interceptions        = c("interception_player_id"),
      def_pass_defended        = c("pass_defense_1_player_id", "pass_defense_2_player_id"),
      def_fumbles_forced       = c("forced_fumble_player_1_player_id", "forced_fumble_player_2_player_id"),
      fumble_recovery_opp      = c("fumble_recovery_1_player_id", "fumble_recovery_2_player_id")
    )

    stat_tables <- lapply(names(stat_specs), function(stat_name) {
      count_stat_from_cols(pbp, stat_specs[[stat_name]], stat_name)
    })

    pbp_def_stats <- Reduce(
      function(x, y) full_join(x, y, by = c("player_id", "week")),
      stat_tables
    ) %>%
      mutate(across(-c(player_id, week), ~ coalesce(as.integer(.x), 0L))) %>%
      semi_join(missing_def_keys, by = c("player_id", "week"))

    message("Play-by-play backfill produced ", nrow(pbp_def_stats), " matching row(s)")
  }
}

individual_def_df <- expand.grid(player_id = players_def$player_id, week = def_weeks,
                                  stringsAsFactors = FALSE) %>%
  left_join(players_def, by = "player_id") %>%
  left_join(weekly_def %>% select(-any_of(c("player_name", "position", "headshot_url"))),
            by = c("player_id", "week")) %>%
  left_join(injuries,      by = c("player_id", "week")) %>%
  left_join(defense_snaps, by = c("player_id", "week")) %>%
  left_join(pbp_def_stats, by = c("player_id", "week"), suffix = c("", "_pbp")) %>%
  mutate(
    def_tackles_solo         = coalesce(def_tackles_solo, def_tackles_solo_pbp),
    def_tackles_with_assist  = coalesce(def_tackles_with_assist, def_tackles_with_assist_pbp),
    def_tackles_for_loss     = coalesce(def_tackles_for_loss, def_tackles_for_loss_pbp),
    def_sacks                = coalesce(def_sacks, def_sacks_pbp),
    def_qb_hits              = coalesce(def_qb_hits, def_qb_hits_pbp),
    def_interceptions        = coalesce(def_interceptions, def_interceptions_pbp),
    def_pass_defended        = coalesce(def_pass_defended, def_pass_defended_pbp),
    def_fumbles_forced       = coalesce(def_fumbles_forced, def_fumbles_forced_pbp),
    fumble_recovery_opp      = coalesce(fumble_recovery_opp, fumble_recovery_opp_pbp),
    # If the primary source had no row (season NA) but PBP backfilled it,
    # the row is definitively real data for this season — stamp it so
    # "season": null no longer shows up for confirmed-played weeks that we
    # were able to reconstruct. Rows with no PBP match either stay NA here
    # and get zero-filled downstream as before (no snap data, truly inactive).
    season = if_else(is.na(season) & !is.na(def_tackles_solo_pbp), .env$season, season)
  ) %>%
  select(-ends_with("_pbp")) %>%
  left_join(espn_team_lookup, by = "player_id") %>%
  mutate(
    team          = nflreadr::clean_team_abbrs(team),
    team          = coalesce(team, snap_team),
    opponent_team = nflreadr::clean_team_abbrs(opponent_team),
    opponent_team = coalesce(opponent_team, snap_opponent)
  ) %>%
  group_by(player_id) %>%
  mutate(
    # Season-long team assumption (unchanged from before): once we know a
    # player's team from ANY week/source, apply it everywhere so one missing
    # stat row doesn't null out an otherwise-known team.
    team = if (all(is.na(team))) NA_character_ else last(na.omit(team))
  ) %>%
  ungroup() %>%
  mutate(team = coalesce(team, espn_current_team)) %>%
  left_join(team_status_lookup, by = c("team", "week")) %>%
  left_join(team_week_opponent, by = c("team", "week")) %>%
  mutate(opponent_team = coalesce(opponent_team, sched_opponent)) %>%
  select(-snap_team, -snap_opponent, -sched_opponent, -espn_current_team) %>%
  coalesce_cols(def_stat_cols) %>%
  mutate(
    fantasy_points_ppr        = coalesce(fantasy_points_ppr, 0),
    snap_count                = coalesce(snap_count, 0L),
    game_played               = snap_count > 0,
    team_status               = coalesce(team_status, "played"),
    opponent_team             = coalesce(opponent_team, ""),
    injury_status             = coalesce(injury_status, "ACTIVE"),
    practice_status           = coalesce(practice_status, ""),
    primary_injury            = coalesce(primary_injury, ""),
    secondary_injury          = coalesce(secondary_injury, ""),
    practice_primary_injury   = coalesce(practice_primary_injury, ""),
    practice_secondary_injury = coalesce(practice_secondary_injury, "")
  )

# Keep only players who recorded at least 1 defensive snap in any week this season.
# Uses two sources to avoid dropping players with broken pfr_id bridges:
#   1. defense_snaps (snap count data via pfr bridge)
#   2. weekly_def_raw (direct appearance in defensive play-by-play stats)
active_defense_ids <- bind_rows(
  defense_snaps %>% filter(snap_count > 0) %>% select(player_id),
  weekly_def_raw %>%
    mutate(position = normalize_def_position(position)) %>%
    filter(position %in% def_positions, !is.na(player_id), player_id != "") %>%
    select(player_id)
) %>% distinct(player_id)

individual_def_combined <- bind_rows(lapply(def_positions, function(pos) {
  individual_def_df %>%
    filter(position == pos) %>%
    semi_join(active_defense_ids, by = "player_id") %>%
    select(any_of(c(BASE_COLS, POSITION_COLS[[pos]])))
}))

defense_export <- bind_rows(individual_def_combined, team_def)

# =====================
# EXPORT — OFFENSE (QB/RB/WR/TE/K only)
# =====================
off_dir <- stats_dir("Offense")

for (w in sort(unique(offense_combined$week))) {
  file_name <- file.path(off_dir, sprintf("player_stats_%s_week%02d.json", season, as.integer(w)))
  if (file.exists(file_name) && week_is_final(w)) {
    message("Skipping (already exists, week is final): ", file_name)
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
  if (file.exists(file_name) && week_is_final(w)) {
    message("Skipping (already exists, week is final): ", file_name)
    next
  }
  export_week_json(defense_export %>% filter(week == w), def_dir, season, w)
}

message("✅ All weekly JSON files generated successfully.")
