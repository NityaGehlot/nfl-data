# scripts/projection_helpers.R
# Fetching and slimming Sleeper's weekly projections (used by update_projections.R).

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !(length(a) == 1 && is.na(a))) a else b

PROJECTION_POSITIONS <- c("QB", "RB", "WR", "TE", "K", "DEF")

# The projected stats worth showing on a player page (the rest of Sleeper's ~60 fields are
# buckets, ADP and other things the app doesn't use).
PROJECTION_STATS <- c(
  "pass_att", "pass_cmp", "pass_yd", "pass_td", "pass_int",
  "rush_att", "rush_yd", "rush_td",
  "rec_tgt", "rec", "rec_yd", "rec_td",
  "fgm", "fga", "xpm",
  "def_sack", "def_int", "def_fum_rec", "def_td", "pts_allow"
)

#' Sleeper's projections for one week. Tries the newer endpoint (with each player's team and
#' opponent) and falls back to the classic /v1 one. `get_json(url)` returns parsed JSON or NULL.
fetch_week_projections <- function(season, week, get_json) {
  positions <- paste0("position[]=", PROJECTION_POSITIONS, collapse = "&")
  newer <- get_json(sprintf("https://api.sleeper.com/projections/nfl/%d/%d?season_type=regular&%s", season, week, positions))
  if (is.list(newer) && length(newer)) return(newer)
  classic <- get_json(sprintf("https://api.sleeper.app/v1/projections/nfl/regular/%d/%d", season, week))
  if (!is.list(classic) || !length(classic)) return(list())
  # Classic form: { player_id: stats } -> the same row shape as the newer one.
  lapply(names(classic), function(id) list(player_id = id, stats = classic[[id]]))
}

#' Rows -> { player_id: { name, pos, team, opp, ppr, half, std, stats } } for players Sleeper
#' projects to score (everyone else is left out to keep the file small).
slim_projections <- function(rows) {
  out <- list()
  for (r in rows) {
    s <- r[["stats"]] %||% list()
    ppr <- suppressWarnings(as.numeric(s$pts_ppr %||% NA))
    if (is.na(ppr) || ppr <= 0) next
    p <- r[["player"]] %||% list()  # [[ ]]: $ would partial-match "player_id"
    name <- trimws(paste(p$first_name %||% "", p$last_name %||% ""))
    stats <- list()
    for (k in PROJECTION_STATS) {
      v <- suppressWarnings(as.numeric(s[[k]] %||% NA))
      if (!is.na(v) && v != 0) stats[[k]] <- round(v, 2)
    }
    out[[as.character(r$player_id)]] <- Filter(Negate(is.null), list(
      name = if (nzchar(name)) name else NULL,
      pos = p$position %||% (p$fantasy_positions[[1]] %||% NULL),
      team = r$team %||% p$team,
      opp = r$opponent,
      ppr = round(ppr, 2),
      half = round(as.numeric(s$pts_half_ppr %||% ppr), 2),
      std = round(as.numeric(s$pts_std %||% ppr), 2),
      stats = if (length(stats)) stats else NULL
    ))
  }
  out
}
