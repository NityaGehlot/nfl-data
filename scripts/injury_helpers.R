# =====================
# scripts/injury_helpers.R
# Pure helpers for update_injuries.R: normalizing statuses, building the current injury
# list and keeping a day-by-day history for the week (like a team's official injury report).
# No network access, so these can be tested with scripts/tests/test_injury_helpers.R.
# =====================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !(length(a) == 1 && is.na(a))) a else b

# Sleeper: Questionable / Doubtful / Out / IR / PUP / Sus / NA / COV / DNR
# NFL report: Questionable / Doubtful / Out
normalize_status <- function(x) {
  s <- toupper(trimws(as.character(x %||% "")))
  if (s %in% c("", "NULL", "ACTIVE", "HEALTHY")) return("")
  if (s %in% c("Q", "QUESTIONABLE")) return("QUESTIONABLE")
  if (s %in% c("D", "DOUBTFUL")) return("DOUBTFUL")
  if (s %in% c("O", "OUT")) return("OUT")
  if (s %in% c("IR", "INJURED RESERVE", "IR-R", "IR-NFI", "IR-DFR")) return("IR")
  if (s %in% c("PUP", "PUP-R", "PUP-P")) return("PUP")
  if (s %in% c("SUS", "SUSPENDED", "SUSP")) return("SUSPENDED")
  if (s %in% c("NA", "NFI", "NFI-R", "NON FOOTBALL INJURY", "NOT ACTIVE")) return("NOT_ACTIVE")
  if (s %in% c("COV", "COVID")) return("OUT")
  s
}

# NFL practice participation -> DNP / LP / FP (blank when not listed)
normalize_practice <- function(x) {
  s <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(s)) return("")
  if (grepl("did not|dnp|not participate", s)) return("DNP")
  if (grepl("limited|^lp$", s)) return("LP")
  if (grepl("full|^fp$", s)) return("FP")
  if (grepl("out", s)) return("DNP")
  ""
}

# Accepts a POSIXct or an ISO-8601 string like "2026-10-28T21:00:00Z". (Plain as.POSIXct()
# silently drops the time from that string format, which shifts the Eastern date.)
to_utc <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  s <- sub("Z$", "", as.character(x))
  t <- as.POSIXct(s, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  if (is.na(t)) t <- as.POSIXct(s, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  if (is.na(t)) t <- as.POSIXct(s, tz = "UTC")
  t
}

# Weekday label in US Eastern time (practice reports are published on ET days).
weekday_label <- function(time_utc) {
  format(to_utc(time_utc), "%a", tz = "America/New_York")
}

eastern_date <- function(time_utc) {
  format(to_utc(time_utc), "%Y-%m-%d", tz = "America/New_York")
}

severity <- function(status) {
  order <- c(IR = 1, SUSPENDED = 2, PUP = 3, NOT_ACTIVE = 4, OUT = 5, DOUBTFUL = 6, QUESTIONABLE = 7)
  unname(order[status] %||% 9)
}

#' Build the list of currently injured / designated players.
#' @param sleeper_players named list from Sleeper /players/nfl (keyed by player id)
#' @param report data.frame of this week's NFL injury report rows (nflreadr::load_injuries
#'   columns: gsis_id, report_status, practice_status, report_primary_injury, ...). May be empty.
#' @return list of player records (only players with a designation, report row or practice note)
build_injury_rows <- function(sleeper_players, report, positions = c("QB", "RB", "WR", "TE", "K", "DEF")) {
  report <- if (is.null(report)) data.frame() else as.data.frame(report)
  report_by_gsis <- if (nrow(report) > 0 && "gsis_id" %in% names(report)) split(report, report$gsis_id) else list()

  rows <- list()
  for (id in names(sleeper_players)) {
    p <- sleeper_players[[id]]
    pos <- toupper(p$position %||% "")
    if (!(pos %in% positions) || is.null(p$team) || !nzchar(p$team %||% "")) next

    gsis <- trimws(as.character(p$gsis_id %||% ""))
    r <- if (nzchar(gsis) && !is.null(report_by_gsis[[gsis]])) report_by_gsis[[gsis]][1, ] else NULL

    sleeper_status <- normalize_status(p$injury_status)
    game_status <- if (!is.null(r)) normalize_status(r$report_status) else ""
    practice <- if (!is.null(r)) normalize_practice(r$practice_status) else ""
    if (!nzchar(sleeper_status) && !nzchar(game_status) && !nzchar(practice)) next

    injury <- p$injury_body_part %||% (if (!is.null(r)) r$report_primary_injury %||% r$practice_primary_injury else NULL) %||% ""
    rows[[length(rows) + 1]] <- list(
      player_id = id,
      gsis_id = if (nzchar(gsis)) gsis else NULL,
      full_name = p$full_name %||% paste(p$first_name %||% "", p$last_name %||% ""),
      team = p$team,
      position = pos,
      # Most current designation: Sleeper's live status, else the official game status.
      status = if (nzchar(sleeper_status)) sleeper_status else game_status,
      game_status = if (nzchar(game_status)) game_status else NULL,
      practice_status = if (nzchar(practice)) practice else NULL,
      injury = if (nzchar(injury)) injury else NULL,
      notes = p$injury_notes %||% NULL,
      injury_start_date = p$injury_start_date %||% NULL
    )
  }

  if (length(rows) == 0) return(list())
  ord <- order(vapply(rows, function(r) severity(r$status), numeric(1)),
               vapply(rows, function(r) r$team, character(1)),
               vapply(rows, function(r) r$full_name, character(1)))
  rows[ord]
}

#' Add today's snapshot to the week's history.
#' history: named list player_id -> list of entries list(date, day, practice_status, game_status, status).
#' One entry per Eastern date per player; a later run the same day replaces that day's entry.
merge_history <- function(history, rows, time_utc) {
  history <- history %||% list()
  date <- eastern_date(time_utc)
  day <- weekday_label(time_utc)
  for (r in rows) {
    entry <- Filter(Negate(is.null), list(
      date = date, day = day,
      practice_status = r$practice_status,
      game_status = r$game_status,
      status = r$status
    ))
    prev <- history[[r$player_id]] %||% list()
    prev <- Filter(function(e) !identical(e$date, date), prev)
    history[[r$player_id]] <- c(prev, list(entry))
  }
  history
}

#' Attach each player's entries for the week so the app can draw the Wed/Thu/Fri table.
attach_history <- function(rows, history) {
  lapply(rows, function(r) {
    r$history <- history[[r$player_id]] %||% list()
    r
  })
}
