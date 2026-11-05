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

    # Sleeper keeps a player's Questionable/Doubtful/Out from his last game until the new week's
    # report. Say where the status comes from so it isn't read as this week's designation.
    status_note <- if (nzchar(game_status)) {
      "From this week's official injury report."
    } else if (sleeper_status %in% c("QUESTIONABLE", "DOUBTFUL", "OUT")) {
      "Carried over from his last game. This week's official status comes out on Friday (Thursday or Saturday for other game days)."
    } else NULL

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
      status_note = status_note,
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
      practice_source = r$practice_source,
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

# ---------------------
# Latest injury news (reports that come out before the official injury report)
# ---------------------

#' Practice participation a headline reports: "DNP", "LP", "FP", or "" (none).
practice_from_headline <- function(title) {
  t <- tolower(title %||% "")
  if (grepl("(out of|absent from|away from|missed|miss(es)?) practice|did not practice|didn't practice|not (spotted|seen|present)|(sits?|sat) out|held out|\\bdnp\\b", t)) return("DNP")
  if (grepl("limited", t)) return("LP")
  if (grepl("full practice|full participant|fully participat|back at practice|returns? to practice|practic(ed|es|ing) in full", t)) return("FP")
  ""
}

# Headlines about HIS injury or practice status (same pattern as the app's injuryNews.ts).
# Words like "injuries" alone aren't enough: "...a career day when injuries mounted" is a game story.
INJURY_NEWS_RX <- "ruled out|\\b(is|listed|remains?|as|considered)\\s+(questionable|doubtful|out)\\b|questionable|doubtful|week-to-week|day-to-day|game-time decision|injured reserve|\\b(placed|lands?|landed|heads?|goes?) on (ir|injured reserve)\\b|\\bto ir\\b|off (ir|injured reserve)|activated|designated to return|did not practice|didn't practice|\\bdnp\\b|(limited|full|no|not) (in )?practice|practice (in full|fully|limited)|(out of|absent from|miss(es|ed)?|sits? out|held out of|returns? to|back at) practice|not spotted|sidelined|(suffer|sustain|aggravat|tweak)\\w* (an? |his )?\\w*\\s?(injury|sprain|strain|tear|fracture|concussion)|(ankle|knee|hamstring|groin|calf|foot|shoulder|back|neck|hip|quad|wrist|hand|toe|rib|chest|elbow|achilles|acl|mcl|oblique)\\s+(injury|sprain|strain|tear|issue|soreness|surgery)|concussion|surgery|\\bmri\\b|x-rays?|carted off|left (the )?game|won't return|will not return|out for (the )?(season|year|game|week|\\d)|expected to (miss|return|play)|(update|news) on .* injury|injury update|\\((ankle|knee|hamstring|groin|calf|foot|shoulder|back|neck|hip|quad|wrist|hand|toe|ribs?|chest|elbow|achilles|concussion|illness|thumb|finger|pectoral|abdomen|heel|shin|personal)[^)]*\\)"

#' Is this headline an update on HIS injury? The injury words must be in the same part of the
#' headline as his name ("Jeremiah Love placed on IR, Tyler Allgeier becomes RB1" is Love's
#' injury, not Allgeier's). Headlines that don't name him count only when filed as injury news.
is_injury_headline <- function(title, player, category = NULL) {
  title <- tolower(title %||% "")
  filed_injury <- (category %||% "") %in% c("injury", "injury_return")
  last <- tolower(player %||% "")
  last <- sub("\\s+(jr|sr|ii|iii|iv|v)\\.?$", "", last)
  last <- gsub("[^a-z' -]", "", last)
  parts <- strsplit(trimws(last), "\\s+")[[1]]
  last <- if (length(parts)) parts[length(parts)] else ""
  if (!nzchar(last) || !grepl(last, title, fixed = TRUE)) return(filed_injury)
  # Dashes and curly quotes become breaks too (works the same in any locale).
  ascii <- iconv(title, "UTF-8", "ASCII", sub = ",")
  if (is.na(ascii)) ascii <- title
  clauses <- strsplit(ascii, "[,;:|]|\\s-\\s|\\band\\b|\\bwhile\\b", perl = TRUE)[[1]]
  own <- any(vapply(clauses, function(c) grepl(last, c, fixed = TRUE) && grepl(INJURY_NEWS_RX, c, perl = TRUE), logical(1)))
  own || (filed_injury && !grepl(INJURY_NEWS_RX, title, perl = TRUE))
}

#' Newest injury-related article per player (by name) within `max_days` of `now_date`.
#' news: list of articles (title, player, published "YYYY-MM-DD", source, impact, summary, category).
latest_injury_news <- function(news, now_date, max_days = 4) {
  out <- list()
  for (a in news) {
    name <- tolower(a$player %||% "")
    if (!nzchar(name)) next
    pub <- suppressWarnings(as.Date(a$published %||% NA))
    if (is.na(pub) || as.numeric(as.Date(now_date) - pub) > max_days || pub > as.Date(now_date) + 1) next
    is_injury <- is_injury_headline(a$title, a$player, a$category)
    if (!is_injury) next
    prev <- out[[name]]
    if (is.null(prev) || pub > as.Date(prev$published)) {
      out[[name]] <- Filter(Negate(is.null), list(
        title = a$title, source = a$source, published = as.character(pub), impact = a$impact,
        summary = a$summary, link = a$url %||% a$link,
        practice = { p <- practice_from_headline(a$title); if (nzchar(p)) p else NULL }
      ))
    }
  }
  out
}

#' Attach each injured player's latest injury news, and fill in today's practice from news when
#' the official report hasn't said anything yet (marked practice_source = "news").
attach_news <- function(rows, news_by_name, today) {
  lapply(rows, function(r) {
    n <- news_by_name[[tolower(r$full_name %||% "")]]
    if (is.null(n)) return(r)
    r$latest_news <- n
    recent <- as.numeric(as.Date(today) - as.Date(n$published)) <= 1
    if (is.null(r$practice_status) && !is.null(n$practice) && recent) {
      r$practice_status <- n$practice
      r$practice_source <- "news"
    }
    r
  })
}

# ---------------------
# Past weeks: the official report for every week of a season (backfill_injury_history.R)
# ---------------------

#' nflverse practice text -> "DNP" / "LP" / "FP" (NA when not on the practice report).
practice_code <- function(x) {
  t <- tolower(ifelse(is.na(x), "", x))
  ifelse(grepl("did not|not participate", t), "DNP",
    ifelse(grepl("limited", t), "LP", ifelse(grepl("full", t), "FP", NA_character_)))
}

#' One season of nflverse injury rows -> list(week -> list(gsis_id -> list(entry))).
#' Keeps each player's final report for the week (latest date_modified).
injury_report_weeks <- function(inj) {
  inj <- as.data.frame(inj, stringsAsFactors = FALSE)
  inj <- inj[!is.na(inj$gsis_id) & nzchar(inj$gsis_id) & !is.na(inj$week), , drop = FALSE]
  if (!nrow(inj)) return(list())
  modified <- as.character(inj$date_modified %||% rep(NA_character_, nrow(inj)))
  inj <- inj[order(modified, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  inj <- inj[!duplicated(paste(inj$week, inj$gsis_id)), , drop = FALSE]
  modified <- as.character(inj$date_modified)
  stamp <- as.POSIXct(substr(modified, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  status <- toupper(trimws(ifelse(is.na(inj$report_status), "", inj$report_status)))
  practice <- practice_code(inj$practice_status)
  injury <- ifelse(!is.na(inj$report_primary_injury) & nzchar(inj$report_primary_injury), inj$report_primary_injury, inj$practice_primary_injury)
  out <- list()
  for (i in seq_len(nrow(inj))) {
    has_time <- !is.na(stamp[i])
    entry <- Filter(function(v) !is.null(v) && !(length(v) == 1 && is.na(v)), list(
      date = if (has_time) eastern_date(stamp[i]) else NULL,
      day = if (has_time) weekday_label(stamp[i]) else "Final",
      practice_status = practice[i],
      game_status = if (nzchar(status[i])) status[i] else NULL,
      injury = injury[i],
      name = inj$full_name[i],
      team = inj$team[i],
      position = inj$position[i],
      source = "official"
    ))
    wk <- as.character(as.integer(inj$week[i]))
    if (is.null(out[[wk]])) out[[wk]] <- list()
    out[[wk]][[inj$gsis_id[i]]] <- list(entry)
  }
  out
}
