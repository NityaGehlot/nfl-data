# =====================
# scripts/update_injuries.R
# Current injury designations + this week's day-by-day practice report.
#
# Outputs:
#   data/injuries/injuries_current.json            every injured / listed fantasy player right now
#   data/injuries/history/{season}/week{NN}.json   one entry per player per day this week
#                                                  (earlier seasons: scripts/backfill_injury_history.R)
#
# Sources: Sleeper's live player database (IR / Out / Questionable / ... + body part), the
# NFL's official weekly injury report via nflreadr (game status + practice participation), and
# the latest injury headlines from data/news (beat reports like "not spotted at practice"
# usually come out before the official report).
# Runs a few times a day from .github/workflows/update-injuries.yml.
# =====================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})
source("scripts/injury_helpers.R")

OUTPUT_DIR  <- "data/injuries"
HISTORY_DIR <- file.path(OUTPUT_DIR, "history")
dir.create(HISTORY_DIR, recursive = TRUE, showWarnings = FALSE)

SLEEPER_API <- Sys.getenv("SLEEPER_API_URL", "https://api.sleeper.app/v1")

get_json <- function(url) {
  res <- GET(url, timeout(60))
  if (status_code(res) != 200) stop("Request failed (", status_code(res), "): ", url)
  fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

now_utc <- Sys.time()
attr(now_utc, "tzone") <- "UTC"

# ---- Which season / week is it? ----
state  <- get_json(paste0(SLEEPER_API, "/state/nfl"))
season <- as.integer(state$season %||% format(now_utc, "%Y"))
week   <- as.integer(state$week %||% 0)
season_type <- state$season_type %||% "regular"
message("Season ", season, " week ", week, " (", season_type, ")")

# ---- Sleeper live designations ----
message("Downloading Sleeper players...")
players <- get_json(paste0(SLEEPER_API, "/players/nfl"))
message("Sleeper players: ", length(players))

# ---- Official NFL injury report for this week (practice participation + game status) ----
report <- NULL
season_report <- NULL
if (season_type %in% c("regular", "post") && week > 0) {
  report <- tryCatch({
    inj <- nflreadr::load_injuries(seasons = season)
    inj <- as.data.frame(inj)
    season_report <- inj
    inj <- inj[inj$week == week, , drop = FALSE]
    # Keep the latest row per player for the week
    if ("date_modified" %in% names(inj)) inj <- inj[order(inj$date_modified, decreasing = TRUE), , drop = FALSE]
    inj[!duplicated(inj$gsis_id), , drop = FALSE]
  }, error = function(e) {
    message("⚠️ NFL injury report unavailable (", conditionMessage(e), "); using Sleeper only")
    NULL
  })
  message("NFL report rows this week: ", if (is.null(report)) 0 else nrow(report))
}

rows <- build_injury_rows(players, report)
message("Players with a designation: ", length(rows))

# ---- Latest injury news (e.g. "not spotted at practice" comes out before the official report) ----
news_files <- list.files("data/news", pattern = "^news_.*\\.json$", full.names = TRUE)
news_files <- news_files[basename(news_files) != "news_meta.json"]
news <- unlist(lapply(news_files, function(f) tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) list())), recursive = FALSE)
rows <- attach_news(rows, latest_injury_news(news, eastern_date(now_utc)), eastern_date(now_utc))
message("Players with recent injury news: ", sum(vapply(rows, function(r) !is.null(r$latest_news), logical(1))))

# ---- Day-by-day history for this week: data/injuries/history/{season}/week{NN}.json ----
season_dir <- file.path(HISTORY_DIR, season)
dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)
# Files from before the per-season folders (history/2026_week08.json) move into them.
for (old in list.files(HISTORY_DIR, pattern = "^[0-9]{4}_week[0-9]{2}\\.json$", full.names = TRUE)) {
  m <- regmatches(basename(old), regexec("^([0-9]{4})_week([0-9]{2})\\.json$", basename(old)))[[1]]
  dest <- file.path(HISTORY_DIR, m[2], sprintf("week%s.json", m[3]))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(dest)) file.rename(old, dest) else file.remove(old)
}
# Earlier weeks of this season that were never saved: fill them from the official report.
if (!is.null(season_report) && week > 1) {
  past <- injury_report_weeks(season_report[season_report$week < week, , drop = FALSE])
  for (wk in names(past)) {
    path <- file.path(season_dir, sprintf("week%02d.json", as.integer(wk)))
    if (!file.exists(path)) write_json(past[[wk]], path, auto_unbox = TRUE, null = "null")
  }
}
history_file <- file.path(season_dir, sprintf("week%02d.json", week))
history <- if (file.exists(history_file)) fromJSON(history_file, simplifyVector = FALSE) else list()
history <- merge_history(history, rows, now_utc)
if (week > 0) write_json(history, history_file, auto_unbox = TRUE, pretty = TRUE, null = "null")

current <- list(
  generated_at = format(now_utc, "%Y-%m-%dT%H:%M:%SZ"),
  season = season,
  week = week,
  season_type = season_type,
  players = attach_history(rows, history)
)
write_json(current, file.path(OUTPUT_DIR, "injuries_current.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
message("✅ Wrote ", length(rows), " injury records")
