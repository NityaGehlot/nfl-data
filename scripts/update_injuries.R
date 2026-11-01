# =====================
# scripts/update_injuries.R
# Current injury designations + this week's day-by-day practice report.
#
# Outputs:
#   data/injuries/injuries_current.json            every injured / listed fantasy player right now
#   data/injuries/history/{season}_week{NN}.json   one entry per player per day this week
#
# Sources: Sleeper's live player database (IR / Out / Questionable / ... + body part) and the
# NFL's official weekly injury report via nflreadr (game status + practice participation).
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
if (season_type %in% c("regular", "post") && week > 0) {
  report <- tryCatch({
    inj <- nflreadr::load_injuries(seasons = season)
    inj <- as.data.frame(inj)
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

# ---- Day-by-day history for this week ----
history_file <- file.path(HISTORY_DIR, sprintf("%d_week%02d.json", season, week))
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
