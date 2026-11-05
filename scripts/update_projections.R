# scripts/update_projections.R
# Saves Sleeper's weekly fantasy projections for the rest of the season, so the app can show a
# player's projected points for upcoming weeks without calling Sleeper itself.
#
# Output: data/projections/{season}/week{NN}.json, one file per week from the current week
# through the end of the regular season (past weeks are left alone: they have real stats):
#   { season, week, generated_at, players: { <sleeper id>: { name, pos, team, opp, ppr, half, std, stats } } }
# `stats` holds the main projected stat line (yards, TDs, catches, kicks) for the player page.
#
# Projections change during the week (injuries, depth charts), so the workflow runs every few hours.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})
source("scripts/projection_helpers.R")

OUTPUT_DIR <- "data/projections"
SLEEPER_V1 <- "https://api.sleeper.app/v1"
LAST_REGULAR_WEEK <- as.integer(Sys.getenv("LAST_REGULAR_WEEK", "18"))

get_json <- function(url) {
  res <- tryCatch(GET(url, timeout(60)), error = function(e) NULL)
  if (is.null(res) || status_code(res) != 200) return(NULL)
  fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

state <- get_json(paste0(SLEEPER_V1, "/state/nfl"))
if (is.null(state)) stop("Could not read the NFL state from Sleeper")
season <- as.integer(state$season)
current <- as.integer(state$week %||% 1)
if (!identical(state$season_type, "regular")) {
  message("Not the regular season (", state$season_type, "): nothing to project")
  quit(status = 0)
}

written <- 0L
for (week in seq(max(1L, current), LAST_REGULAR_WEEK)) {
  rows <- fetch_week_projections(season, week, get_json)
  if (!length(rows)) {
    message("⚠️ Week ", week, ": no projections from Sleeper")
    next
  }
  players <- slim_projections(rows)
  dir.create(file.path(OUTPUT_DIR, season), recursive = TRUE, showWarnings = FALSE)
  out <- list(
    season = season,
    week = week,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    players = players
  )
  write_json(out, file.path(OUTPUT_DIR, season, sprintf("week%02d.json", week)), auto_unbox = TRUE, digits = NA, null = "null")
  message("✅ Week ", week, ": ", length(players), " players")
  written <- written + 1L
}
message("Wrote ", written, " projection files")
