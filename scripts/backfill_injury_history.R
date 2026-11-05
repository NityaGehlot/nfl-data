# scripts/backfill_injury_history.R
# Fills data/injuries/history/{season}/week{NN}.json with the NFL's official injury report for
# every week of every season we have stats for (2011 on), from nflverse's injury data
# (github.com/nflverse/nflverse-data, release "injuries").
#
# What a team's official report says each week:
#   * practice participation: Did Not Participate (DNP) / Limited (LP) / Full (FP)
#   * game status (Friday, or Saturday for Monday games): Out / Doubtful / Questionable
#   * the injury (body part)
# nflverse keeps the final version of each week's report, so a past week has one entry per player
# (the day it was last updated). Players on injured reserve aren't on the weekly report.
#
# Week files written by update_injuries.R for the live season (day-by-day, keyed by Sleeper id)
# are never overwritten. Backfilled weeks are keyed by gsis id and carry name / team / position,
# so the app can match players either way.
#
# Usage: Rscript scripts/backfill_injury_history.R [first_season] [last_season]
#   (defaults: 2011 .. the latest season in data/Stats)

suppressPackageStartupMessages({
  library(jsonlite)
})
source("scripts/injury_helpers.R")

HISTORY_DIR <- "data/injuries/history"
NFLVERSE_URL <- "https://github.com/nflverse/nflverse-data/releases/download/injuries/injuries_%d.csv"

args <- commandArgs(trailingOnly = TRUE)
stat_seasons <- suppressWarnings(as.integer(sub(" Season$", "", list.files("data/Stats", pattern = "^[0-9]{4} Season$"))))
stat_seasons <- stat_seasons[!is.na(stat_seasons)]
first <- if (length(args) >= 1) as.integer(args[1]) else 2011L
last <- if (length(args) >= 2) as.integer(args[2]) else if (length(stat_seasons)) max(stat_seasons) else as.integer(format(Sys.Date(), "%Y"))

load_season <- function(season) {
  if (requireNamespace("nflreadr", quietly = TRUE)) {
    out <- tryCatch(as.data.frame(nflreadr::load_injuries(seasons = season)), error = function(e) NULL)
    if (!is.null(out) && nrow(out)) return(out)
  }
  tryCatch(read.csv(sprintf(NFLVERSE_URL, season), stringsAsFactors = FALSE, na.strings = c("", "NA")), error = function(e) NULL)
}

written <- 0L
for (season in seq(first, last)) {
  inj <- load_season(season)
  if (is.null(inj) || !nrow(inj)) {
    message("⚠️ ", season, ": no injury data")
    next
  }
  weeks <- injury_report_weeks(inj)
  dir.create(file.path(HISTORY_DIR, season), recursive = TRUE, showWarnings = FALSE)
  for (wk in names(weeks)) {
    path <- file.path(HISTORY_DIR, season, sprintf("week%02d.json", as.integer(wk)))
    if (file.exists(path)) next # live day-by-day files win
    write_json(weeks[[wk]], path, auto_unbox = TRUE, null = "null")
    written <- written + 1L
  }
  message("✅ ", season, ": ", length(weeks), " weeks")
}
message("Wrote ", written, " week files")
