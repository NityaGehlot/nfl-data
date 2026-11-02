# scripts/generate_defense_rankings.R
# Writes data/defense/defense_rankings_<season>.json and defense_rankings_current.json:
# per-game yards/TDs/fantasy points each defense allows, overall and by position, with ranks.
# Run after generate_weekly_stats.R:  Rscript scripts/generate_defense_rankings.R [season]

suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
})
source("scripts/defense_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
stats_root <- "data/Stats"
available <- suppressWarnings(as.integer(sub(" Season$", "", list.files(stats_root))))
available <- sort(available[!is.na(available)])
season <- if (length(args) >= 1 && nzchar(args[1])) as.integer(args[1]) else max(available)

offense_dir <- file.path(stats_root, paste(season, "Season"), paste(season, "Offense"))
files <- list.files(offense_dir, pattern = "\\.json$", full.names = TRUE)
if (length(files) == 0) stop("No offense stats found in ", offense_dir)

read_rows <- function(path) {
  data <- fromJSON(path, simplifyVector = FALSE)
  rows <- unlist(data, recursive = FALSE)
  if (length(rows) == 0) return(NULL)
  bind_rows(lapply(rows, function(r) {
    r <- r[!vapply(r, is.null, logical(1))]
    as.data.frame(lapply(r, function(v) if (is.list(v)) NA else v), stringsAsFactors = FALSE)
  }))
}

rows <- bind_rows(lapply(files, read_rows))
rows <- rows %>% filter(is.na(game_played) | game_played %in% c(TRUE, "TRUE", "true"))
through_week <- max(as.integer(rows$week), na.rm = TRUE)

table <- compute_defense_table(rows)
recent <- compute_defense_table(rows %>% filter(as.integer(week) > through_week - 4))
out <- defense_json(table, season, through_week, recent)

dir.create("data/defense", showWarnings = FALSE, recursive = TRUE)
write_json(out, file.path("data/defense", paste0("defense_rankings_", season, ".json")), auto_unbox = TRUE, pretty = TRUE, digits = NA)
if (season == max(available)) {
  write_json(out, "data/defense/defense_rankings_current.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)
}
message("Wrote defense rankings for ", nrow(table), " teams (season ", season, ", through week ", through_week, ")")
