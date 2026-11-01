# scripts/validate_data.R
# Schema validation for generated JSON files.
# Usage: Rscript scripts/validate_data.R            (latest season's stats + all news)
#        Rscript scripts/validate_data.R --all      (every season)
#        Rscript scripts/validate_data.R --season 2025
# Exit code: 0 = all pass, 1 = one or more failures.

library(jsonlite)

`%||%` <- function(a, b) if (!is.null(a)) a else b

REQUIRED_COMMON_FIELDS <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "fantasy_points_ppr", "injury_status"
)

# Offense + team defense, plus individual defensive players (IDP) in the Defense files
VALID_POSITIONS <- c("QB", "RB", "WR", "TE", "K", "DEF", "DL", "LB", "CB", "S", "DB")

POSITION_FIELDS <- list(
  QB  = c("completions", "attempts", "passing_yards", "passing_tds",
          "passing_interceptions", "carries", "rushing_yards", "rushing_tds", "fumbles"),
  RB  = c("carries", "rushing_yards", "rushing_tds",
          "receptions", "targets", "receiving_yards", "receiving_tds", "fumbles"),
  WR  = c("receptions", "targets", "receiving_yards", "receiving_tds",
          "carries", "rushing_yards", "rushing_tds", "fumbles"),
  TE  = c("receptions", "targets", "receiving_yards", "receiving_tds",
          "carries", "rushing_yards", "rushing_tds", "fumbles"),
  K   = c("fg_made", "fg_att", "fg_missed", "pat_made", "pat_att", "pat_missed"),
  DEF = c("def_sacks", "def_interceptions", "def_fumbles_forced",
          "def_tds", "def_safeties", "fumble_recovery_opp",
          "passing_yards_allowed", "rushing_yards_allowed")
)

VALID_NEWS_IMPACT <- c("negative", "slightly_negative", "roster_move", "positive", "neutral")

args <- commandArgs(trailingOnly = TRUE)
season_arg <- if ("--season" %in% args) args[which(args == "--season") + 1] else NULL
check_all  <- "--all" %in% args

errors   <- list()
warnings <- list()

add_err  <- function(file, msg) errors[[length(errors) + 1]] <<- paste0("[", file, "] ERROR: ", msg)
add_warn <- function(file, msg) warnings[[length(warnings) + 1]] <<- paste0("[", file, "] WARN:  ", msg)

# =====================
# VALIDATE STATS FILES
# =====================
# Files live at data/Stats/{season} Season/{season} Offense|Defense/player_stats_{season}_weekNN.json
stats_files <- list.files("data/Stats", pattern = "^player_stats_.*\\.json$", full.names = TRUE, recursive = TRUE)
file_seasons <- as.integer(sub("^player_stats_(\\d{4})_.*$", "\\1", basename(stats_files)))
if (!is.null(season_arg)) {
  stats_files <- stats_files[file_seasons == as.integer(season_arg)]
} else if (!check_all && length(stats_files) > 0) {
  stats_files <- stats_files[file_seasons == max(file_seasons, na.rm = TRUE)]
}
cat("Checking", length(stats_files), "stats file(s)\n")

if (length(stats_files) == 0) {
  add_warn("data/Stats", "No player_stats_*.json files found — has the generator been run?")
} else {
  for (f in stats_files) {
    fname <- sub("^data/Stats/", "", f)

    raw <- tryCatch(fromJSON(f, simplifyDataFrame = FALSE), error = function(e) NULL)
    if (is.null(raw)) {
      add_err(fname, "Could not parse JSON")
      next
    }

    # Files are either a plain array of rows or an object of arrays (e.g. {"1": [...], "2": [...]});
    # flatten the second form so both validate the same way.
    if (is.list(raw) && !is.null(names(raw))) {
      if (!all(vapply(raw, function(x) is.list(x) && is.null(names(x)), logical(1)))) {
        add_err(fname, "Expected an array of rows or an object whose values are arrays of rows")
        next
      }
      raw <- unlist(unname(raw), recursive = FALSE)
    }

    if (length(raw) == 0) {
      add_warn(fname, "Array is empty")
      next
    }

    file_season <- as.integer(sub("^player_stats_(\\d{4})_.*$", "\\1", basename(f)))
    file_week   <- as.integer(sub("^.*_week(\\d+)\\.json$", "\\1", basename(f)))

    for (i in seq_along(raw)) {
      player <- raw[[i]]
      prefix <- paste0("record[", i, "]")

      # Common required fields
      missing_common <- setdiff(REQUIRED_COMMON_FIELDS, names(player))
      if (length(missing_common) > 0) {
        add_err(fname, paste0(prefix, " missing fields: ", paste(missing_common, collapse = ", ")))
      }

      # Position must be valid
      pos <- player[["position"]]
      if (!is.null(pos) && !(pos %in% VALID_POSITIONS)) {
        add_err(fname, paste0(prefix, " unknown position: '", pos, "'"))
      }

      # Position-specific required fields
      if (!is.null(pos) && pos %in% names(POSITION_FIELDS)) {
        missing_pos <- setdiff(POSITION_FIELDS[[pos]], names(player))
        if (length(missing_pos) > 0) {
          add_warn(fname, paste0(prefix, " (", pos, ") missing position fields: ",
                                 paste(missing_pos, collapse = ", ")))
        }
      }

      # Season and week must be numeric and match the file name
      season <- player[["season"]]
      if (!is.null(season) && !is.numeric(season)) {
        add_err(fname, paste0(prefix, " 'season' should be numeric, got: ", class(season)))
      } else if (!is.null(season) && !is.na(file_season) && season != file_season) {
        add_err(fname, paste0(prefix, " season ", season, " doesn't match file season ", file_season))
      }
      week <- player[["week"]]
      if (!is.null(week) && !is.numeric(week)) {
        add_err(fname, paste0(prefix, " 'week' should be numeric, got: ", class(week)))
      } else if (!is.null(week) && !is.na(file_week) && week != file_week) {
        add_err(fname, paste0(prefix, " week ", week, " doesn't match file week ", file_week))
      }
    }
  }
}

# =====================
# VALIDATE NEWS FILES (data/news/news_{position}.json)
# =====================
news_files <- list.files("data/news", pattern = "^news_.*\\.json$", full.names = TRUE)
news_files <- news_files[basename(news_files) != "news_meta.json"]  # run timestamp, not articles
news_required <- c("title", "link", "player", "impact")

if (length(news_files) == 0) add_warn("data/news", "No news_*.json files found — has the news generator been run?")

for (news_file in news_files) {
  fname <- basename(news_file)
  raw_news <- tryCatch(fromJSON(news_file, simplifyDataFrame = FALSE), error = function(e) NULL)
  if (is.null(raw_news)) {
    add_err(fname, "Could not parse JSON")
    next
  }
  if (length(raw_news) == 0) next
  if (!is.list(raw_news) || !is.null(names(raw_news))) {
    add_err(fname, "Expected top-level JSON array")
    next
  }

  for (i in seq_along(raw_news)) {
    article <- raw_news[[i]]
    prefix  <- paste0("article[", i, "]")

    missing_fields <- setdiff(news_required, names(article))
    if (length(missing_fields) > 0) {
      add_err(fname, paste0(prefix, " missing fields: ", paste(missing_fields, collapse = ", ")))
    }

    impact <- article[["impact"]]
    if (!is.null(impact) && !(impact %in% VALID_NEWS_IMPACT)) {
      add_warn(fname, paste0(prefix, " unknown impact value: '", impact, "'"))
    }

    for (field in c("published", "summary", "source")) {
      if (is.list(article[[field]])) add_err(fname, paste0(prefix, " '", field, "' is an object instead of text"))
    }
  }

  # Every article sharing one date means RSS dates failed to parse and fell back to "today".
  dates <- unlist(lapply(raw_news, function(a) a[["published"]]))
  if (length(dates) >= 20 && max(table(dates)) / length(dates) > 0.9) {
    add_warn(fname, paste0(round(100 * max(table(dates)) / length(dates)), "% of articles share the date ",
                           names(which.max(table(dates))), " — check RSS date parsing"))
  }
  if (length(raw_news) >= 20 && !any(vapply(raw_news, function(a) !is.null(a[["summary"]]), logical(1)))) {
    add_warn(fname, "No articles have a summary — regenerate with the updated news script")
  }
}

# =====================
# VALIDATE INJURIES (data/injuries/injuries_current.json)
# =====================
injury_file <- "data/injuries/injuries_current.json"
if (!file.exists(injury_file)) {
  add_warn(injury_file, "Not found — has update_injuries.R been run?")
} else {
  inj <- tryCatch(fromJSON(injury_file, simplifyDataFrame = FALSE), error = function(e) NULL)
  if (is.null(inj) || is.null(inj$players)) {
    add_err(injury_file, "Could not parse JSON or missing 'players'")
  } else {
    valid_status <- c("QUESTIONABLE", "DOUBTFUL", "OUT", "IR", "PUP", "SUSPENDED", "NOT_ACTIVE", "")
    for (i in seq_along(inj$players)) {
      p <- inj$players[[i]]
      missing <- setdiff(c("player_id", "full_name", "team", "position", "status"), names(p))
      if (length(missing) > 0) add_err(injury_file, paste0("player[", i, "] missing: ", paste(missing, collapse = ", ")))
      if (!is.null(p$status) && !(p$status %in% valid_status)) add_warn(injury_file, paste0("player[", i, "] unknown status: ", p$status))
    }
    age_hours <- as.numeric(difftime(Sys.time(), as.POSIXct(sub("Z$", "", inj$generated_at %||% ""), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"), units = "hours"))
    if (!is.na(age_hours) && age_hours > 36) add_warn(injury_file, paste0("Last updated ", round(age_hours), " hours ago"))
  }
}

# =====================
# FILENAME CONVENTION CHECK
# =====================
bad_names <- stats_files[grepl("week [0-9]", stats_files)]
if (length(bad_names) > 0) {
  add_err("data/", paste0(
    "Files with spaces in week number (rename to zero-padded format): ",
    paste(basename(bad_names), collapse = ", ")
  ))
}

# =====================
# REPORT
# =====================
cat("\n===== nfl-data validation =====\n")

if (length(warnings) > 0) {
  cat("\nWarnings:\n")
  for (w in warnings) cat(" ", w, "\n")
}

if (length(errors) > 0) {
  cat("\nErrors:\n")
  for (e in errors) cat(" ", e, "\n")
  cat("\n❌ Validation FAILED —", length(errors), "error(s),", length(warnings), "warning(s)\n\n")
  quit(status = 1)
} else {
  cat("\n✅ Validation PASSED —", length(warnings), "warning(s)\n\n")
}
