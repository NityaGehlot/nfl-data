# scripts/validate_data.R
# Schema validation for generated JSON files.
# Usage: Rscript scripts/validate_data.R
# Exit code: 0 = all pass, 1 = one or more failures.

library(jsonlite)

REQUIRED_COMMON_FIELDS <- c(
  "season", "week", "player_id", "player_name",
  "position", "team", "opponent_team",
  "fantasy_points_ppr", "injury_status"
)

VALID_POSITIONS <- c("QB", "RB", "WR", "TE", "K", "DEF")

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

errors   <- list()
warnings <- list()

add_err  <- function(file, msg) errors[[length(errors) + 1]] <<- paste0("[", file, "] ERROR: ", msg)
add_warn <- function(file, msg) warnings[[length(warnings) + 1]] <<- paste0("[", file, "] WARN:  ", msg)

# =====================
# VALIDATE STATS FILES
# =====================
stats_files <- list.files("data", pattern = "^player_stats_.*\\.json$", full.names = TRUE)

if (length(stats_files) == 0) {
  add_warn("data/", "No player_stats_*.json files found — has the generator been run?")
} else {
  for (f in stats_files) {
    fname <- basename(f)

    raw <- tryCatch(fromJSON(f, simplifyDataFrame = FALSE), error = function(e) NULL)
    if (is.null(raw)) {
      add_err(fname, "Could not parse JSON")
      next
    }

    # Must be a top-level array
    if (!is.list(raw) || !is.null(names(raw))) {
      add_err(fname, paste0(
        "Expected top-level JSON array but got ",
        if (!is.null(names(raw))) "named object" else class(raw)
      ))
      next
    }

    if (length(raw) == 0) {
      add_warn(fname, "Array is empty")
      next
    }

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

      # Season and week must be numeric
      if (!is.null(player[["season"]]) && !is.numeric(player[["season"]])) {
        add_err(fname, paste0(prefix, " 'season' should be numeric, got: ", class(player[["season"]])))
      }
      if (!is.null(player[["week"]]) && !is.numeric(player[["week"]])) {
        add_err(fname, paste0(prefix, " 'week' should be numeric, got: ", class(player[["week"]])))
      }
    }
  }
}

# =====================
# VALIDATE NEWS FILE
# =====================
news_file <- "data/nfl_news.json"

if (!file.exists(news_file)) {
  add_warn(news_file, "nfl_news.json not found — has the news generator been run?")
} else {
  raw_news <- tryCatch(fromJSON(news_file, simplifyDataFrame = FALSE), error = function(e) NULL)
  if (is.null(raw_news)) {
    add_err(news_file, "Could not parse JSON")
  } else if (!is.list(raw_news) || !is.null(names(raw_news))) {
    add_err(news_file, "Expected top-level JSON array")
  } else {
    news_required <- c("title", "summary", "link", "published", "source", "impact")
    for (i in seq_along(raw_news)) {
      article <- raw_news[[i]]
      prefix  <- paste0("article[", i, "]")

      missing_fields <- setdiff(news_required, names(article))
      if (length(missing_fields) > 0) {
        add_err(news_file, paste0(prefix, " missing fields: ", paste(missing_fields, collapse = ", ")))
      }

      impact <- article[["impact"]]
      if (!is.null(impact) && !(impact %in% VALID_NEWS_IMPACT)) {
        add_warn(news_file, paste0(prefix, " unknown impact value: '", impact, "'"))
      }
    }
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
