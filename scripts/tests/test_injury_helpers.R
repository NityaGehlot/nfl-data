# Run from the repo root: Rscript scripts/tests/test_injury_helpers.R
source("scripts/injury_helpers.R")

failures <- 0
check <- function(desc, actual, expected) {
  if (!identical(actual, expected)) {
    failures <<- failures + 1
    cat("FAIL:", desc, "\n  expected:", format(expected), "\n  actual:  ", format(actual), "\n")
  } else cat("ok:", desc, "\n")
}

check("IR", normalize_status("IR"), "IR")
check("Sus", normalize_status("Sus"), "SUSPENDED")
check("Questionable", normalize_status("Questionable"), "QUESTIONABLE")
check("blank/NULL", normalize_status(NULL), "")
check("NA from Sleeper means not active", normalize_status("NA"), "NOT_ACTIVE")
check("DNP", normalize_practice("Did Not Participate In Practice"), "DNP")
check("LP", normalize_practice("Limited Participation in Practice"), "LP")
check("FP", normalize_practice("Full Participation in Practice"), "FP")
check("weekday in Eastern time", weekday_label("2026-10-29T02:00:00Z"), "Wed") # 10pm Wed ET
check("Eastern date", eastern_date("2026-10-29T02:00:00Z"), "2026-10-28")

sleeper <- list(
  "4881" = list(full_name = "Romeo Doubs", position = "WR", team = "GB", injury_status = "IR",
                injury_body_part = "Hamstring", gsis_id = "00-0037000", injury_start_date = "2026-10-20"),
  "100" = list(full_name = "Healthy Guy", position = "RB", team = "KC", injury_status = NULL, gsis_id = "00-0000100"),
  "200" = list(full_name = "Practice Guy", position = "WR", team = "BUF", injury_status = NULL, gsis_id = "00-0000200"),
  "300" = list(full_name = "Q Guy", position = "TE", team = "DAL", injury_status = "Questionable", gsis_id = "00-0000300"),
  "400" = list(full_name = "Lineman", position = "OT", team = "DAL", injury_status = "Out"),
  "500" = list(full_name = "Free Agent", position = "WR", team = NULL, injury_status = "Out")
)
report <- data.frame(
  gsis_id = c("00-0000200", "00-0000300"),
  report_status = c("", "Questionable"),
  practice_status = c("Limited Participation in Practice", "Did Not Participate In Practice"),
  report_primary_injury = c("Ankle", "Knee"),
  practice_primary_injury = c("Ankle", "Knee"),
  stringsAsFactors = FALSE
)
rows <- build_injury_rows(sleeper, report)
ids <- vapply(rows, function(r) r$player_id, character(1))
check("includes IR player even with no report (Romeo Doubs case)", "4881" %in% ids, TRUE)
check("includes practice-only listing", "200" %in% ids, TRUE)
check("skips healthy, non-fantasy positions and free agents", any(c("100", "400", "500") %in% ids), FALSE)
check("most severe first", ids[1], "4881")
doubs <- rows[[1]]
check("IR status", doubs$status, "IR")
check("body part", doubs$injury, "Hamstring")
q <- rows[[which(ids == "300")]]
check("game status from report", q$game_status, "QUESTIONABLE")
check("practice status from report", q$practice_status, "DNP")
check("practice-only player has LP", rows[[which(ids == "200")]]$practice_status, "LP")

h <- merge_history(list(), rows, "2026-10-28T21:00:00Z") # Wed
h <- merge_history(h, rows, "2026-10-28T23:30:00Z")      # Wed again -> replaces
rows[[which(ids == "300")]]$practice_status <- "LP"
h <- merge_history(h, rows, "2026-10-29T21:00:00Z")      # Thu
check("one entry per day", length(h[["300"]]), 2L)
check("days in order", vapply(h[["300"]], function(e) e$day, character(1)), c("Wed", "Thu"))
check("Thursday practice updated", h[["300"]][[2]]$practice_status, "LP")
withHist <- attach_history(rows, h)
check("history attached", length(withHist[[which(ids == "300")]]$history), 2L)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll injury helper tests passed\n")
