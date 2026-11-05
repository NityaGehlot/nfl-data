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
check("official report noted", q$status_note, "From this week's official injury report.")
carry <- build_injury_rows(list("9" = list(full_name = "Carry Over", position = "RB", team = "JAX", injury_status = "Questionable", gsis_id = "00-9")), report)
check("status carried over from last game is labeled", grepl("^Carried over from his last game", carry[[1]]$status_note), TRUE)
check("IR has no carry-over note", is.null(rows[[1]]$status_note), TRUE)

h <- merge_history(list(), rows, "2026-10-28T21:00:00Z") # Wed
h <- merge_history(h, rows, "2026-10-28T23:30:00Z")      # Wed again -> replaces
rows[[which(ids == "300")]]$practice_status <- "LP"
h <- merge_history(h, rows, "2026-10-29T21:00:00Z")      # Thu
check("one entry per day", length(h[["300"]]), 2L)
check("days in order", vapply(h[["300"]], function(e) e$day, character(1)), c("Wed", "Thu"))
check("Thursday practice updated", h[["300"]][[2]]$practice_status, "LP")
withHist <- attach_history(rows, h)
check("history attached", length(withHist[[which(ids == "300")]]$history), 2L)

# News that comes out before the official report (the Bhayshul Tuten case)
check("not spotted = DNP", practice_from_headline("Tuten (ankle/foot) not spotted on Tuesday"), "DNP")
check("remains out of practice = DNP", practice_from_headline("Bhayshul Tuten remains out of practice for the Jaguars"), "DNP")
check("limited = LP", practice_from_headline("Bhayshul Tuten limited in practice"), "LP")
check("back at practice = FP", practice_from_headline("Bhayshul Tuten back at practice"), "FP")
check("no practice info", practice_from_headline("Bhayshul Tuten bringing the explosives"), "")
news <- list(
  list(title = "Fantasy Football: Bhayshul Tuten bringing the explosives", player = "Bhayshul Tuten", published = "2026-11-02", category = "fantasy_advice"),
  list(title = "Tuten (ankle/foot) not spotted on Tuesday", player = "Bhayshul Tuten", published = "2026-11-03", category = "injury", source = "NBC Sports"),
  list(title = "Old injury note", player = "Bhayshul Tuten", published = "2026-10-01", category = "injury")
)
latest <- latest_injury_news(news, "2026-11-03")
check("latest injury headline picked", latest[["bhayshul tuten"]]$title, "Tuten (ankle/foot) not spotted on Tuesday")
check("practice read from it", latest[["bhayshul tuten"]]$practice, "DNP")
tuten <- list(list(player_id = "1", full_name = "Bhayshul Tuten", status = "QUESTIONABLE"))
with_news <- attach_news(tuten, latest, "2026-11-03")
check("news attached", with_news[[1]]$latest_news$source, "NBC Sports")
check("practice from news when no official report", with_news[[1]]$practice_status, "DNP")
check("marked as from news", with_news[[1]]$practice_source, "news")
official <- attach_news(list(list(player_id = "1", full_name = "Bhayshul Tuten", status = "QUESTIONABLE", practice_status = "LP")), latest, "2026-11-03")
check("official report wins over news", official[[1]]$practice_status, "LP")
stale <- attach_news(tuten, latest, "2026-11-06")
check("old news doesn't fill today's practice", is.null(stale[[1]]$practice_status), TRUE)
h2 <- merge_history(list(), with_news, "2026-11-03T21:00:00Z")
check("history keeps the news source", h2[["1"]][[1]]$practice_source, "news")

# Only headlines about HIS injury count (round 8)
check("game story mentioning injuries is not injury news",
  is_injury_headline("Tyler Allgeier answered the call for a career day when injuries mounted for the Arizona Cardinals", "Tyler Allgeier", "general"), FALSE)
check("someone else's injury in the same headline doesn't count",
  is_injury_headline("Fantasy Football Injury Report: Jeremiah Love Placed on IR, Tyler Allgeier Becomes RB1", "Tyler Allgeier", "fantasy_advice"), FALSE)
check("his practice news counts", is_injury_headline("Bhayshul Tuten not spotted at Jaguars practice", "Bhayshul Tuten"), TRUE)
check("(ankle) tag counts", is_injury_headline("Jaguars RB Bhayshul Tuten (ankle/foot) questionable for Sunday", "Bhayshul Tuten"), TRUE)
check("filed as injury without naming him counts", is_injury_headline("Jaguars RB status for Sunday", "Bhayshul Tuten", "injury"), TRUE)
mixed <- latest_injury_news(list(
  list(title = "Tyler Allgeier answered the call for a career day when injuries mounted", player = "Tyler Allgeier", published = "2026-11-03", category = "general")
), "2026-11-03")
check("no injury news picked for a game story", is.null(mixed[["tyler allgeier"]]), TRUE)

# Past weeks from nflverse's official report (backfill, round 9)
check("practice codes", practice_code(c("Did Not Participate In Practice", "Limited Participation in Practice", "Full Participation in Practice", NA)), c("DNP", "LP", "FP", NA))
season_rows <- data.frame(
  season = 2025, week = c(3, 3, 3, 4),
  gsis_id = c("00-1", "00-1", "00-2", "00-1"),
  full_name = c("A Back", "A Back", "B Wide", "A Back"), team = c("JAX", "JAX", "KC", "JAX"), position = c("RB", "RB", "WR", "RB"),
  report_primary_injury = c("Ankle", "Ankle", NA, NA), practice_primary_injury = c("Ankle", "Ankle", "Rest", NA),
  report_status = c("Questionable", "Out", NA, NA),
  practice_status = c("Limited Participation in Practice", "Did Not Participate In Practice", "Did Not Participate In Practice", "Full Participation in Practice"),
  date_modified = c("2025-09-19T16:00:00Z", "2025-09-20T16:00:00Z", "2025-09-19T16:00:00Z", "2025-09-26T16:00:00Z"),
  stringsAsFactors = FALSE
)
weeks <- injury_report_weeks(season_rows)
check("weeks split", sort(names(weeks)), c("3", "4"))
check("latest report wins", weeks[["3"]][["00-1"]][[1]]$game_status, "OUT")
check("practice from the latest report", weeks[["3"]][["00-1"]][[1]]$practice_status, "DNP")
check("name kept for matching", weeks[["3"]][["00-1"]][[1]]$name, "A Back")
check("practice-only player has no game status", is.null(weeks[["3"]][["00-2"]][[1]]$game_status), TRUE)
check("injury falls back to practice injury", weeks[["3"]][["00-2"]][[1]]$injury, "Rest")
check("cleared the next week", weeks[["4"]][["00-1"]][[1]]$practice_status, "FP")
check("day of the report", weeks[["3"]][["00-1"]][[1]]$day, "Sat")

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll injury helper tests passed\n")
