# Run from the repo root: Rscript scripts/tests/test_news_helpers.R
source("scripts/news_helpers.R")

failures <- 0
check <- function(desc, actual, expected) {
  ok <- identical(actual, expected)
  if (!ok) {
    failures <<- failures + 1
    cat("FAIL:", desc, "\n  expected:", format(expected), "\n  actual:  ", format(actual), "\n")
  } else {
    cat("ok:", desc, "\n")
  }
}

# Dates: RSS "GMT" dates used to fail and fall back to today's date
check("parses RSS GMT date", parse_rss_date("Tue, 28 Oct 2026 14:05:00 GMT"), as.Date("2026-10-28"))
check("parses numeric offset", parse_rss_date("Tue, 28 Oct 2026 23:05:00 +0000"), as.Date("2026-10-28"))
check("parses ISO date", parse_rss_date("2026-10-20"), as.Date("2026-10-20"))
check("bad date is NA", is.na(parse_rss_date("not a date")), TRUE)

check("season start in season", current_season_start(as.Date("2026-10-31")), as.Date("2026-09-01"))
check("season start in playoffs", current_season_start(as.Date("2027-01-20")), as.Date("2026-09-01"))

# Title/source split
s <- split_title_source("CeeDee Lamb says Cowboys have a Cardinals boogeyman - CBS Sports")
check("headline split", s$headline, "CeeDee Lamb says Cowboys have a Cardinals boogeyman")
check("source split", s$source, "CBS Sports")
check("no source", split_title_source("Just a headline")$source, NA_character_)
check("keeps hyphenated words", split_title_source("Amon-Ra St. Brown scores twice - ESPN")$headline, "Amon-Ra St. Brown scores twice")

# Relevance
check("full name match", mentions_player("Ja'Marr Chase dominates Steelers", "Ja'Marr Chase"), TRUE)
check("distinctive last name", mentions_player("Bengals: Chase limited at practice", "Ja'Marr Chase"), TRUE)
check("common last name alone isn't enough", mentions_player("Allen leads Bills", "Josh Allen"), FALSE)
check("other player isn't a match", mentions_player("Week 8 rankings: Puka Nacua tops list", "CeeDee Lamb"), FALSE)
check("suffix ignored", mentions_player("Marvin Harrison catches two TDs", "Marvin Harrison Jr."), TRUE)

# Classification — the old keyword check matched "out" inside "mouth" and "ir" inside "their"
c1 <- classify_headline("CeeDee Lamb says the Cardinals have punched us in the mouth every single time", "CeeDee Lamb")
check("'mouth' is not an injury", c1$impact, "neutral")
c2 <- classify_headline("Bills' offense finds their rhythm", "Josh Allen")
check("'their' is not IR", c2$category == "injury", FALSE)
check("ruled out", classify_headline("Saquon Barkley ruled out vs. Giants with ankle injury")$impact, "negative")
check("injury part in summary", grepl("ankle", classify_headline("Saquon Barkley ruled out vs. Giants with ankle injury", "Saquon Barkley")$summary), TRUE)
check("questionable", classify_headline("Bijan Robinson listed questionable with hamstring")$impact, "slightly_negative")
check("season-ending", classify_headline("Mark Andrews suffers torn ACL, out for season")$impact, "negative")
check("return to practice", classify_headline("Christian McCaffrey returns to practice")$impact, "positive")
check("trade", classify_headline("Raiders trade Davante Adams to Jets")$category, "transaction")
check("released", classify_headline("Bears released veteran running back")$impact, "roster_move")
check("'running back' is not a back injury", classify_headline("Jets sign running back off practice squad")$category, "transaction")
check("back injury", classify_headline("Joe Burrow has back spasms")$category, "injury")
check("multi-player headline uses this player's clause",
      classify_headline("Ladd McConkey set to play, Derwin James out vs. Rams", "Ladd McConkey")$impact, "positive")
check("and the other player's clause",
      classify_headline("Ladd McConkey set to play, Derwin James out vs. Rams", "Derwin James")$impact, "negative")
check("big game", classify_headline("Jahmyr Gibbs explodes for three touchdowns")$impact, "positive")
check("benched", classify_headline("Bryce Young benched for Andy Dalton")$impact, "negative")
check("suspension", classify_headline("Rashee Rice facing suspension")$category, "suspension")
check("fantasy advice", classify_headline("Week 9 start/sit: Trust Garrett Wilson")$category, "fantasy_advice")

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll news helper tests passed\n")
