# Run from the repo root: Rscript scripts/tests/test_defense_helpers.R
source("scripts/defense_helpers.R")

failures <- 0
check <- function(desc, actual, expected) {
  if (!isTRUE(all.equal(actual, expected))) {
    failures <<- failures + 1
    cat("FAIL:", desc, "\n  expected:", format(expected), "\n  actual:  ", format(actual), "\n")
  } else cat("ok:", desc, "\n")
}

rows <- data.frame(
  week = c(1, 1, 1, 2, 2, 1, 1),
  team = c("KC", "KC", "KC", "KC", "KC", "BUF", "BUF"),
  opponent_team = c("BUF", "BUF", "BUF", "MIA", "MIA", "KC", "KC"),
  position = c("QB", "RB", "WR", "QB", "WR", "QB", "WR"),
  passing_yards = c(300, 0, 0, 200, 0, 150, 0),
  passing_tds = c(3, 0, 0, 1, 0, 1, 0),
  rushing_yards = c(10, 100, 0, 0, 0, 20, 0),
  rushing_tds = c(0, 1, 0, 0, 0, 0, 0),
  receiving_yards = c(0, 20, 120, 0, 90, 0, 60),
  receiving_tds = c(0, 0, 2, 0, 1, 0, 1),
  fantasy_points_ppr = c(25, 18, 26, 12, 15, 9, 12),
  stringsAsFactors = FALSE
)
t <- compute_defense_table(rows)
buf <- t[t$defense == "BUF", ]
kc <- t[t$defense == "KC", ]
check("BUF allowed 300 pass yds in 1 game", buf$pass_yds_pg, 300)
check("BUF allowed 110 rush yds", buf$rush_yds_pg, 110)
check("BUF allowed 26 WR points", buf$fpa_wr_pg, 26)
check("KC allowed 150 pass yds", kc$pass_yds_pg, 150)
check("KC is the toughest pass defense (rank 1)", kc$rank_pass, 1L)
check("BUF is the easiest pass defense", buf$rank_pass, 3L)
check("games counted per defense", t[t$defense == "MIA", ]$games, 1L)
check("tiers", matchup_tier(c(1, 16, 32)), c("tough", "average", "easy"))
j <- defense_json(t, 2026, 2)
check("json has every defense", sort(names(j$teams)), c("BUF", "KC", "MIA"))
check("json ranks", j$teams$KC$ranks$pass, 1L)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll defense helper tests passed\n")
