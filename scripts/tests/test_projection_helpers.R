# Run from the repo root: Rscript scripts/tests/test_projection_helpers.R
suppressPackageStartupMessages(library(jsonlite))
source("scripts/projection_helpers.R")

failures <- 0
check <- function(label, got, want) {
  if (identical(got, want)) cat("ok:", label, "\n") else {
    failures <<- failures + 1
    cat("FAIL:", label, "\n  got: ", deparse(got), "\n  want:", deparse(want), "\n")
  }
}

# The newer endpoint's row shape (trimmed from a real response).
newer <- fromJSON('[
  {"player_id": "6130", "team": "NYG", "opponent": "WAS", "week": 10,
   "player": {"first_name": "Devin", "last_name": "Singletary", "position": "RB"},
   "stats": {"pts_ppr": 8.2, "pts_half_ppr": 7.3, "pts_std": 6.4, "rush_att": 7.51, "rush_yd": 29, "rec": 1.8, "adp_dd_ppr": 120, "rec_0_4": 0.4}},
  {"player_id": "999", "team": "KC", "player": {"first_name": "No", "last_name": "Points", "position": "WR"}, "stats": {"pts_ppr": 0}},
  {"player_id": "KC", "team": "KC", "opponent": "DEN", "player": {"first_name": "Kansas City", "last_name": "Chiefs", "position": "DEF"},
   "stats": {"pts_ppr": 7.1, "def_sack": 2.4, "def_int": 0.8}}
]', simplifyVector = FALSE)

fake_get <- function(url) if (grepl("api.sleeper.com", url, fixed = TRUE)) newer else NULL
rows <- fetch_week_projections(2026, 10, fake_get)
check("newer endpoint used", length(rows), 3L)

slim <- slim_projections(rows)
check("players without projected points left out", sort(names(slim)), c("6130", "KC"))
check("name", slim[["6130"]]$name, "Devin Singletary")
check("opponent", slim[["6130"]]$opp, "WAS")
check("ppr / half / std", c(slim[["6130"]]$ppr, slim[["6130"]]$half, slim[["6130"]]$std), c(8.2, 7.3, 6.4))
check("the full stat line minus ranks / ADP / pre-scored totals", sort(names(slim[["6130"]]$stats)), c("rec", "rec_0_4", "rush_att", "rush_yd"))
check("defense stats", slim[["KC"]]$stats$def_sack, 2.4)
dpts <- slim_projections(list(list(player_id = "NE", stats = list(pts_ppr = 7.6, pts_std = 7.6, pts_allow_14_20 = 1, sack = 2.5))))
check("points-allowed buckets kept, pre-scored totals dropped", sort(names(dpts[["NE"]]$stats)), c("pts_allow_14_20", "sack"))

# Classic /v1 fallback: { player_id: stats }.
classic <- list("4046" = list(pts_ppr = 18.4, pts_half_ppr = 18.4, pts_std = 18.4, pass_yd = 251.3))
fallback_get <- function(url) if (grepl("/v1/projections", url, fixed = TRUE)) classic else NULL
rows2 <- fetch_week_projections(2026, 10, fallback_get)
check("falls back to the classic endpoint", rows2[[1]]$player_id, "4046")
check("classic rows slim too", slim_projections(rows2)[["4046"]]$stats$pass_yd, 251.3)
check("nothing from either endpoint", length(fetch_week_projections(2026, 10, function(u) NULL)), 0L)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll projection helper tests passed\n")
