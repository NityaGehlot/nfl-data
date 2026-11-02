# scripts/defense_helpers.R
# How tough each NFL defense is: yards, touchdowns and fantasy points it allows per game,
# overall and to each position, with ranks (1 = toughest, 32 = easiest).
# Built from the offense stats files: every offensive player row says who the opponent was,
# so adding up what a defense's opponents produced gives what that defense allowed.

suppressPackageStartupMessages({
  library(dplyr)
})

DEF_POSITIONS <- c("QB", "RB", "WR", "TE", "K")

num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[is.na(x)] <- 0
  x
}

# rows: data frame of offensive player-week rows with team, opponent_team, week, position and stats.
# Returns one row per defense with per-game numbers and ranks.
compute_defense_table <- function(rows) {
  rows <- rows %>%
    mutate(
      opponent_team = toupper(trimws(as.character(opponent_team))),
      position = toupper(as.character(position)),
      week = as.integer(week)
    ) %>%
    filter(nzchar(opponent_team), !is.na(week))

  if (nrow(rows) == 0) return(data.frame())

  for (col in c("passing_yards", "passing_tds", "rushing_yards", "rushing_tds",
                "receiving_yards", "receiving_tds", "fantasy_points_ppr")) {
    rows[[col]] <- if (col %in% names(rows)) num(rows[[col]]) else 0
  }

  games <- rows %>% group_by(defense = opponent_team) %>% summarise(games = n_distinct(week), .groups = "drop")

  totals <- rows %>%
    group_by(defense = opponent_team) %>%
    summarise(
      pass_yds = sum(passing_yards[position == "QB"]),
      pass_td = sum(passing_tds[position == "QB"]),
      rush_yds = sum(rushing_yards),
      rush_td = sum(rushing_tds),
      .groups = "drop"
    )

  fpa <- rows %>%
    filter(position %in% DEF_POSITIONS) %>%
    group_by(defense = opponent_team, position) %>%
    summarise(points = sum(fantasy_points_ppr), .groups = "drop")

  table <- games %>% left_join(totals, by = "defense")
  for (pos in DEF_POSITIONS) {
    pos_points <- fpa %>% filter(position == pos) %>% select(defense, points)
    names(pos_points)[2] <- paste0("fpa_", tolower(pos))
    table <- table %>% left_join(pos_points, by = "defense")
  }

  per_game <- function(x, g) round(ifelse(g > 0, x / g, 0), 1)
  table <- table %>%
    mutate(across(c(pass_yds, pass_td, rush_yds, rush_td, starts_with("fpa_")), ~ coalesce(.x, 0))) %>%
    mutate(
      pass_yds_pg = per_game(pass_yds, games),
      pass_td_pg = per_game(pass_td, games),
      rush_yds_pg = per_game(rush_yds, games),
      rush_td_pg = per_game(rush_td, games),
      total_yds_pg = round(pass_yds_pg + rush_yds_pg, 1),
      across(starts_with("fpa_"), ~ per_game(.x, games), .names = "{.col}_pg")
    )

  # Rank 1 = allows the least (toughest matchup).
  rank_low <- function(x) as.integer(rank(x, ties.method = "min"))
  table %>%
    mutate(
      rank_pass = rank_low(pass_yds_pg),
      rank_rush = rank_low(rush_yds_pg),
      rank_overall = rank_low(total_yds_pg),
      rank_qb = rank_low(fpa_qb_pg),
      rank_rb = rank_low(fpa_rb_pg),
      rank_wr = rank_low(fpa_wr_pg),
      rank_te = rank_low(fpa_te_pg),
      rank_k = rank_low(fpa_k_pg)
    ) %>%
    arrange(rank_overall)
}

# "tough" for the best third of defenses, "easy" for the worst third.
matchup_tier <- function(rank, teams = 32) {
  ifelse(rank <= ceiling(teams / 3), "tough", ifelse(rank > teams - ceiling(teams / 3), "easy", "average"))
}

# Turn the table into the JSON shape the app reads.
defense_json <- function(table, season, through_week, recent = NULL) {
  n <- nrow(table)
  one <- function(r, tbl) {
    list(
      games = r$games,
      pass_yds_allowed = r$pass_yds_pg,
      pass_td_allowed = r$pass_td_pg,
      rush_yds_allowed = r$rush_yds_pg,
      rush_td_allowed = r$rush_td_pg,
      total_yds_allowed = r$total_yds_pg,
      fantasy_points_allowed = list(QB = r$fpa_qb_pg, RB = r$fpa_rb_pg, WR = r$fpa_wr_pg, TE = r$fpa_te_pg, K = r$fpa_k_pg),
      ranks = list(pass = r$rank_pass, rush = r$rank_rush, overall = r$rank_overall,
                   QB = r$rank_qb, RB = r$rank_rb, WR = r$rank_wr, TE = r$rank_te, K = r$rank_k),
      tiers = list(pass = matchup_tier(r$rank_pass, n), rush = matchup_tier(r$rank_rush, n),
                   overall = matchup_tier(r$rank_overall, n))
    )
  }
  teams <- list()
  for (i in seq_len(n)) {
    r <- table[i, ]
    entry <- one(r, table)
    if (!is.null(recent) && r$defense %in% recent$defense) {
      rr <- recent[recent$defense == r$defense, ]
      entry$last_4 <- list(
        games = rr$games,
        pass_yds_allowed = rr$pass_yds_pg,
        rush_yds_allowed = rr$rush_yds_pg,
        fantasy_points_allowed = list(QB = rr$fpa_qb_pg, RB = rr$fpa_rb_pg, WR = rr$fpa_wr_pg, TE = rr$fpa_te_pg, K = rr$fpa_k_pg)
      )
    }
    teams[[r$defense]] <- entry
  }
  list(
    season = season,
    through_week = through_week,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    rank_note = "Rank 1 = allows the fewest yards/points (toughest matchup); rank 32 = allows the most (easiest matchup).",
    teams = teams
  )
}
