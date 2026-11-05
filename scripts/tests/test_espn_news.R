# Run from the repo root: Rscript scripts/tests/test_espn_news.R
# ESPN news parsing and matching to players (no network: a fake fetcher returns fixtures).
suppressPackageStartupMessages(library(jsonlite))
source("scripts/news_helpers.R")
source("scripts/espn_news.R")

failures <- 0
check <- function(label, got, want) {
  if (identical(got, want)) cat("ok:", label, "\n") else {
    failures <<- failures + 1
    cat("FAIL:", label, "\n  got: ", deparse(got), "\n  want:", deparse(want), "\n")
  }
}

feed <- fromJSON('{
  "articles": [
    {"id": 1, "type": "Story", "headline": "Bhayshul Tuten not spotted at Jaguars practice",
     "description": "The rookie RB missed practice Wednesday.", "published": "2026-11-04T18:22:31Z",
     "images": [{"type": "header", "url": "https://a.espncdn.com/photo/2026/1104/tuten.jpg"}, {"type": "inline", "url": "https://x/y.jpg"}],
     "links": {"web": {"href": "https://www.espn.com/nfl/story/_/id/1/tuten"}},
     "categories": [{"type": "athlete", "description": "Bhayshul Tuten", "athleteId": 5}, {"type": "team", "description": "Jacksonville Jaguars"}]},
    {"id": 2, "type": "Story", "headline": "Jaguars film study: what the offense needs",
     "published": "2026-11-03T10:00:00Z", "images": [{"url": "http://a.espncdn.com/photo/film.jpg"}],
     "links": {"web": {"href": "https://www.espn.com/nfl/story/_/id/2/film"}},
     "categories": [{"type": "athlete", "description": "Bhayshul Tuten"}, {"type": "athlete", "description": "Travis Etienne Jr."}]},
    {"id": 3, "type": "Media", "headline": "Video: highlights", "links": {"web": {"href": "https://espn.com/v"}}},
    {"id": 4, "type": "Story", "headline": "Old story about Bhayshul Tuten", "published": "2025-08-01T10:00:00Z",
     "links": {"web": {"href": "https://www.espn.com/nfl/story/_/id/4/old"}},
     "categories": [{"type": "athlete", "description": "Bhayshul Tuten"}]}
  ]
}', simplifyVector = FALSE)

parsed <- parse_espn_articles(feed)
check("media skipped", length(parsed), 3L)
check("header image picked", parsed[[1]]$image, "https://a.espncdn.com/photo/2026/1104/tuten.jpg")
check("image forced to https", parsed[[2]]$image, "https://a.espncdn.com/photo/film.jpg")
check("athletes tagged", parsed[[2]]$athletes, c("Bhayshul Tuten", "Travis Etienne Jr."))
check("published date", parsed[[1]]$published, "2026-11-04")

# Two team feeds returning the same story: deduplicated.
all <- fetch_espn_news(c(30, 34), get_json = function(url) feed, delay = 0)
check("deduplicated across feeds", length(all), 3L)

by <- espn_articles_by_player(all)
mine <- espn_player_articles(by, "Bhayshul Tuten", "2026-09-01")
check("only stories about him (named, or only him tagged), this season", length(mine), 1L)
check("article has picture + real link", c(mine[[1]]$image, mine[[1]]$url), c("https://a.espncdn.com/photo/2026/1104/tuten.jpg", "https://www.espn.com/nfl/story/_/id/1/tuten"))
check("source", mine[[1]]$source, "ESPN")
check("classified", is.character(mine[[1]]$category), TRUE)

# Same story from Google (no image) and ESPN (image): the ESPN one is kept.
google <- list(title = "Bhayshul Tuten not spotted at Jaguars practice", link = "https://news.google.com/rss/articles/x", published = "2026-11-04", player = "Bhayshul Tuten", category = "injury")
picked <- select_diverse(list(google, mine[[1]]), 3)
check("picture version wins a duplicate story", picked[[1]]$source, "ESPN")
check("duplicate dropped", length(picked), 1L)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll ESPN news tests passed\n")
