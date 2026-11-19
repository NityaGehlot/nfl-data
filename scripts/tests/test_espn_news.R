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

# ESPN videos with a cover are kept and marked as video
vfeed <- fromJSON('{"articles":[{"id":9,"type":"Media","headline":"Bhayshul Tuten highlights","published":"2026-11-04T10:00:00Z",
  "images":[{"url":"https://a.espncdn.com/video/cover.jpg"}],"links":{"web":{"href":"https://www.espn.com/video/clip?id=9"}},
  "categories":[{"type":"athlete","description":"Bhayshul Tuten"}]}]}', simplifyVector = FALSE)
vp <- parse_espn_articles(vfeed)
check("ESPN video kept", length(vp), 1L)
check("ESPN video marked", isTRUE(vp[[1]]$video), TRUE)
check("ESPN video flag reaches the player article", isTRUE(espn_player_articles(espn_articles_by_player(vp), "Bhayshul Tuten")[[1]]$video), TRUE)

# ESPN per-player fantasy news: stories with a picture and a link; RotoWire blurbs skipped
pfeed <- fromJSON('{"feed":[
  {"id":11,"type":"Story","headline":"Gibbs set for bigger role","published":"2026-11-15T10:00:00Z",
   "images":[{"url":"https://a.espncdn.com/photo/gibbs.jpg"}],"links":{"web":{"href":"https://www.espn.com/fantasy/story/_/id/11"}}},
  {"id":12,"type":"Rotowire","headline":"Gibbs rushed 14 times","published":"2026-11-16T10:00:00Z"},
  {"id":13,"type":"Story","headline":"No picture story","published":"2026-11-14T10:00:00Z","links":{"web":{"href":"https://www.espn.com/x"}}}]}', simplifyVector = FALSE)
pn <- parse_espn_player_news(pfeed, "Jahmyr Gibbs")
check("player feed: only stories with picture + link", length(pn), 1L)
check("player feed: tagged with the player", pn[[1]]$athletes, "Jahmyr Gibbs")
check("player feed: image", pn[[1]]$image, "https://a.espncdn.com/photo/gibbs.jpg")
check("player feed reaches the player's news", espn_player_articles(espn_articles_by_player(pn), "Jahmyr Gibbs")[[1]]$url, "https://www.espn.com/fantasy/story/_/id/11")

ids <- load_espn_ids(function(url) data.frame(sleeper_id = c("9221", NA), espn_id = c("4429795", "1"), stringsAsFactors = FALSE))
check("espn id map", ids[["9221"]], "4429795")
check("espn id map download fails -> empty", length(load_espn_ids(function(url) stop("offline"))), 0L)

players <- list(list(player_id = "9221", full_name = "Jahmyr Gibbs", team = "DET", position = "RB"),
                list(player_id = "1", full_name = "Free Agent", team = NULL, position = "RB"),
                list(player_id = "2", full_name = "Lineman", team = "DET", position = "OT"))
asked <- character(0)
got <- fetch_espn_player_news(players, c("9221" = "4429795", "1" = "5", "2" = "6"), function(url) { asked <<- c(asked, url); pfeed }, delay = 0)
check("only fantasy players on a team are asked", asked, sprintf(ESPN_PLAYER_NEWS_URL, "4429795"))
check("their stories come back", length(got), 1L)

# Player feed stories belong to the player we asked about, even if ESPN spells his name differently
kfeed <- fromJSON('{"feed":[{"id":21,"type":"Story","headline":"Walker carries the load","published":"2026-11-15T10:00:00Z",
  "images":[{"url":"https://a.espncdn.com/photo/kw.jpg"}],"links":{"web":{"href":"https://www.espn.com/x/21"}},
  "categories":[{"type":"athlete","description":"Kenneth Walker III"}]}]}', simplifyVector = FALSE)
kw <- parse_espn_player_news(kfeed, "Kenneth Walker")
check("ESPN name suffix still matches our player", length(espn_player_articles(espn_articles_by_player(kw), "Kenneth Walker")), 1L)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll ESPN news tests passed\n")
