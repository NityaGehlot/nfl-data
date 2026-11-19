# scripts/espn_news.R
# Player news with pictures from ESPN's public news feed (the same JSON feed ESPN's site and
# apps use, and the one the app's Home headlines come from). Each article comes with its cover
# image, its real link and the players it's about (ESPN tags athletes), so unlike Google News
# links nothing has to be resolved or scraped to show a picture.
#
#   https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?team={espn team id}&limit=50
#
# One request per team per run (32), merged into each player's news before the Google articles.

ESPN_NEWS_BASE <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/news"
# ESPN's NFL team ids (31 and 32 are unused).
ESPN_NFL_TEAM_IDS <- c(1:30, 33, 34)

espn_team_news_url <- function(team_id, limit = 50) sprintf("%s?team=%d&limit=%d", ESPN_NEWS_BASE, as.integer(team_id), as.integer(limit))

#' The cover picture: the "header" image if there is one, else the first.
espn_image <- function(images) {
  if (!length(images)) return(NULL)
  header <- Filter(function(i) identical(i$type, "header"), images)
  pick <- if (length(header)) header[[1]] else images[[1]]
  url <- pick$url
  if (is.null(url) || !nzchar(url)) return(NULL)
  sub("^http://", "https://", url)
}

#' ESPN news JSON (parsed with simplifyVector = FALSE) -> list of articles with the athletes they tag.
parse_espn_articles <- function(data) {
  out <- list()
  for (a in data$articles %||% list()) {
    # Videos ("Media") are kept: their image is the video's cover, and they're marked as video.
    if (is.null(a$headline)) next
    is_video <- identical(a$type, "Media")
    if (is_video && is.null(espn_image(a$images %||% list()))) next
    link <- a$links$web$href %||% a$links$mobile$href
    if (is.null(link)) next
    cats <- a$categories %||% list()
    athletes <- unlist(lapply(cats, function(c) if (identical(c$type, "athlete") && !is.null(c$description)) c$description else NULL))
    published <- substr(a$published %||% a$lastModified %||% "", 1, 10)
    out[[length(out) + 1]] <- list(
      id = as.character(a$id %||% link),
      title = a$headline,
      description = a$description %||% "",
      link = link,
      image = espn_image(a$images %||% list()),
      published = if (nzchar(published)) published else NULL,
      athletes = as.character(athletes %||% character(0)),
      video = if (is_video) TRUE else NULL
    )
  }
  out
}

#' All articles from every team feed (deduplicated). `get_json(url)` returns parsed JSON or NULL.
fetch_espn_news <- function(team_ids = ESPN_NFL_TEAM_IDS, get_json = espn_get_json, delay = 0.2) {
  seen <- character(0)
  all <- list()
  ok <- 0L
  for (id in team_ids) {
    data <- get_json(espn_team_news_url(id))
    if (!is.null(data)) ok <- ok + 1L
    for (a in parse_espn_articles(data %||% list())) {
      if (a$id %in% seen) next
      seen <- c(seen, a$id)
      all[[length(all) + 1]] <- a
    }
    if (delay > 0) Sys.sleep(delay)
  }
  message("ESPN news: ", length(all), " articles from ", ok, " of ", length(team_ids), " team feeds")
  all
}

espn_get_json <- function(url) {
  tryCatch({
    res <- httr::GET(url, httr::timeout(20))
    if (httr::status_code(res) != 200) return(NULL)
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  }, error = function(e) NULL)
}

#' Player name (lowercase) -> articles tagging him.
espn_articles_by_player <- function(articles) {
  by <- list()
  for (a in articles) {
    for (name in unique(tolower(a$athletes))) by[[name]] <- c(by[[name]] %||% list(), list(a))
  }
  by
}

#' ESPN articles for one player, shaped like the Google articles (title, source, link, url,
#' image, published, player, impact, category, summary). Kept when the headline names him, or
#' when he's the only player the story is about.
espn_player_articles <- function(by_player, player_name, season_start = NULL) {
  hits <- by_player[[tolower(player_name)]] %||% list()
  out <- list()
  for (a in hits) {
    if (!(mentions_player(a$title, player_name) || length(a$athletes) == 1)) next
    if (!is.null(season_start) && !is.null(a$published) && as.Date(a$published) < as.Date(season_start)) next
    info <- classify_headline(a$title, player_name)
    out[[length(out) + 1]] <- Filter(Negate(is.null), list(
      title = a$title,
      source = "ESPN",
      link = a$link,
      url = a$link,
      image = a$image,
      video = a$video,
      published = a$published,
      player = player_name,
      impact = info$impact,
      category = info$category,
      summary = info$summary
    ))
  }
  out
}

# ---------------------------------------------------------------------------------------------
# ESPN's per-player fantasy news (the feed behind ESPN Fantasy player cards): stories and videos
# about one player, with their cover pictures and real links. RotoWire blurbs in the same feed
# have no picture or link and are skipped.
ESPN_PLAYER_NEWS_URL <- "https://site.api.espn.com/apis/fantasy/v2/games/ffl/news/players?playerId=%s&limit=10"
ESPN_ID_MAP_URL <- "https://raw.githubusercontent.com/dynastyprocess/data/master/files/db_playerids.csv"
ESPN_PLAYER_POSITIONS <- c("QB", "RB", "WR", "TE", "K")

#' Player feed JSON -> articles (same shape as parse_espn_articles), tagged with the player.
parse_espn_player_news <- function(data, player_name) {
  items <- Filter(function(a) !identical(a$type, "Rotowire"), data$feed %||% list())
  arts <- parse_espn_articles(list(articles = items))
  arts <- Filter(function(a) !is.null(a$image), arts)
  # This feed is about the player we asked for: tag his stories with our spelling of his name
  # (ESPN may write "Kenneth Walker III" where we have "Kenneth Walker").
  lapply(arts, function(a) {
    a$athletes <- player_name
    a
  })
}

#' Sleeper id -> ESPN id, from the public player id table. Empty when it can't be downloaded.
load_espn_ids <- function(get_csv = function(url) utils::read.csv(url, colClasses = "character")) {
  tab <- tryCatch(get_csv(ESPN_ID_MAP_URL), error = function(e) NULL)
  if (is.null(tab) || !all(c("sleeper_id", "espn_id") %in% names(tab))) {
    message("⚠️ ESPN id map unavailable; skipping ESPN player news")
    return(character(0))
  }
  ok <- !is.na(tab$sleeper_id) & nzchar(tab$sleeper_id) & !is.na(tab$espn_id) & nzchar(tab$espn_id)
  stats::setNames(as.character(tab$espn_id[ok]), as.character(tab$sleeper_id[ok]))
}

#' Stories from each fantasy-position player on an NFL team who has an ESPN id (flat list).
fetch_espn_player_news <- function(players, ids, get_json = espn_get_json, delay = 0.2) {
  out <- list()
  asked <- 0L
  for (p in players) {
    pos <- toupper(p$position %||% "")
    team <- p$team %||% ""
    espn_id <- if (!is.null(p$player_id)) unname(ids[as.character(p$player_id)]) else NA
    if (!(pos %in% ESPN_PLAYER_POSITIONS) || !nzchar(team) || length(espn_id) == 0 || is.na(espn_id)) next
    asked <- asked + 1L
    data <- get_json(sprintf(ESPN_PLAYER_NEWS_URL, espn_id))
    if (!is.null(data)) out <- c(out, parse_espn_player_news(data, p$full_name %||% ""))
    if (delay > 0) Sys.sleep(delay)
  }
  message("ESPN player news: ", length(out), " stories from ", asked, " players")
  out
}
