# =====================
# scripts/generate_nfl_news.R
# Player-targeted NFL news for fantasy football app
# Uses ESPN API + NFL RSS feeds (GitHub Actions compatible)
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)

# =====================
# CONFIG
# =====================
OUTPUT_FILE   <- "data/nfl_news.json"
MAX_ARTICLES  <- 200
REQUEST_DELAY <- 0.5

TOP_PLAYER_QUERY_LIMIT <- 60

# Hard floor — never show anything older than Sep 4 2025
HARD_FLOOR <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
cutoff     <- HARD_FLOOR

message(paste("Window: ", format(HARD_FLOOR, "%Y-%m-%d"), "to", format(Sys.time(), "%Y-%m-%d")))

CURRENT_YEAR <- "2026"

# =====================
# SAFE HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_parse_date <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA_real_)
  tryCatch({
    parsed <- parse_date_time(x, orders = c(
      "a, d b Y H:M:S z",
      "a, d b Y H:M:S",
      "ymd HMS",
      "ymd HM",
      "Y-m-dTH:M:SZ",
      "Y-m-dTH:M:S",
      "Y-m-d H:M:S"
    ), quiet = TRUE, tz = "UTC")
    if (length(parsed) == 0 || all(is.na(parsed))) return(NA_real_)
    as.numeric(parsed[[1]])
  }, error = function(e) NA_real_)
}

is_recent_enough <- function(published_str) {
  ts <- safe_parse_date(published_str)
  if (is.na(ts)) return(FALSE)
  ts >= as.numeric(cutoff)
}

# =====================
# LOAD SLEEPER PLAYERS
# =====================
message("Loading Sleeper players...")

players_raw <- tryCatch(
  fromJSON("https://api.sleeper.app/v1/players/nfl", simplifyDataFrame = FALSE),
  error = function(e) { message("WARNING: Sleeper failed: ", e$message); list() }
)

players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  p_clean <- lapply(p, function(val) {
    if (is.null(val) || length(val) == 0) return(NA)
    if (length(val) > 1) return(val[[1]])
    val
  })
  tryCatch(as.data.frame(p_clean, stringsAsFactors = FALSE), error = function(e) NULL)
}))

active_players <- players_df %>%
  filter(!is.na(status), status == "Active") %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "K")) %>%
  mutate(
    first_name_clean = ifelse(is.na(first_name), "", trimws(first_name)),
    last_name_clean  = ifelse(is.na(last_name),  "", trimws(last_name)),
    full_name        = tolower(paste(first_name_clean, last_name_clean)),
    display_name     = paste(first_name_clean, last_name_clean)
  ) %>%
  filter(full_name != "" & full_name != " ")

player_lookup <- setNames(active_players$display_name, active_players$full_name)
player_names  <- names(player_lookup)
message(paste("Active players loaded:", length(player_names)))

# =====================
# PLAYER DETECTION
# =====================
detect_players <- function(text) {
  if (is.null(text) || is.na(text) || text == "") return(character(0))
  text_lower <- tolower(text)
  matched <- player_names[sapply(player_names, function(nm) {
    grepl(paste0("\\b", gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", nm), "\\b"), text_lower)
  })]
  unique(unname(player_lookup[matched]))
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  if (is.null(text) || is.na(text) || text == "") return("neutral")
  t <- tolower(text)
  if (grepl("injur|injured|out for|placed on ir|season-ending|surgery|torn|fracture|concussion|doubtful|ruled out|pup list", t)) return("negative")
  if (grepl("questionable|limited|day-to-day|sore|ailing|monitor|missed practice", t))                                          return("slightly_negative")
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cuts|released|cut |restructure|visits|agrees|joining|leaving", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|record|mvp|pro bowl|comeback|return|activated|off ir|re-signs|retained", t))         return("positive")
  "neutral"
}

# =====================
# RELEVANCE FILTER
# =====================
is_relevant <- function(title, desc = "") {
  text <- tolower(paste(title, desc))
  if (grepl("mock draft class|college prospect ranking|simulation|high school|ncaa recruiting", text)) return(FALSE)
  if (grepl("oldest player|all-time list|throwback|decades ago|flashback|history of|years ago", text)) return(FALSE)
  has_keyword <- grepl(
    paste0("free agent|free agency|sign|signed|trade|traded|contract|extension|restructure|",
           "release|released|cut |waiver|visit|agrees|deal |draft|ota|minicamp|training camp|",
           "injur|pup|ir |suspend|retire|comeback|offseason|depth chart|starter|",
           "touchdown|rushing|receiving|passing|quarterback|running back|wide receiver|tight end|",
           "fantasy|nfl|football"),
    text
  )
  has_player <- length(detect_players(text)) > 0
  has_keyword || has_player
}

# =====================
# ESPN API — multiple endpoints
# These work reliably from GitHub Actions
# =====================
fetch_espn_news <- function() {
  message("Fetching ESPN NFL news...")
  tryCatch({
    res  <- GET("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?limit=100",
                timeout(20))
    if (res$status_code != 200) { message("ESPN news status: ", res$status_code); return(list()) }
    data     <- fromJSON(content(res, "text", encoding = "UTF-8"))
    articles <- data$articles
    if (is.null(articles) || nrow(articles) == 0) return(list())
    if (is.data.frame(articles)) articles <- split(articles, seq(nrow(articles)))
    message(paste("  ESPN returned", length(articles), "articles"))
    results <- lapply(articles, function(a) {
      tryCatch({
        title     <- a$headline    %||% ""
        desc      <- a$description %||% ""
        link      <- tryCatch(a$links$web$href %||% "", error = function(e) "")
        published <- a$published   %||% ""
        if (title == "" || link == "") return(NULL)
        message("  ESPN article date: ", published, " | ", str_trunc(title, 60))
        if (!is_recent_enough(published)) { message("    -> REJECTED (too old)"); return(NULL) }
        if (!is_relevant(title, desc))    { message("    -> REJECTED (not relevant)"); return(NULL) }
        full_text <- paste(title, desc)
        list(
          title             = title,
          summary           = str_trunc(ifelse(desc != "", desc, title), 200),
          link              = link,
          published         = published,
          source            = "ESPN",
          players_mentioned = detect_players(full_text),
          impact            = get_impact(full_text)
        )
      }, error = function(e) NULL)
    })
    Filter(Negate(is.null), results)
  }, error = function(e) { message("ESPN error: ", e$message); list() })
}

# =====================
# NFL.COM RSS — works from CI
# =====================
fetch_nfl_rss <- function() {
  feeds <- list(
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=news", label = "NFL.com News"),
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=injuries", label = "NFL.com Injuries"),
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=transactions", label = "NFL.com Transactions")
  )

  results <- list()
  for (feed in feeds) {
    message(paste("Fetching", feed$label, "..."))
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(feed$url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (compatible; RScript/1.0)"))
      if (res$status_code != 200) { message("  Status: ", res$status_code); next }
      xml   <- read_xml(content(res, "text", encoding = "UTF-8"))
      items <- xml_find_all(xml, "//item")
      message(paste("  Items found:", length(items)))

      feed_results <- lapply(items, function(item) {
        tryCatch({
          title <- xml_text(xml_find_first(item, "title"))
          link  <- xml_text(xml_find_first(item, "link"))
          pub   <- xml_text(xml_find_first(item, "pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
          if (is.na(title) || title == "") return(NULL)
          message("  NFL.com item date: ", pub, " | ", str_trunc(title, 60))
          if (!is_recent_enough(pub))     { message("    -> REJECTED (too old)"); return(NULL) }
          clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))
          full_text   <- paste(clean_title, desc)
          if (!is_relevant(clean_title, desc)) { message("    -> REJECTED (not relevant)"); return(NULL) }
          list(
            title             = clean_title,
            summary           = str_trunc(ifelse(!is.na(desc) & nchar(desc) > 10, desc, clean_title), 200),
            link              = link,
            published         = pub,
            source            = "NFL.com",
            players_mentioned = detect_players(full_text),
            impact            = get_impact(full_text)
          )
        }, error = function(e) NULL)
      })
      results <- c(results, Filter(Negate(is.null), feed_results))
    }, error = function(e) message(paste("  Feed error:", e$message)))
  }
  results
}

# =====================
# ESPN TEAM NEWS — pulls news per team (very reliable, CI-friendly)
# This gives player-level news because ESPN team feeds include
# individual player injury/transaction items
# =====================
fetch_espn_team_news <- function() {
  # All 32 NFL team abbreviations
  teams <- c(
    "buf","mia","ne","nyj",           # AFC East
    "bal","cin","cle","pit",           # AFC North
    "hou","ind","jax","ten",           # AFC South
    "den","kc","lv","lac",             # AFC West
    "dal","nyg","phi","wsh",           # NFC East
    "chi","det","gb","min",            # NFC North
    "atl","car","no","tb",             # NFC South
    "ari","lar","sf","sea"             # NFC West
  )

  message(paste("\nFetching ESPN team news for", length(teams), "teams..."))
  results <- list()

  for (team in teams) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      url <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?team=", team, "&limit=10")
      res <- GET(url, timeout(15))
      if (res$status_code != 200) next

      data     <- fromJSON(content(res, "text", encoding = "UTF-8"))
      articles <- data$articles
      if (is.null(articles) || nrow(articles) == 0) next
      if (is.data.frame(articles)) articles <- split(articles, seq(nrow(articles)))

      team_results <- lapply(articles, function(a) {
        tryCatch({
          title     <- a$headline    %||% ""
          desc      <- a$description %||% ""
          link      <- tryCatch(a$links$web$href %||% "", error = function(e) "")
          published <- a$published   %||% ""
          if (title == "" || link == "") return(NULL)
          if (!is_recent_enough(published)) return(NULL)
          if (!is_relevant(title, desc))    return(NULL)
          full_text <- paste(title, desc)
          list(
            title             = title,
            summary           = str_trunc(ifelse(desc != "", desc, title), 200),
            link              = link,
            published         = published,
            source            = "ESPN",
            players_mentioned = detect_players(full_text),
            impact            = get_impact(full_text)
          )
        }, error = function(e) NULL)
      })
      results <- c(results, Filter(Negate(is.null), team_results))
    }, error = function(e) NULL)
  }

  message(paste("ESPN team news total:", length(results)))
  results
}

# =====================
# ROTOWORLD / NBC SPORTS RSS — CI friendly
# =====================
fetch_rotoworld_rss <- function() {
  feeds <- list(
    list(url = "https://www.nbcsports.com/rss/sports/nfl", label = "NBC Sports NFL"),
    list(url = "https://www.cbssports.com/rss/headlines/nfl/", label = "CBS Sports NFL")
  )

  results <- list()
  for (feed in feeds) {
    message(paste("Fetching", feed$label, "..."))
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(feed$url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (compatible; RScript/1.0)"))
      if (res$status_code != 200) { message("  Status: ", res$status_code); next }
      xml   <- read_xml(content(res, "text", encoding = "UTF-8"))
      items <- xml_find_all(xml, "//item")
      message(paste("  Items found:", length(items)))

      feed_results <- lapply(items, function(item) {
        tryCatch({
          title <- xml_text(xml_find_first(item, "title"))
          link  <- xml_text(xml_find_first(item, "link"))
          pub   <- xml_text(xml_find_first(item, "pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
          if (is.na(title) || title == "") return(NULL)
          if (!is_recent_enough(pub))      return(NULL)
          clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))
          full_text   <- paste(clean_title, desc)
          if (!is_relevant(clean_title, desc)) return(NULL)
          list(
            title             = clean_title,
            summary           = str_trunc(ifelse(!is.na(desc) & nchar(desc) > 10, desc, clean_title), 200),
            link              = link,
            published         = pub,
            source            = sub("https://www\\.|/.*", "", feed$url),
            players_mentioned = detect_players(full_text),
            impact            = get_impact(full_text)
          )
        }, error = function(e) NULL)
      })
      results <- c(results, Filter(Negate(is.null), feed_results))
    }, error = function(e) message(paste("  Feed error:", e$message)))
  }
  results
}

# =====================
# RUN ALL SOURCES
# =====================
message("\n=== Starting news fetch ===\n")

all_news <- c(
  fetch_espn_news(),
  fetch_espn_team_news(),
  fetch_nfl_rss(),
  fetch_rotoworld_rss()
)

all_news <- Filter(Negate(is.null), all_news)
message(paste("\nTotal raw articles:", length(all_news)))

# =====================
# FINAL DATE PASS
# =====================
before   <- length(all_news)
all_news <- Filter(function(x) is_recent_enough(x$published), all_news)
message(paste("Dropped by final date pass:", before - length(all_news)))
message(paste("After date filter:", length(all_news)))

# =====================
# DEDUPLICATE
# =====================
seen <- character(0)
all_news <- Filter(function(x) {
  key <- tolower(trimws(x$title))
  if (key %in% seen) return(FALSE)
  seen <<- c(seen, key)
  TRUE
}, all_news)
message(paste("After dedup:", length(all_news)))

# =====================
# SORT
# =====================
impact_rank <- c(
  "negative"          = 5,
  "slightly_negative" = 4,
  "roster_move"       = 3,
  "positive"          = 2,
  "neutral"           = 1
)
all_news <- all_news[order(
  sapply(all_news, function(x) impact_rank[x$impact %||% "neutral"]),
  decreasing = TRUE
)]

# =====================
# LIMIT
# =====================
all_news <- head(all_news, MAX_ARTICLES)

# =====================
# FALLBACK
# =====================
if (length(all_news) == 0) {
  message("WARNING: All sources returned 0 articles — check logs above for rejection reasons")
  all_news <- list(list(
    title             = "No recent NFL news available",
    summary           = "Check back soon for the latest player updates.",
    link              = "https://www.espn.com/nfl/",
    published         = format(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"),
    source            = "SYSTEM",
    players_mentioned = character(0),
    impact            = "neutral"
  ))
}

# =====================
# SAVE
# =====================
if (!dir.exists("data")) dir.create("data")
write_json(all_news, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)
message(paste("\n✅ Done! Articles saved:", length(all_news)))
