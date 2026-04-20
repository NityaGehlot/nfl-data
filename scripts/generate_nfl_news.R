# =====================
# scripts/generate_nfl_news.R
# Player-targeted NFL news for fantasy football app
# Multi-source: ESPN API (general + all 32 teams) + NFL.com +
#               CBS Sports + NBC Sports + ProFootballTalk +
#               FantasyPros + Bleacher Report + The Athletic (RSS)
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
MAX_ARTICLES  <- 300        # raised ceiling — more sources = more raw articles
REQUEST_DELAY <- 0.4        # seconds between calls

# Hard floor — never show anything older than Sep 4 2025
HARD_FLOOR <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
cutoff     <- HARD_FLOOR

message(paste("Window:", format(HARD_FLOOR, "%Y-%m-%d"), "to", format(Sys.time(), "%Y-%m-%d")))

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
      "ymd HMS", "ymd HM",
      "Y-m-dTH:M:SZ",
      "Y-m-dTH:M:S",
      "Y-m-d H:M:S"
    ), quiet = TRUE, tz = "UTC")
    if (length(parsed) == 0 || all(is.na(parsed))) return(NA_real_)
    as.numeric(parsed[[1]])
  }, error = function(e) NA_real_)
}

is_recent_enough <- function(pub) {
  ts <- safe_parse_date(pub)
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
  tl <- tolower(text)
  matched <- player_names[sapply(player_names, function(nm) {
    grepl(paste0("\\b", gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", nm), "\\b"), tl)
  })]
  unique(unname(player_lookup[matched]))
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  if (is.null(text) || is.na(text) || text == "") return("neutral")
  t <- tolower(text)
  if (grepl("injur|injured|out for|placed on ir|season-ending|surgery|torn|fracture|concussion|doubtful|ruled out|pup list|non-football injury", t)) return("negative")
  if (grepl("questionable|limited|day-to-day|sore|ailing|monitor|missed practice|did not practice|dnp", t))                                          return("slightly_negative")
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cuts|released|cut |restructure|visits|agrees|joining|leaving|released|departed", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|record|mvp|pro bowl|comeback|return|activated|off ir|re-signs|retained|named starter", t))                return("positive")
  "neutral"
}

# =====================
# RELEVANCE FILTER
# =====================
is_relevant <- function(title, desc = "") {
  text <- tolower(paste(title, desc))
  # Hard exclusions
  if (grepl("mock draft class|college prospect ranking|simulation|high school|ncaa recruiting", text)) return(FALSE)
  if (grepl("oldest player|all-time list|throwback|decades ago|flashback|history of|years ago", text)) return(FALSE)
  # Keep if it has a known player OR a relevant NFL keyword
  has_keyword <- grepl(
    paste0("free agent|free agency|sign|signed|trade|traded|contract|extension|restructure|",
           "release|released|cut |waiver|visit|agrees|deal |draft|ota|minicamp|training camp|",
           "injur|pup|ir |suspend|retire|comeback|offseason|depth chart|starter|",
           "touchdown|rushing|receiving|passing|quarterback|running back|wide receiver|tight end|",
           "fantasy|nfl|football|roster|transaction"),
    text
  )
  has_player <- length(detect_players(text)) > 0
  has_keyword || has_player
}

# =====================
# GENERIC RSS FETCHER
# Used by all RSS-based sources
# =====================
fetch_rss <- function(url, source_label) {
  Sys.sleep(REQUEST_DELAY)
  tryCatch({
    res <- GET(url, timeout(15),
               add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36"))
    if (res$status_code != 200) {
      message(paste("  ", source_label, "status:", res$status_code))
      return(list())
    }
    raw_text <- content(res, "text", encoding = "UTF-8")
    xml      <- read_xml(raw_text)
    items    <- xml_find_all(xml, "//item")
    if (length(items) == 0) { message(paste("  ", source_label, "— 0 items")); return(list()) }
    message(paste("  ", source_label, "—", length(items), "items"))

    results <- lapply(items, function(item) {
      tryCatch({
        title <- xml_text(xml_find_first(item, "title"))
        link  <- xml_text(xml_find_first(item, "link"))
        pub   <- xml_text(xml_find_first(item, "pubDate"))
        desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
        if (is.na(title) || trimws(title) == "") return(NULL)
        if (!is_recent_enough(pub))               return(NULL)
        clean_title <- str_trim(str_replace(title, "\\s*[-|]\\s*[^-|]+$", ""))
        if (nchar(clean_title) < 5) clean_title <- title
        full_text   <- paste(clean_title, desc)
        if (!is_relevant(clean_title, desc))      return(NULL)
        list(
          title             = clean_title,
          summary           = str_trunc(ifelse(!is.na(desc) & nchar(trimws(desc)) > 15, desc, clean_title), 250),
          link              = link,
          published         = pub,
          source            = source_label,
          players_mentioned = detect_players(full_text),
          impact            = get_impact(full_text)
        )
      }, error = function(e) NULL)
    })
    Filter(Negate(is.null), results)
  }, error = function(e) {
    message(paste("  ERROR:", source_label, "-", e$message))
    list()
  })
}

# =====================
# SOURCE 1: ESPN GENERAL NEWS API
# =====================
fetch_espn_general <- function() {
  message("\n[Source] ESPN General News")
  tryCatch({
    res <- GET("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?limit=100",
               timeout(20))
    if (res$status_code != 200) return(list())
    data     <- fromJSON(content(res, "text", encoding = "UTF-8"))
    articles <- data$articles
    if (is.null(articles) || nrow(articles) == 0) return(list())
    if (is.data.frame(articles)) articles <- split(articles, seq(nrow(articles)))
    message(paste("  ESPN general —", length(articles), "articles"))

    results <- lapply(articles, function(a) {
      tryCatch({
        title     <- a$headline    %||% ""
        desc      <- a$description %||% ""
        link      <- tryCatch(a$links$web$href %||% "", error = function(e) "")
        published <- a$published   %||% ""
        if (title == "" || link == "")     return(NULL)
        if (!is_recent_enough(published))  return(NULL)
        if (!is_relevant(title, desc))     return(NULL)
        full_text <- paste(title, desc)
        list(
          title             = title,
          summary           = str_trunc(ifelse(desc != "", desc, title), 250),
          link              = link,
          published         = published,
          source            = "ESPN",
          players_mentioned = detect_players(full_text),
          impact            = get_impact(full_text)
        )
      }, error = function(e) NULL)
    })
    Filter(Negate(is.null), results)
  }, error = function(e) { message("ESPN general error:", e$message); list() })
}

# =====================
# SOURCE 2: ESPN PER-TEAM NEWS API (all 32 teams)
# Best source for player-specific news
# =====================
fetch_espn_teams <- function() {
  teams <- c(
    "buf","mia","ne","nyj",
    "bal","cin","cle","pit",
    "hou","ind","jax","ten",
    "den","kc","lv","lac",
    "dal","nyg","phi","wsh",
    "chi","det","gb","min",
    "atl","car","no","tb",
    "ari","lar","sf","sea"
  )
  message(paste("\n[Source] ESPN Team News (", length(teams), "teams)"))
  results <- list()

  for (team in teams) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      url <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?team=",
                    team, "&limit=20")
      res  <- GET(url, timeout(15))
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
          if (title == "" || link == "")    return(NULL)
          if (!is_recent_enough(published)) return(NULL)
          if (!is_relevant(title, desc))    return(NULL)
          full_text <- paste(title, desc)
          list(
            title             = title,
            summary           = str_trunc(ifelse(desc != "", desc, title), 250),
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
  message(paste("  ESPN team news total:", length(results)))
  results
}

# =====================
# SOURCE 3: NFL.COM RSS FEEDS
# =====================
fetch_nfl_com <- function() {
  message("\n[Source] NFL.com RSS")
  feeds <- list(
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=news",         label = "NFL.com/news"),
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=injuries",     label = "NFL.com/injuries"),
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=transactions", label = "NFL.com/transactions"),
    list(url = "https://www.nfl.com/rss/rsslanding?searchString=fantasy",      label = "NFL.com/fantasy")
  )
  results <- list()
  for (f in feeds) results <- c(results, fetch_rss(f$url, f$label))
  results
}

# =====================
# SOURCE 4: ProFootballTalk (NBC Sports) — very strong for transactions/injuries
# =====================
fetch_pft <- function() {
  message("\n[Source] ProFootballTalk RSS")
  feeds <- list(
    list(url = "https://profootballtalk.nbcsports.com/feed/",                     label = "PFT"),
    list(url = "https://profootballtalk.nbcsports.com/category/news/feed/",       label = "PFT/news"),
    list(url = "https://profootballtalk.nbcsports.com/category/transactions/feed/", label = "PFT/transactions"),
    list(url = "https://profootballtalk.nbcsports.com/category/injuries/feed/",   label = "PFT/injuries")
  )
  results <- list()
  for (f in feeds) results <- c(results, fetch_rss(f$url, f$label))
  results
}

# =====================
# SOURCE 5: CBS Sports NFL RSS
# =====================
fetch_cbs <- function() {
  message("\n[Source] CBS Sports RSS")
  feeds <- list(
    list(url = "https://www.cbssports.com/rss/headlines/nfl/",          label = "CBS/nfl"),
    list(url = "https://www.cbssports.com/rss/headlines/fantasy/nfl/",  label = "CBS/fantasy-nfl")
  )
  results <- list()
  for (f in feeds) results <- c(results, fetch_rss(f$url, f$label))
  results
}

# =====================
# SOURCE 6: Bleacher Report NFL RSS
# =====================
fetch_bleacher <- function() {
  message("\n[Source] Bleacher Report RSS")
  feeds <- list(
    list(url = "https://bleacherreport.com/articles/feed?tag_id=16",  label = "BR/nfl"),
    list(url = "https://bleacherreport.com/articles/feed?tag_id=9",   label = "BR/fantasy")
  )
  results <- list()
  for (f in feeds) results <- c(results, fetch_rss(f$url, f$label))
  results
}

# =====================
# SOURCE 7: The Athletic NFL RSS
# =====================
fetch_athletic <- function() {
  message("\n[Source] The Athletic RSS")
  fetch_rss("https://theathletic.com/nfl/feed/", "TheAthletic/nfl")
}

# =====================
# SOURCE 8: FantasyPros RSS
# =====================
fetch_fantasypros <- function() {
  message("\n[Source] FantasyPros RSS")
  feeds <- list(
    list(url = "https://www.fantasypros.com/nfl/feed/",         label = "FantasyPros"),
    list(url = "https://www.fantasypros.com/nfl/news/feed/",    label = "FantasyPros/news")
  )
  results <- list()
  for (f in feeds) results <- c(results, fetch_rss(f$url, f$label))
  results
}

# =====================
# SOURCE 9: Spotrac (contracts / transactions)
# =====================
fetch_spotrac <- function() {
  message("\n[Source] Spotrac RSS")
  fetch_rss("https://www.spotrac.com/feed/", "Spotrac")
}

# =====================
# SOURCE 10: Over The Cap (contracts)
# =====================
fetch_overthecap <- function() {
  message("\n[Source] Over The Cap RSS")
  fetch_rss("https://overthecap.com/feed/", "OverTheCap")
}

# =====================
# RUN ALL SOURCES
# =====================
message("\n=== Starting news fetch ===\n")

all_news <- c(
  fetch_espn_general(),
  fetch_espn_teams(),
  fetch_nfl_com(),
  fetch_pft(),
  fetch_cbs(),
  fetch_bleacher(),
  fetch_athletic(),
  fetch_fantasypros(),
  fetch_spotrac(),
  fetch_overthecap()
)

all_news <- Filter(Negate(is.null), all_news)
message(paste("\nTotal raw articles:", length(all_news)))

# =====================
# FINAL DATE PASS (belt-and-suspenders)
# =====================
before   <- length(all_news)
all_news <- Filter(function(x) is_recent_enough(x$published), all_news)
message(paste("Dropped by date filter:", before - length(all_news),
              "| Remaining:", length(all_news)))

# =====================
# DEDUPLICATE BY TITLE
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
# SORT: newest first within each impact tier
# This ensures fresh articles replace old ones naturally each run
# =====================
impact_rank <- c(
  "negative"          = 5,
  "slightly_negative" = 4,
  "roster_move"       = 3,
  "positive"          = 2,
  "neutral"           = 1
)

pub_ts <- sapply(all_news, function(x) safe_parse_date(x$published) %||% 0)

all_news <- all_news[order(
  sapply(all_news, function(x) impact_rank[x$impact %||% "neutral"]),
  pub_ts,
  decreasing = TRUE
)]

# =====================
# LIMIT — keep only the MAX_ARTICLES most relevant/recent
# Oldest articles fall off the bottom automatically each run
# =====================
all_news <- head(all_news, MAX_ARTICLES)

if (length(all_news) > 0) {
  oldest <- all_news[[length(all_news)]]$published
  newest <- all_news[[1]]$published
  message(paste("Oldest kept:", oldest))
  message(paste("Newest kept:", newest))
}

# =====================
# FALLBACK
# =====================
if (length(all_news) == 0) {
  message("WARNING: All sources returned 0 articles after filtering")
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
# SAVE — overwrites the JSON completely each run
# This is intentional: old articles are replaced by current ones
# =====================
if (!dir.exists("data")) dir.create("data")
write_json(all_news, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)
message(paste("\n✅ Done! Articles saved:", length(all_news),
              "| Floor:", format(HARD_FLOOR, "%Y-%m-%d"),
              "| Run:", format(Sys.time(), "%Y-%m-%d %H:%M UTC")))
