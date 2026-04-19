# =====================
# scripts/generate_nfl_news.R
# Player-targeted NFL news for fantasy football app
# Offseason-aware version — hard floor Sep 4 2025
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
REQUEST_DELAY <- 1.2   # seconds between Google RSS calls

TOP_PLAYER_QUERY_LIMIT <- 60

# ── Date window ───────────────────────────────────────────────────────────────
# HARD_FLOOR: absolute earliest date we will ever accept.
# Nothing older than this will appear in the JSON, ever.
# Set to Sep 4 2025 (NFL season opener) so we cover the full
# 2025 season + 2026 offseason. Move forward each year.
HARD_FLOOR <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")

# Rolling window: only show the last N days from today.
# During the offseason 45 days is fine. Drop to 14 once
# the regular season starts so week-old injury news falls off.
DAYS_BACK <- 45
cutoff    <- max(HARD_FLOOR, Sys.time() - days(DAYS_BACK))

message(paste("Hard floor :", format(HARD_FLOOR, "%Y-%m-%d")))
message(paste("Live cutoff:", format(cutoff,     "%Y-%m-%d %H:%M UTC")))

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

# Returns TRUE only if the article's date is >= cutoff.
# If date cannot be parsed at all → REJECT (no more free passes for bad dates).
is_recent_enough <- function(published_str) {
  ts <- safe_parse_date(published_str)
  if (is.na(ts)) return(FALSE)          # unparseable = reject
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
  if (grepl("questionable|limited|day-to-day|sore|ailing|monitor|missed practice|didn't practice", t))                          return("slightly_negative")
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cuts|released|cut |restructure|visits|agrees|joining|leaving", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|record|mvp|pro bowl|comeback|return|activated|off ir|re-signs|retained", t))         return("positive")
  "neutral"
}

# =====================
# RELEVANCE FILTER
# =====================
is_relevant <- function(title, desc = "") {
  text <- tolower(paste(title, desc))
  if (grepl("mock draft class|college prospect ranking|simulation|high school|ncaa recruiting|2025 nfl draft prospect", text)) return(FALSE)
  if (grepl("oldest player|all-time list|throwback|decades ago|flashback|history of|years ago", text))                        return(FALSE)
  if (grepl("week \\d+ recap|final score|box score|game summary|highlights from", text))                                      return(FALSE)

  has_offseason <- grepl(
    paste0("free agent|free agency|sign|signed|signs|trade|traded|contract|extension|restructure|",
           "release|released|cut |waiver|visit|agrees|deal |draft|ota|minicamp|training camp|",
           "injur|pup|ir |suspend|retire|comeback|",
           "2026 season|2025 season|offseason|depth chart|compete|competition|starter|",
           "nfl network|espn|fantasy football"),
    text
  )
  has_player <- length(detect_players(text)) > 0

  has_offseason || has_player
}

# =====================
# GOOGLE NEWS RSS — single query
# Rejects articles that fail the date check immediately on fetch
# =====================
fetch_google_query <- function(query) {
  Sys.sleep(REQUEST_DELAY)
  tryCatch({
    encoded <- URLencode(query, reserved = TRUE)
    url     <- paste0("https://news.google.com/rss/search?q=", encoded,
                      "&hl=en-US&gl=US&ceid=US:en")
    xml   <- read_xml(url)
    items <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(list())

    results <- lapply(items, function(item) {
      tryCatch({
        title <- xml_text(xml_find_first(item, "title"))
        link  <- xml_text(xml_find_first(item, "link"))
        pub   <- xml_text(xml_find_first(item, "pubDate"))
        desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")

        if (is.na(title) || title == "") return(NULL)

        # ── Hard date gate: reject old articles immediately ──────────────────
        if (!is_recent_enough(pub)) return(NULL)

        clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))
        full_text   <- paste(clean_title, desc)
        if (!is_relevant(clean_title, desc)) return(NULL)

        list(
          title             = clean_title,
          summary           = str_trunc(ifelse(!is.na(desc) & nchar(desc) > 10, desc, clean_title), 200),
          link              = link,
          published         = pub,
          source            = "GoogleNews",
          players_mentioned = detect_players(full_text),
          impact            = get_impact(full_text)
        )
      }, error = function(e) NULL)
    })
    Filter(Negate(is.null), results)
  }, error = function(e) {
    message("  WARNING: Google query failed ['", query, "']: ", e$message)
    list()
  })
}

# =====================
# ESPN NEWS
# =====================
fetch_espn <- function() {
  message("Fetching ESPN NFL news...")
  tryCatch({
    res <- GET("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news",
               timeout(20))
    if (res$status_code != 200) { message("ESPN status: ", res$status_code); return(list()) }

    data     <- fromJSON(content(res, "text", encoding = "UTF-8"))
    articles <- data$articles
    if (is.null(articles) || nrow(articles) == 0) return(list())
    if (is.data.frame(articles)) articles <- split(articles, seq(nrow(articles)))

    results <- lapply(articles, function(a) {
      tryCatch({
        title     <- a$headline    %||% ""
        desc      <- a$description %||% ""
        link      <- tryCatch(a$links$web$href %||% "", error = function(e) "")
        published <- a$published   %||% ""
        if (title == "" || link == "") return(NULL)

        # ── Hard date gate ───────────────────────────────────────────────────
        if (!is_recent_enough(published)) return(NULL)

        if (!is_relevant(title, desc)) return(NULL)
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
# OFFSEASON TOPIC QUERIES
# =====================
fetch_topic_news <- function() {
  topics <- c(
    paste("NFL free agency", CURRENT_YEAR),
    paste("NFL free agent signings", CURRENT_YEAR),
    paste("NFL free agent visits", CURRENT_YEAR),
    paste("NFL trade", CURRENT_YEAR),
    paste("NFL trade rumors", CURRENT_YEAR),
    paste("NFL contract extension", CURRENT_YEAR),
    paste("NFL contract restructure", CURRENT_YEAR),
    paste("NFL player released cut", CURRENT_YEAR),
    paste("NFL waiver wire", CURRENT_YEAR),
    paste("NFL draft", CURRENT_YEAR),
    paste("NFL draft pick trade", CURRENT_YEAR),
    paste("NFL injury offseason", CURRENT_YEAR),
    paste("NFL PUP list", CURRENT_YEAR),
    paste("NFL player suspended", CURRENT_YEAR),
    paste("NFL OTA minicamp", CURRENT_YEAR),
    paste("NFL training camp", CURRENT_YEAR),
    paste("fantasy football offseason moves", CURRENT_YEAR),
    paste("NFL depth chart update", CURRENT_YEAR),
    paste("NFL starter competition", CURRENT_YEAR)
  )

  message("\nFetching offseason topic news (", length(topics), " queries)...")
  results <- list()
  for (q in topics) {
    message("  [topic] ", q)
    results <- c(results, fetch_google_query(q))
  }
  results
}

# =====================
# PLAYER-SPECIFIC NEWS
# =====================
fetch_player_news <- function() {
  pos_order <- c("QB" = 1, "WR" = 2, "RB" = 3, "TE" = 4, "K" = 5)

  top_players <- active_players %>%
    mutate(pos_rank = sapply(position, function(p) pos_order[p] %||% 99)) %>%
    arrange(pos_rank) %>%
    slice_head(n = TOP_PLAYER_QUERY_LIMIT)

  message("\nFetching player-specific news (", nrow(top_players), " players)...")
  results <- list()

  for (i in seq_len(nrow(top_players))) {
    name    <- top_players$display_name[i]
    query   <- paste(name, "NFL", CURRENT_YEAR)
    message("  [player] ", name)
    fetched <- fetch_google_query(query)

    fetched <- lapply(fetched, function(art) {
      if (!name %in% art$players_mentioned) {
        art$players_mentioned <- unique(c(name, art$players_mentioned))
      }
      art
    })
    results <- c(results, fetched)
  }
  results
}

# =====================
# RUN ALL SOURCES
# =====================
message("\n=== Starting news fetch ===\n")

all_news <- c(
  fetch_espn(),
  fetch_topic_news(),
  fetch_player_news()
)

all_news <- Filter(Negate(is.null), all_news)
message(paste("\nTotal raw articles:", length(all_news)))

# =====================
# FINAL DATE PASS — belt-and-suspenders
# Catches anything that slipped through (e.g. ESPN with bad date field)
# =====================
before_date_filter <- length(all_news)
all_news <- Filter(function(x) is_recent_enough(x$published), all_news)
message(paste("Dropped by final date pass:", before_date_filter - length(all_news)))
message(paste("After date filter:", length(all_news)))

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
  message("WARNING: No news found — using fallback")
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
message(paste("\n✅ Done! Articles saved:", length(all_news),
              "| Cutoff:", format(cutoff, "%Y-%m-%d"),
              "| Floor:", format(HARD_FLOOR, "%Y-%m-%d")))
