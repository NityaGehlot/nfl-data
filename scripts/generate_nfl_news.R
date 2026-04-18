# =====================
# scripts/generate_nfl_news.R
# Player-targeted NFL news for fantasy football app
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
OUTPUT_FILE  <- "data/nfl_news.json"
MAX_ARTICLES <- 200
HOURS_BACK   <- 24 * 7   # 7 days
REQUEST_DELAY <- 1.2      # seconds between Google RSS calls (be polite)

# How many top players to query individually (by Google RSS)
TOP_PLAYER_QUERY_LIMIT <- 60

# =====================
# SAFE HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_parse_date <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA)
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
    if (length(parsed) == 0 || all(is.na(parsed))) return(NA)
    parsed[[1]]
  }, error = function(e) NA)
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

# Active skill-position players only
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

# Build lookup: lowercase full name -> display name
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
  if (grepl("injur|injured|out for|placed on ir|season-ending|surgery|torn|fracture|concussion|doubtful|ruled out", t)) return("negative")
  if (grepl("questionable|limited|day-to-day|sore|ailing|monitor|missed practice|didn't practice", t))                  return("slightly_negative")
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cuts|released|cut ", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|record|mvp|pro bowl|comeback|return|activated|off ir", t))                   return("positive")
  "neutral"
}

# =====================
# RELEVANCE FILTER
# =====================
is_relevant <- function(title, desc = "") {
  text <- tolower(paste(title, desc))
  if (grepl("mock draft|college prospect|simulation|nfl combine|high school|ncaa|recruiting|nfl draft prospect|2026 draft", text)) return(FALSE)
  if (grepl("oldest player|all-time list|throwback|decades ago|flashback|history of", text)) return(FALSE)
  has_keyword <- grepl("injur|trade|sign|free agent|roster|waiver|cut|suspend|contract|fantasy|start|bench|touchdown|yard|reception|quarterback|running back|wide receiver|tight end|questionable|ruled out|ir |practice", text)
  has_player  <- length(detect_players(text)) > 0
  has_keyword || has_player
}

# =====================
# GOOGLE NEWS RSS — single query
# =====================
fetch_google_query <- function(query) {
  Sys.sleep(REQUEST_DELAY)
  tryCatch({
    encoded <- URLencode(query, reserved = TRUE)
    url     <- paste0("https://news.google.com/rss/search?q=", encoded, "&hl=en-US&gl=US&ceid=US:en")
    xml     <- read_xml(url)
    items   <- xml_find_all(xml, "//item")
    if (length(items) == 0) return(list())

    results <- lapply(items, function(item) {
      tryCatch({
        title <- xml_text(xml_find_first(item, "title"))
        link  <- xml_text(xml_find_first(item, "link"))
        pub   <- xml_text(xml_find_first(item, "pubDate"))
        desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
        if (is.na(title) || title == "") return(NULL)

        # Strip trailing " - Source Name" that Google appends
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
    res <- GET("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news", timeout(20))
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
        full_text <- paste(title, desc)
        if (!is_relevant(title, desc)) return(NULL)
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
# TOPIC-LEVEL NEWS (injuries, trades, FA, roster moves)
# =====================
fetch_topic_news <- function() {
  topics <- c(
    "NFL injury report 2025",
    "NFL player ruled out week",
    "NFL free agent signing 2025",
    "NFL trade deadline 2025",
    "NFL roster cut released 2025",
    "NFL player suspended 2025",
    "NFL contract extension 2025",
    "NFL starter benched replaced",
    "NFL player placed on IR",
    "NFL waiver wire pickup 2025",
    "fantasy football injury update 2025",
    "NFL player questionable doubtful this week"
  )
  message("\nFetching topic-level Google News (", length(topics), " queries)...")
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

  message("\nFetching player-specific Google News (", nrow(top_players), " players)...")
  results <- list()

  for (i in seq_len(nrow(top_players))) {
    name  <- top_players$display_name[i]
    query <- paste(name, "NFL 2025")
    message("  [player] ", name)
    fetched <- fetch_google_query(query)

    # Guarantee this player appears in players_mentioned
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
# DATE FILTER
# =====================
cutoff <- Sys.time() - hours(HOURS_BACK)
all_news <- Filter(function(x) {
  tryCatch({
    parsed <- safe_parse_date(x$published)
    if (is.na(parsed)) return(TRUE)
    as.numeric(parsed) >= as.numeric(cutoff)
  }, error = function(e) TRUE)
}, all_news)

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
# SORT: injuries > roster moves > positive > neutral
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
message(paste("\n✅ Done! Articles saved:", length(all_news)))
