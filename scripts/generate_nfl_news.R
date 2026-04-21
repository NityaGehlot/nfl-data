# =====================
# scripts/generate_nfl_news.R (WORKING FINAL VERSION)
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)
library(stringdist)

# =====================
# CONFIG
# =====================
OUTPUT_DIR <- "data"
MAX_PER_PLAYER <- 2
REQUEST_DELAY <- 0.5

SEASON_START <- as.Date("2025-09-04")
TODAY <- Sys.Date()

# =====================
# LOAD PLAYER STATS
# =====================
message("Loading player stats...")

raw_stats <- fromJSON(
  "data/player_stats_2025_week17.json",
  simplifyDataFrame = FALSE
)

clean_rows <- lapply(raw_stats, function(entry) {

  if (length(entry) == 0) return(NULL)

  player <- entry[[1]]

  if (is.null(player$player_name) || is.null(player$position)) return(NULL)

  player$fantasy_points_ppr <- suppressWarnings(
    as.numeric(player$fantasy_points_ppr)
  )

  return(player)
})

clean_rows <- Filter(Negate(is.null), clean_rows)
stats <- bind_rows(clean_rows)

if (nrow(stats) == 0) {
  stop("❌ No valid player data loaded")
}

# =====================
# CLEAN + FILTER
# =====================
stats <- stats %>%
  filter(!is.na(player_name), !is.na(position)) %>%
  filter(fantasy_points_ppr > 0) %>%
  distinct(player_name, .keep_all = TRUE) %>%
  arrange(desc(fantasy_points_ppr))

# =====================
# 🔥 EXPANDED COVERAGE
# =====================
qb_players <- stats %>% filter(position == "QB") %>% slice_head(n = 64)
rb_players <- stats %>% filter(position == "RB") %>% slice_head(n = 80)
wr_players <- stats %>% filter(position == "WR") %>% slice_head(n = 140)
te_players <- stats %>% filter(position == "TE") %>% slice_head(n = 64)
k_players  <- stats %>% filter(position == "K")  %>% slice_head(n = 32)

# =====================
# HELPERS
# =====================
safe_parse_date <- function(x) {
  tryCatch({
    as.Date(parse_date_time(
      x,
      orders = c("a, d b Y H:M:S z", "ymd HMS", "Y-m-dTH:M:SZ"),
      tz = "UTC"
    ))
  }, error = function(e) NA)
}

get_impact <- function(text) {
  t <- tolower(text)

  if (grepl("injur|out|ir|surgery", t)) return("negative")
  if (grepl("questionable|limited", t)) return("slightly_negative")
  if (grepl("signed|trade|contract|released|cut", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|huge", t)) return("positive")

  "neutral"
}

# 🔥 Prevent duplicate topics
is_duplicate_topic <- function(title, existing_titles) {
  any(sapply(existing_titles, function(t) {
    stringdist(tolower(title), tolower(t), method = "jw") < 0.2
  }))
}

# =====================
# 🔥 FETCH NEWS (FIXED CORE)
# =====================
fetch_google <- function(player_name) {

  query <- paste(player_name, "NFL")

  url <- paste0(
    "https://news.google.com/rss/search?q=",
    URLencode(query),
    "&hl=en-US&gl=US&ceid=US:en"
  )

  xml <- tryCatch(read_xml(url), error = function(e) NULL)
  if (is.null(xml)) return(list())

  items <- xml_find_all(xml, "//item")

  articles <- list()

  for (item in items) {

    title <- xml_text(xml_find_first(item, "title"))
    link  <- xml_text(xml_find_first(item, "link"))
    pub   <- xml_text(xml_find_first(item, "pubDate"))

    parsed <- safe_parse_date(pub)

    # ❗ CRITICAL FIX:
    # Don't drop if NA (Google sometimes fails parsing)
    if (!is.na(parsed) && parsed < SEASON_START) next

    clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))

    articles <- c(articles, list(list(
      title = clean_title,
      summary = str_trunc(clean_title, 160),
      link = link,
      published = as.character(parsed),
      player = player_name,
      impact = get_impact(clean_title)
    )))
  }

  if (length(articles) == 0) return(list())

  # Sort newest first (NA goes last automatically)
  articles <- articles[order(
    sapply(articles, function(x) safe_parse_date(x$published)),
    decreasing = TRUE,
    na.last = TRUE
  )]

  selected <- list()
  titles <- c()

  # =====================
  # 🔥 PRIORITY SYSTEM
  # =====================

  # 1. Try TODAY first
  for (article in articles) {

    if (length(selected) >= MAX_PER_PLAYER) break

    pub_date <- safe_parse_date(article$published)

    if (!is.na(pub_date) && pub_date != TODAY) next
    if (is_duplicate_topic(article$title, titles)) next

    selected <- c(selected, list(article))
    titles <- c(titles, article$title)
  }

  # 2. Fallback → ANY recent
  if (length(selected) < MAX_PER_PLAYER) {
    for (article in articles) {

      if (length(selected) >= MAX_PER_PLAYER) break
      if (is_duplicate_topic(article$title, titles)) next

      selected <- c(selected, list(article))
      titles <- c(titles, article$title)
    }
  }

  selected
}

# =====================
# BUILD NEWS
# =====================
build_news <- function(player_df) {

  result <- list()

  for (i in seq_len(nrow(player_df))) {

    player_name <- player_df$player_name[i]

    message("Fetching: ", player_name)

    Sys.sleep(REQUEST_DELAY)

    news <- fetch_google(player_name)

    if (length(news) > 0) {
      result <- c(result, news)
    }
  }

  result
}

# =====================
# GENERATE FILES
# =====================
message("Generating QB news...")
qb_news <- build_news(qb_players)

message("Generating RB news...")
rb_news <- build_news(rb_players)

message("Generating WR news...")
wr_news <- build_news(wr_players)

message("Generating TE news...")
te_news <- build_news(te_players)

message("Generating K news...")
k_news <- build_news(k_players)

# =====================
# SAVE FILES
# =====================
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

write_json(qb_news, file.path(OUTPUT_DIR, "news_qb.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(rb_news, file.path(OUTPUT_DIR, "news_rb.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(wr_news, file.path(OUTPUT_DIR, "news_wr.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(te_news, file.path(OUTPUT_DIR, "news_te.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(k_news,  file.path(OUTPUT_DIR, "news_k.json"),  pretty=TRUE, auto_unbox=TRUE)

message("✅ DONE — News generated with real coverage")
