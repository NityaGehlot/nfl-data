# =====================
# scripts/generate_nfl_news.R (FINAL STABLE VERSION)
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)
library(tibble)

# =====================
# CONFIG
# =====================
OUTPUT_DIR <- "data"
MAX_PER_PLAYER <- 3
REQUEST_DELAY <- 1

SEASON_START <- as.POSIXct("2025-09-04", tz = "UTC")
NOW_TIME <- Sys.time()

# =====================
# LOAD PLAYER STATS (FIXED)
# =====================
message("Loading player stats...")

raw_stats <- fromJSON("data/player_stats_2025_week17.json", simplifyDataFrame = FALSE)

# Flatten JSON → dataframe
stats <- bind_rows(lapply(raw_stats, function(x) {
  if (length(x) > 0) {
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    df$fantasy_points_ppr <- as.numeric(df$fantasy_points_ppr)
    return(df)
  }
}))

# Clean
stats <- stats %>%
  filter(!is.na(player_name), !is.na(position))

# Sort by best fantasy players
stats <- stats %>%
  arrange(desc(fantasy_points_ppr))

# =====================
# LIMIT TO RELEVANT PLAYERS
# =====================
qb_players <- stats %>% filter(position == "QB") %>% slice_head(n = 20)
rb_players <- stats %>% filter(position == "RB") %>% slice_head(n = 30)
wr_players <- stats %>% filter(position == "WR") %>% slice_head(n = 40)
te_players <- stats %>% filter(position == "TE") %>% slice_head(n = 20)
k_players  <- stats %>% filter(position == "K")  %>% slice_head(n = 15)

# =====================
# HELPERS
# =====================
safe_parse_date <- function(x) {
  tryCatch({
    parse_date_time(x, orders = c(
      "a, d b Y H:M:S z",
      "ymd HMS",
      "Y-m-dTH:M:SZ"
    ), tz = "UTC")
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

# =====================
# FETCH GOOGLE NEWS
# =====================
fetch_google <- function(player_name) {

  query <- paste(player_name, "NFL injury OR fantasy OR update OR news")

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

    # STRICT DATE FILTER
    if (!is.na(parsed) && (parsed < SEASON_START || parsed > NOW_TIME)) next

    clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))

    articles <- c(articles, list(list(
      title = clean_title,
      summary = str_trunc(clean_title, 160),
      link = link,
      published = pub,
      player = player_name,
      impact = get_impact(clean_title)
    )))
  }

  # Sort by newest first
  articles <- articles[order(
    sapply(articles, function(x) safe_parse_date(x$published)),
    decreasing = TRUE
  )]

  # Return top N per player
  head(articles, MAX_PER_PLAYER)
}

# =====================
# BUILD NEWS PER POSITION
# =====================
build_news <- function(player_df) {

  result <- list()

  for (i in 1:nrow(player_df)) {

    player_name <- player_df$player_name[i]

    message("Fetching:", player_name)

    Sys.sleep(REQUEST_DELAY)

    news <- fetch_google(player_name)

    # Only keep players that actually have news
    if (length(news) >= 1) {
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

message("✅ DONE")
