# =====================
# scripts/generate_nfl_news.R (FINAL WORKING VERSION)
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)
library(stringdist)

# =====================
# HELPER
# =====================
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# =====================
# CONFIG
# =====================
OUTPUT_DIR <- "data"
MAX_PER_PLAYER <- 2
REQUEST_DELAY <- 0.3   # faster

SEASON_START <- Sys.Date() - 7   # 🔥 last 7 days instead of season start

# =====================
# CHECK FILE
# =====================
if (!file.exists("data/sleeper_players.json")) {
  stop("❌ sleeper_players.json not found. Run update_players.R first.")
}

# =====================
# LOAD PLAYERS
# =====================
message("Loading Sleeper players...")

players_raw <- fromJSON(
  "data/sleeper_players.json",
  simplifyVector = FALSE
)

players_list <- lapply(players_raw, function(p) {

  if (is.null(p$first_name) || is.null(p$last_name)) return(NULL)

  depth <- p$depth_chart_position
  if (is.null(depth) || length(depth) == 0) depth <- NA

  depth <- suppressWarnings(as.numeric(depth))

  data.frame(
    player_name = paste(p$first_name, p$last_name),
    position = p$position %||% NA,
    status = p$status %||% NA,
    depth_chart_position = depth,
    stringsAsFactors = FALSE
  )
})

players_list <- Filter(Negate(is.null), players_list)
players <- bind_rows(players_list)

message("Total players loaded: ", nrow(players))

# =====================
# FIX DEPTH NAs
# =====================
players$depth_chart_position[is.na(players$depth_chart_position)] <- 99

# =====================
# CLEAN
# =====================
players <- players %>%
  filter(
    status == "Active",
    !is.na(position),
    !is.na(player_name)
  )

# =====================
# 🔥 LESS STRICT FILTER (IMPORTANT)
# =====================
players <- players %>%
  filter(
    (position == "QB" & depth_chart_position <= 3) |
    (position == "RB" & depth_chart_position <= 4) |
    (position == "WR" & depth_chart_position <= 5) |
    (position == "TE" & depth_chart_position <= 3) |
    (position == "K"  & depth_chart_position <= 1)
  )

message("Players after depth filter: ", nrow(players))

# =====================
# SPLIT
# =====================
qb_players <- players %>% filter(position == "QB")
rb_players <- players %>% filter(position == "RB")
wr_players <- players %>% filter(position == "WR")
te_players <- players %>% filter(position == "TE")
k_players  <- players %>% filter(position == "K")

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

is_duplicate_topic <- function(title, existing_titles) {
  any(sapply(existing_titles, function(t) {
    stringdist(tolower(title), tolower(t), method = "jw") < 0.2
  }))
}

# =====================
# 🔥 FETCH NEWS (FIXED)
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

    # 🔥 ONLY last 7 days
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

  # Sort newest first
  articles <- articles[order(
    sapply(articles, function(x) safe_parse_date(x$published)),
    decreasing = TRUE,
    na.last = TRUE
  )]

  selected <- list()
  titles <- c()

  # 🔥 TAKE FIRST UNIQUE ARTICLES (NO "TODAY ONLY")
  for (article in articles) {

    if (length(selected) >= MAX_PER_PLAYER) break
    if (is_duplicate_topic(article$title, titles)) next

    selected <- c(selected, list(article))
    titles <- c(titles, article$title)
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
# GENERATE
# =====================
qb_news <- build_news(qb_players)
rb_news <- build_news(rb_players)
wr_news <- build_news(wr_players)
te_news <- build_news(te_players)
k_news  <- build_news(k_players)

# =====================
# SAVE
# =====================
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

write_json(qb_news, file.path(OUTPUT_DIR, "news_qb.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(rb_news, file.path(OUTPUT_DIR, "news_rb.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(wr_news, file.path(OUTPUT_DIR, "news_wr.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(te_news, file.path(OUTPUT_DIR, "news_te.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(k_news,  file.path(OUTPUT_DIR, "news_k.json"),  pretty=TRUE, auto_unbox=TRUE)

message("✅ DONE — News generated successfully")
