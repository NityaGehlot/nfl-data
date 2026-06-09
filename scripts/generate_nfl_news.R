# =====================
# scripts/generate_nfl_news.R (CLEANED + NEWS FOLDER OUTPUT)
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
OUTPUT_DIR <- "data/news"
MAX_PER_PLAYER <- 2
REQUEST_DELAY <- 0.3
SEASON_START <- as.Date("2025-09-04")

# =====================
# CHECK FILE (ONLY REQUIRED DEPENDENCY)
# =====================
if (!file.exists("data/sleeperAPI/sleeper_players.json")) {
  stop("❌ sleeper_players.json not found in data/sleeperAPI/")
}

# =====================
# LOAD SLEEPER PLAYERS
# =====================
message("Loading Sleeper players...")

players_raw <- fromJSON(
  "data/sleeperAPI/sleeper_players.json",
  simplifyVector = FALSE
)

# =====================
# SAFE CONVERSION
# =====================
players_list <- lapply(players_raw, function(p) {

  if (is.null(p$first_name) || is.null(p$last_name)) return(NULL)

  depth_raw <- p$depth_chart_order

  depth <- tryCatch(as.numeric(depth_raw), error = function(e) NA_real_)
  if (length(depth) == 0 || is.na(depth)) depth <- 99

  data.frame(
    player_name = paste(p$first_name, p$last_name),
    position = p$position %||% NA,
    team = p$team %||% NA,
    status = p$status %||% NA,
    depth_chart_order = depth,
    stringsAsFactors = FALSE
  )
})

players_list <- Filter(Negate(is.null), players_list)
players <- bind_rows(players_list)

message("Total players loaded: ", nrow(players))

# =====================
# CLEAN DATA
# =====================
players <- players %>%
  filter(!is.na(position), !is.na(player_name), !is.na(team)) %>%
  filter(status == "Active" | is.na(status))

message("After cleaning: ", nrow(players))

# =====================
# DEPTH FILTER
# =====================
players <- players %>%
  filter(
    (position == "QB" & depth_chart_order <= 2) |
    (position == "RB" & depth_chart_order <= 3) |
    (position == "WR" & depth_chart_order <= 6) |
    (position == "TE" & depth_chart_order <= 3) |
    (position == "K" & depth_chart_order == 1)
  )

message("After depth filter: ", nrow(players))

# =====================
# SPLIT
# =====================
qb_players <- players %>% filter(position == "QB")
rb_players <- players %>% filter(position == "RB")
wr_players <- players %>% filter(position == "WR")
te_players <- players %>% filter(position == "TE")
k_players  <- players %>% filter(position == "K")

# =====================
# DATE PARSER
# =====================
safe_parse_date <- function(x) {
  tryCatch({
    parsed <- parse_date_time(
      x,
      orders = c(
        "a, d b Y H:M:S z",
        "a, d b Y H:M:S",
        "ymd HMS",
        "ymd HM",
        "Y-m-dTH:M:SZ",
        "Y-m-dTH:M:S",
        "Y-m-d"
      ),
      tz = "UTC"
    )

    if (length(parsed) == 0 || all(is.na(parsed))) return(NA)
    as.Date(parsed[[1]])
  }, error = function(e) NA)
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  t <- tolower(text)

  if (grepl("injur|out|ir|surgery", t)) return("negative")
  if (grepl("questionable|limited", t)) return("slightly_negative")
  if (grepl("signed|trade|contract|released|cut", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|huge", t)) return("positive")

  "neutral"
}

# =====================
# DUPLICATE CHECK
# =====================
is_duplicate_topic <- function(title, existing_titles) {
  any(sapply(existing_titles, function(t) {
    stringdist(tolower(title), tolower(t), method = "jw") < 0.2
  }))
}

# =====================
# FETCH GOOGLE NEWS (RSS ONLY)
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
    pub_raw <- xml_text(xml_find_first(item, "pubDate"))
    desc <- xml_text(xml_find_first(item, "description"))

    parsed <- safe_parse_date(pub_raw)

    if (is.na(parsed)) {
      possible_date <- str_extract(desc, "\\w{3}, \\d{1,2} \\w{3} \\d{4}")
      parsed <- safe_parse_date(possible_date)
    }

    if (is.na(parsed)) parsed <- Sys.Date()
    if (parsed < SEASON_START) next

    articles <- c(articles, list(list(
      title = title,
      summary = str_trunc(title, 160),
      link = link,
      published = as.character(parsed),
      player = player_name,
      impact = get_impact(title)
    )))
  }

  if (length(articles) == 0) return(list())

  articles <- articles[order(
    sapply(articles, function(x) safe_parse_date(x$published)),
    decreasing = TRUE,
    na.last = TRUE
  )]

  selected <- list()
  titles <- c()

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
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

write_json(qb_news, file.path(OUTPUT_DIR, "news_qb.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(rb_news, file.path(OUTPUT_DIR, "news_rb.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(wr_news, file.path(OUTPUT_DIR, "news_wr.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(te_news, file.path(OUTPUT_DIR, "news_te.json"), pretty=TRUE, auto_unbox=TRUE)
write_json(k_news,  file.path(OUTPUT_DIR, "news_k.json"),  pretty=TRUE, auto_unbox=TRUE)

message("✅ DONE — News generated successfully")
