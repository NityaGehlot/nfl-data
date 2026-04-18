# =====================
# scripts/generate_nfl_news.R (UPGRADED VERSION)
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(nflreadr)
library(xml2)

# =====================
# SAFE NULL HELPER
# =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =====================
# CONFIG
# =====================
OUTPUT_FILE <- "data/nfl_news.json"

MAX_ARTICLES <- 100
HOURS_BACK <- 168  # 7 days

# =====================
# LOAD ACTIVE NFL PLAYERS (SLEEPER API)
# =====================
message("Loading ACTIVE NFL players (Sleeper)...")

sleeper_url <- "https://api.sleeper.app/v1/players/nfl"
players_raw <- fromJSON(sleeper_url)

message("Loading ACTIVE NFL players (Sleeper)...")

sleeper_url <- "https://api.sleeper.app/v1/players/nfl"
players_raw <- fromJSON(sleeper_url, simplifyDataFrame = FALSE)

# REMOVE NULL / EMPTY ENTRIES (THIS FIXES YOUR ERROR)
players_clean <- players_raw[!sapply(players_raw, is.null)]
players_clean <- players_clean[sapply(players_clean, function(x) length(x) > 0)]

# SAFE CONVERSION
players_df <- bind_rows(lapply(players_clean, function(x) {
  as.data.frame(t(unlist(x)), stringsAsFactors = FALSE)
}))

active_players <- players_df %>%
  filter(status == "Active") %>%
  filter(position %in% c("QB","RB","WR","TE","K","DEF")) %>%
  mutate(
    full_name = tolower(paste(first_name, last_name))
  )

player_names <- active_players$full_name

# =====================
# IMPROVED PLAYER DETECTION
# =====================
detect_players <- function(text) {
  text <- tolower(text)

  matched <- player_names[sapply(player_names, function(name) {

    parts <- strsplit(name, " ")[[1]]
    last_name <- tail(parts, 1)

    # STRICT matching (full name OR last name but word-bounded)
    full_match <- grepl(paste0("\\b", name, "\\b"), text)
    last_match <- grepl(paste0("\\b", last_name, "\\b"), text)

    full_match | last_match
  })]

  unique(matched)
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  text <- tolower(text)

  if (grepl("out|injury|injured|doubtful|surgery|ir|pup", text)) {
    return("negative")
  } else if (grepl("questionable|limited|day-to-day|monitor", text)) {
    return("slightly_negative")
  } else if (grepl("breakout|huge|dominant|career-high|impressive|star", text)) {
    return("positive")
  } else {
    return("neutral")
  }
}

# =====================
# FILTER BAD CONTENT
# =====================
is_relevant_news <- function(text) {
  text <- tolower(text)

  !grepl("mock draft|draft profile|college|prospect|2026 nfl draft|simulation", text)
}

# =====================
# ESPN NEWS FETCH
# =====================
fetch_espn <- function() {

  message("Fetching ESPN news...")

  url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/news"
  res <- GET(url)

  if (res$status_code != 200) return(list())

  data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  articles <- data$articles

  if (is.data.frame(articles)) {
    articles <- split(articles, seq(nrow(articles)))
  }

  lapply(articles, function(a) {

    title <- a$headline %||% ""
    desc <- a$description %||% ""
    link <- a$links$web$href %||% ""
    published <- a$published %||% ""

    text <- paste(title, desc)

    if (!is_relevant_news(text)) return(NULL)

    list(
      title = title,
      summary = str_trunc(desc %||% title, 140),
      link = link,
      published = published,
      source = "ESPN",
      players_mentioned = detect_players(text),
      impact = get_impact(text)
    )
  })
}

# =====================
# GOOGLE NEWS RSS (BIG BOOST IN COVERAGE)
# =====================
fetch_google_news <- function() {

  message("Fetching Google News RSS...")

  base_url <- "https://news.google.com/rss/search?q=NFL+football&hl=en-US&gl=US&ceid=US:en"

  xml <- read_xml(base_url)
  items <- xml_find_all(xml, "//item")

  lapply(items, function(item) {

    title <- xml_text(xml_find_first(item, "title"))
    link <- xml_text(xml_find_first(item, "link"))
    pub <- xml_text(xml_find_first(item, "pubDate"))

    text <- title

    if (!is_relevant_news(text)) return(NULL)

    list(
      title = title,
      summary = str_trunc(title, 140),
      link = link,
      published = pub,
      source = "GoogleNews",
      players_mentioned = detect_players(text),
      impact = get_impact(text)
    )
  })
}

# =====================
# RUN SOURCES
# =====================
all_news <- c(
  fetch_espn(),
  fetch_google_news()
)

all_news <- Filter(Negate(is.null), all_news)

# =====================
# REMOVE EMPTY ENTRIES
# =====================
all_news <- Filter(function(x) {
  !is.null(x$title) && x$title != "" &&
    !is.null(x$link) && x$link != ""
}, all_news)

# =====================
# FILTER RECENT NEWS
# =====================
message("Filtering recent news...")

cutoff <- Sys.time() - hours(HOURS_BACK)

all_news <- Filter(function(x) {

  parsed <- tryCatch(
    parse_date_time(x$published, orders = c("ymd HMS", "ymd HM", "a b d Y H:M:S")),
    error = function(e) NA
  )

  if (is.na(parsed)) return(FALSE)

  parsed >= cutoff

}, all_news)

# =====================
# SORT BY IMPACT
# =====================
impact_priority <- c(
  "negative" = 3,
  "slightly_negative" = 2,
  "neutral" = 1,
  "positive" = 0
)

all_news <- all_news[order(
  sapply(all_news, function(x) impact_priority[x$impact]),
  decreasing = TRUE
)]

# =====================
# LIMIT RESULTS
# =====================
all_news <- head(all_news, MAX_ARTICLES)

# =====================
# SAVE JSON
# =====================
if (!dir.exists("data")) dir.create("data")

write_json(
  all_news,
  OUTPUT_FILE,
  pretty = TRUE,
  auto_unbox = TRUE
)

message("✅ NFL news generated successfully!")
