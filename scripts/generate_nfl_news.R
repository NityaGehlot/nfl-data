# =====================
# scripts/generate_nfl_news.R (STABLE PRODUCTION VERSION)
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
OUTPUT_FILE <- "data/nfl_news.json"
MAX_ARTICLES <- 100
HOURS_BACK <- 24 * 7  # 7 days

# =====================
# SAFE HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_parse_date <- function(x) {
  parsed <- tryCatch({
    parse_date_time(x, orders = c(
      "ymd HMS", "ymd HM", "Ymd HMS",
      "a, d b Y H:M:S", "a d b Y H:M:S",
      "Y-m-d\\TH:M:S"
    ), quiet = TRUE)
  }, error = function(e) NA)

  if (length(parsed) == 0) return(NA)
  parsed
}

# =====================
# LOAD SLEEPER PLAYERS (SAFE VERSION)
# =====================
message("Loading Sleeper players...")

sleeper_url <- "https://api.sleeper.app/v1/players/nfl"
players_raw <- fromJSON(sleeper_url, simplifyDataFrame = FALSE)

# keep only valid entries
players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  as.data.frame(as.list(p), stringsAsFactors = FALSE)
}))

# keep only real active NFL players
active_players <- players_df %>%
  filter(!is.na(status)) %>%
  filter(status == "Active") %>%
  filter(position %in% c("QB","RB","WR","TE","K","DEF")) %>%
  mutate(
    full_name = tolower(paste(first_name, last_name))
  ) %>%
  filter(!is.na(full_name) & full_name != "")

player_names <- unique(active_players$full_name)

message(paste("Active players loaded:", length(player_names)))

# =====================
# PLAYER DETECTION (SAFE + STRICT)
# =====================
detect_players <- function(text) {
  text <- tolower(text)

  matched <- player_names[sapply(player_names, function(name) {
    grepl(paste0("\\b", name, "\\b"), text)
  })]

  unique(matched)
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  text <- tolower(text)

  if (grepl("injury|out|surgery|ir|doubtful", text)) {
    "negative"
  } else if (grepl("questionable|limited|monitor|day-to-day", text)) {
    "slightly_negative"
  } else if (grepl("breakout|dominant|huge|career-high|impressive|star", text)) {
    "positive"
  } else {
    "neutral"
  }
}

# =====================
# FILTER BAD CONTENT
# =====================
is_relevant <- function(text) {
  text <- tolower(text)

  !grepl("mock draft|college|prospect|simulation|2026 nfl draft", text)
}

# =====================
# ESPN NEWS
# =====================
fetch_espn <- function() {

  message("Fetching ESPN...")

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

    if (title == "" || link == "") return(NULL)

    text <- paste(title, desc)

    if (!is_relevant(text)) return(NULL)

    list(
      title = title,
      summary = str_trunc(ifelse(desc != "", desc, title), 140),
      link = link,
      published = published,
      source = "ESPN",
      players_mentioned = detect_players(text),
      impact = get_impact(text)
    )
  })
}

# =====================
# GOOGLE NEWS RSS (STRONG COVERAGE BOOST)
# =====================
fetch_google <- function() {

  message("Fetching Google News RSS...")

  url <- "https://news.google.com/rss/search?q=NFL+football&hl=en-US&gl=US&ceid=US:en"

  xml <- read_xml(url)
  items <- xml_find_all(xml, "//item")

  lapply(items, function(item) {

    title <- xml_text(xml_find_first(item, "title"))
    link <- xml_text(xml_find_first(item, "link"))
    pub <- xml_text(xml_find_first(item, "pubDate"))

    if (title == "") return(NULL)

    if (!is_relevant(title)) return(NULL)

    list(
      title = title,
      summary = str_trunc(title, 140),
      link = link,
      published = pub,
      source = "GoogleNews",
      players_mentioned = detect_players(title),
      impact = get_impact(title)
    )
  })
}

# =====================
# RUN SOURCES
# =====================
news <- c(
  fetch_espn(),
  fetch_google()
)

news <- Filter(Negate(is.null), news)

message(paste("Total raw articles:", length(news)))

# =====================
# FILTER RECENT (SAFE)
# =====================
cutoff <- Sys.time() - hours(HOURS_BACK)

news <- Filter(function(x) {

  parsed <- safe_parse_date(x$published)

  if (is.na(parsed)) return(FALSE)

  parsed >= cutoff

}, news)

# =====================
# REMOVE EMPTY PLAYER MATCHES (BUT NOT TOO STRICT)
# =====================
news <- Filter(function(x) {
  !is.null(x$title) && x$title != ""
}, news)

# =====================
# SORT BY IMPACT
# =====================
impact_priority <- c(
  "negative" = 3,
  "slightly_negative" = 2,
  "neutral" = 1,
  "positive" = 0
)

news <- news[order(
  sapply(news, function(x) impact_priority[x$impact]),
  decreasing = TRUE
)]

# =====================
# LIMIT
# =====================
news <- head(news, MAX_ARTICLES)

# =====================
# FALLBACK (PREVENT EMPTY JSON)
# =====================
if (length(news) == 0) {
  message("⚠️ No filtered news found — adding fallback ESPN items")

  news <- list(list(
    title = "No recent NFL news available",
    summary = "System fallback entry",
    link = "https://www.espn.com/nfl/",
    published = as.character(Sys.time()),
    source = "SYSTEM",
    players_mentioned = character(0),
    impact = "neutral"
  ))
}

# =====================
# SAVE JSON
# =====================
if (!dir.exists("data")) dir.create("data")

write_json(news, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)

message("✅ NFL news generated successfully!")
