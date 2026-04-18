# =====================
# scripts/generate_nfl_news.R
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(nflreadr)

# =====================
# SAFE NULL HELPER
# =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =====================
# CONFIG
# =====================
OUTPUT_FILE <- "data/nfl_news.json"
MAX_ARTICLES <- 50
HOURS_BACK <- 48

# =====================
# LOAD PLAYER DATA
# =====================
message("Loading player data...")

players <- nflreadr::load_players()

if(!"display_name" %in% names(players)) players$display_name <- ""

player_names <- players %>%
  filter(!is.na(display_name), display_name != "") %>%
  pull(display_name) %>%
  tolower()

# =====================
# DETECT PLAYERS
# =====================
detect_players <- function(text) {
  text <- tolower(text)

  matched <- player_names[sapply(player_names, function(name) {
    grepl(name, text, fixed = TRUE)
  })]

  unique(matched)
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  text <- tolower(text)

  if (grepl("out|injured|injury|doubtful|ruled out", text)) {
    return("negative")
  } else if (grepl("questionable|limited|monitor", text)) {
    return("slightly_negative")
  } else if (grepl("breakout|strong|impressive|dominant|huge game", text)) {
    return("positive")
  } else {
    return("neutral")
  }
}

# =====================
# FETCH ESPN NEWS
# =====================
message("Fetching ESPN news...")

url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/news"

response <- GET(url)

if (response$status_code != 200) {
  stop("Failed to fetch ESPN news")
}

data <- fromJSON(content(response, "text", encoding = "UTF-8"))

articles <- data$articles

# FORCE SAFE LIST FORMAT
if (is.data.frame(articles)) {
  articles <- split(articles, seq(nrow(articles)))
}

# =====================
# PROCESS ARTICLES
# =====================
message("Processing articles...")

cleaned <- lapply(articles, function(a) {

  # SAFETY CHECK (fix your crash)
  if (is.null(a) || length(a) == 0) return(NULL)

  title <- tryCatch(a$headline %||% "", error = function(e) "")
  desc  <- tryCatch(a$description %||% "", error = function(e) "")
  published <- tryCatch(a$published %||% "", error = function(e) "")

  link <- ""
  if (!is.null(a$links) && !is.null(a$links$web) && !is.null(a$links$web$href)) {
    link <- a$links$web$href
  }

  combined_text <- paste(title, desc)

  players_found <- detect_players(combined_text)

  summary_text <- ifelse(
    desc != "",
    str_trunc(desc, 120),
    str_trunc(title, 120)
  )

  impact <- get_impact(combined_text)

  list(
    title = title,
    summary = summary_text,
    link = link,
    published = published,
    source = "ESPN",
    players_mentioned = players_found,
    impact = impact
  )
})

cleaned <- Filter(Negate(is.null), cleaned)

# =====================
# REMOVE EMPTY ARTICLES
# =====================
cleaned <- cleaned[sapply(cleaned, function(x) {
  x$title != "" && x$link != ""
})]

# =====================
# KEEP ONLY PLAYER-RELEVANT NEWS
# =====================
cleaned <- cleaned[sapply(cleaned, function(x) {
  length(x$players_mentioned) > 0
})]

# =====================
# FILTER LAST 48 HOURS
# =====================
message("Filtering recent news...")

cutoff <- Sys.time() - hours(HOURS_BACK)

cleaned <- cleaned[sapply(cleaned, function(x) {

  if (x$published == "") return(FALSE)

  parsed <- tryCatch(
    ymd_hms(x$published),
    error = function(e) return(NA)
  )

  if (is.na(parsed)) return(FALSE)

  parsed >= cutoff
})]

# =====================
# SORT BY IMPACT
# =====================
message("Sorting by importance...")

impact_priority <- c(
  "negative" = 3,
  "slightly_negative" = 2,
  "neutral" = 1,
  "positive" = 0
)

cleaned <- cleaned[order(
  sapply(cleaned, function(x) impact_priority[x$impact]),
  decreasing = TRUE
)]

# =====================
# LIMIT TOTAL ARTICLES
# =====================
cleaned <- head(cleaned, MAX_ARTICLES)

# =====================
# SAVE JSON
# =====================
if(!dir.exists("data")) dir.create("data")

write_json(
  cleaned,
  OUTPUT_FILE,
  pretty = TRUE,
  auto_unbox = TRUE
)

message("✅ NFL news JSON generated at: ", OUTPUT_FILE)
