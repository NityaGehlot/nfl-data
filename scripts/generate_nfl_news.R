# =====================
# scripts/generate_nfl_news.R (BALANCED + DEF SUPPORT)
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
MAX_ARTICLES <- 220

SEASON_START <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
NOW_TIME     <- Sys.time()

REQUEST_DELAY <- 1

# =====================
# 🔥 POSITION LIMITS (EDIT THIS)
# =====================
POSITION_LIMITS <- list(
  QB  = 32,
  RB  = 40,
  WR  = 50,
  TE  = 25,
  K   = 32,
  DEF = 32   # includes DST
)

# =====================
# HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_parse_date <- function(x) {
  tryCatch({
    parse_date_time(x, orders = c(
      "a, d b Y H:M:S z",
      "ymd HMS",
      "Y-m-dTH:M:SZ"
    ), tz = "UTC")
  }, error = function(e) NA)
}

# =====================
# LOAD PLAYERS (SLEEPER)
# =====================
message("Loading players...")

players_raw <- fromJSON(
  "https://api.sleeper.app/v1/players/nfl",
  simplifyDataFrame = FALSE
)

players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  tibble::tibble(
    first_name = p$first_name %||% NA,
    last_name  = p$last_name  %||% NA,
    status     = p$status     %||% NA,
    position   = p$position   %||% NA
  )
}))

# =====================
# FILTER ACTIVE PLAYERS
# =====================
active_players <- players_df %>%
  filter(!is.na(status), status == "Active") %>%
  filter(position %in% c("QB","RB","WR","TE","K","DEF","DST")) %>%
  mutate(
    full_name = tolower(paste(first_name, last_name)),
    display_name = paste(first_name, last_name)
  ) %>%
  filter(!is.na(full_name), full_name != "")

# =====================
# ⭐ STAR PLAYERS
# =====================
star_players <- c(
  "caleb williams","joe burrow","patrick mahomes","josh allen",
  "jalen hurts","justin jefferson","ja'marr chase",
  "christian mccaffrey","bijan robinson","travis kelce",
  "cee dee lamb","amon-ra st. brown","aj brown"
)

# =====================
# 🔥 BALANCED POSITION SAMPLING
# =====================
players_by_position <- bind_rows(lapply(names(POSITION_LIMITS), function(pos) {

  limit <- POSITION_LIMITS[[pos]]

  active_players %>%
    filter(
      position == pos |
      (pos == "DEF" & position %in% c("DEF","DST"))
    ) %>%
    slice_head(n = limit)

}))

balanced_players <- players_by_position$display_name

# =====================
# PLAYER LOOKUP (FIX)
# =====================
player_lookup <- setNames(active_players$display_name, active_players$full_name)
player_names  <- names(player_lookup)

# =====================
# DETECT PLAYERS
# =====================
detect_players <- function(text) {

  if (is.null(text) || text == "") return(character(0))

  text <- tolower(text)

  matched <- player_names[sapply(player_names, function(nm) {
    grepl(paste0("\\b", nm, "\\b"), text)
  })]

  unique(unname(player_lookup[matched]))
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  t <- tolower(text)

  if (grepl("injur|out|ir|surgery", t)) return("negative")
  if (grepl("questionable|limited", t)) return("slightly_negative")
  if (grepl("signed|trade|contract|released", t)) return("roster_move")
  if (grepl("huge|breakout|dominant|career-high", t)) return("positive")

  "neutral"
}

# =====================
# GOOGLE NEWS FETCH
# =====================
fetch_google <- function(query) {

  url <- paste0(
    "https://news.google.com/rss/search?q=",
    URLencode(query),
    "&hl=en-US&gl=US&ceid=US:en"
  )

  xml <- tryCatch(read_xml(url), error = function(e) NULL)
  if (is.null(xml)) return(list())

  items <- xml_find_all(xml, "//item")

  lapply(items, function(item) {

    title <- xml_text(xml_find_first(item, "title"))
    link  <- xml_text(xml_find_first(item, "link"))
    pub   <- xml_text(xml_find_first(item, "pubDate"))

    parsed <- safe_parse_date(pub)

    # ✅ SEASON FILTER
    if (!is.na(parsed) && parsed < SEASON_START) return(NULL)

    clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))

    list(
      title = clean_title,
      summary = str_trunc(clean_title, 180),
      link = link,
      published = pub,
      source = "GoogleNews",
      players_mentioned = detect_players(clean_title),
      impact = get_impact(clean_title)
    )
  })
}

# =====================
# QUERY BUILDER
# =====================
build_query <- function(name) {
  paste(name, "NFL injury OR fantasy OR update OR performance OR news")
}

# =====================
# OPTIONAL TEAM DEFENSE QUERIES
# =====================
nfl_teams <- c(
  "Chicago Bears","Kansas City Chiefs","Buffalo Bills",
  "San Francisco 49ers","Philadelphia Eagles","Dallas Cowboys",
  "Miami Dolphins","Baltimore Ravens","Detroit Lions"
)

team_queries <- paste(nfl_teams, "defense NFL news")

# =====================
# FETCH NEWS
# =====================
message("Fetching player news...")

queries <- unique(c(
  sapply(star_players, build_query),
  sapply(balanced_players, build_query),
  team_queries
))

all_news <- list()

for (q in queries) {
  message("Query:", q)
  Sys.sleep(REQUEST_DELAY)

  res <- fetch_google(q)
  all_news <- c(all_news, res)
}

# =====================
# CLEAN
# =====================
all_news <- Filter(Negate(is.null), all_news)

all_news <- Filter(function(x) {
  x$title != "" && x$link != ""
}, all_news)

# =====================
# DATE FILTER
# =====================
all_news <- Filter(function(x) {

  parsed <- safe_parse_date(x$published)
  if (is.na(parsed)) return(TRUE)

  parsed >= SEASON_START && parsed <= NOW_TIME

}, all_news)

# =====================
# DEDUPLICATE
# =====================
seen <- c()

all_news <- Filter(function(x) {

  key <- tolower(x$title)

  if (key %in% seen) return(FALSE)

  seen <<- c(seen, key)
  TRUE

}, all_news)

# =====================
# SORT
# =====================
priority <- c(
  "negative" = 5,
  "slightly_negative" = 4,
  "roster_move" = 3,
  "positive" = 2,
  "neutral" = 1
)

all_news <- all_news[order(
  sapply(all_news, function(x) priority[x$impact]),
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
  all_news <- list(list(
    title = "No recent NFL news found",
    summary = "Try again later.",
    link = "https://www.espn.com/nfl/",
    published = Sys.time(),
    source = "SYSTEM",
    players_mentioned = character(0),
    impact = "neutral"
  ))
}

# =====================
# SAVE
# =====================
if (!dir.exists("data")) dir.create("data")

write_json(
  all_news,
  OUTPUT_FILE,
  pretty = TRUE,
  auto_unbox = TRUE
)

message("✅ DONE — articles:", length(all_news))
