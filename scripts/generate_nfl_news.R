# =====================
# scripts/generate_nfl_news.R (FINAL PRODUCTION VERSION)
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
MAX_ARTICLES <- 150

# 🔥 NFL SEASON WINDOW
SEASON_START <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
NOW_TIME     <- Sys.time()

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
# LOAD ACTIVE PLAYERS (SLEEPER) — FIXED
# =====================
message("Loading players...")

players_raw <- fromJSON(
  "https://api.sleeper.app/v1/players/nfl",
  simplifyDataFrame = FALSE
)

# Remove null entries
players_raw <- players_raw[!sapply(players_raw, is.null)]

# 🔥 SAFE FLATTEN (guarantees columns exist)
players_df <- bind_rows(lapply(players_raw, function(p) {

  tibble::tibble(
    first_name = p$first_name %||% NA,
    last_name  = p$last_name  %||% NA,
    status     = p$status     %||% NA,
    position   = p$position   %||% NA
  )

}))

# =====================
# FILTER ACTIVE FANTASY PLAYERS
# =====================
active_players <- players_df %>%
  filter(!is.na(status), status == "Active") %>%
  filter(position %in% c("QB","RB","WR","TE")) %>%
  mutate(
    full_name = tolower(paste(first_name, last_name)),
    display_name = paste(first_name, last_name)
  ) %>%
  filter(!is.na(full_name), full_name != "")

# =====================
# ⭐ FORCE STAR PLAYERS
# =====================
star_players <- c(
  "caleb williams",
  "joe burrow",
  "patrick mahomes",
  "josh allen",
  "jalen hurts",
  "justin jefferson",
  "ja'marr chase",
  "christian mccaffrey",
  "bijan robinson",
  "travis kelce"
)

# =====================
# PLAYER DETECTION
# =====================
detect_players <- function(text) {
  text <- tolower(text)

  matched <- player_names[sapply(player_names, function(nm) {
    grepl(paste0("\\b", nm, "\\b"), text)
  })]

  unique(unname(player_lookup[matched]))
}

# =====================
# IMPACT
# =====================
get_impact <- function(text) {
  t <- tolower(text)

  if (grepl("injur|out|ir|surgery", t)) return("negative")
  if (grepl("questionable|limited", t)) return("slightly_negative")
  if (grepl("signed|trade|contract|released", t)) return("roster_move")
  if (grepl("huge|breakout|dominant", t)) return("positive")

  "neutral"
}

# =====================
# GOOGLE RSS FETCH
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

    # 🔥 SEASON FILTER
    if (!is.na(parsed) && parsed < SEASON_START) return(NULL)

    text <- title

    list(
      title = str_trim(str_replace(title, "\\s*-\\s*[^-]+$", "")),
      summary = str_trunc(title, 180),
      link = link,
      published = pub,
      source = "GoogleNews",
      players_mentioned = detect_players(text),
      impact = get_impact(text)
    )
  })
}

# =====================
# QUERY BUILDER
# =====================
build_player_query <- function(name) {
  paste(name, "NFL injury OR update OR performance OR news")
}

# =====================
# FETCH PLAYER NEWS
# =====================
message("Fetching player news...")

queries <- c(
  # 🔥 STAR PLAYERS FIRST
  sapply(star_players, build_player_query),

  # Top active players
  sapply(head(active_players$display_name, 50), build_player_query)
)

all_news <- list()

for (q in queries) {
  message("Query:", q)
  Sys.sleep(1)

  res <- fetch_google(q)
  all_news <- c(all_news, res)
}

# =====================
# CLEAN
# =====================
all_news <- Filter(Negate(is.null), all_news)

# remove empty
all_news <- Filter(function(x) {
  x$title != "" && x$link != ""
}, all_news)

# =====================
# DATE FILTER (STRICT SEASON)
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
# SORT (IMPACT PRIORITY)
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
# 🔥 KEEP ONLY MOST RECENT (ROLLING WINDOW)
# =====================
all_news <- head(all_news, MAX_ARTICLES)

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
