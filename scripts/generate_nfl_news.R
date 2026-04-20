# =====================
# scripts/generate_nfl_news.R (FANTASY-CAPPED + RECENCY STRICT + BALANCED)
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
OUTPUT_FILE  <- "data/nfl_news.json"
MAX_ARTICLES <- 220

SEASON_START <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
NOW_TIME     <- Sys.time()

REQUEST_DELAY <- 1
MAX_PER_PLAYER <- 3

# =====================
# POSITION LIMITS
# =====================
POSITION_LIMITS <- list(
  QB  = 25,
  RB  = 35,
  WR  = 45,
  TE  = 25,
  K   = 15,
  DEF = 25
)

# =====================
# HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_parse_date <- function(x) {
  tryCatch({
    parse_date_time(x,
      orders = c(
        "a, d b Y H:M:S z",
        "ymd HMS",
        "Y-m-dTH:M:SZ"
      ),
      tz = "UTC"
    )
  }, error = function(e) NA)
}

# =====================
# STRICT FANTASY FILTER
# =====================
is_fantasy_relevant <- function(text) {

  t <- tolower(text)

  keep <- grepl(
    paste(
      "injur|out|ir|pup|questionable|limited|practice",
      "trade|traded|signed|sign|contract|extension|release|cut|waiver",
      "depth chart|starter|backup|role|snap|usage|carry|target",
      "breakout|dominant|career-high|emerging|bounce back",
      "training camp|ota|minicamp",
      "fantasy|projection|ranking|sleepers",
      sep = "|"
    ),
    t
  )

  remove <- grepl(
    paste(
      "should they|should the|debate|opinion|what we learned",
      "game recap|final score|highlights|reaction|watch",
      "all-time|history of|flashback|top \\d+",
      sep = "|"
    ),
    t
  )

  keep && !remove
}

# =====================
# LOAD PLAYERS
# =====================
message("Loading players...")

players_raw <- fromJSON(
  "https://api.sleeper.app/v1/players/nfl",
  simplifyDataFrame = FALSE
)

players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  tibble(
    first_name = p$first_name %||% NA,
    last_name  = p$last_name %||% NA,
    status     = p$status %||% NA,
    position   = p$position %||% NA
  )
}))

active_players <- players_df %>%
  filter(!is.na(status), status == "Active") %>%
  filter(position %in% c("QB","RB","WR","TE","K","DEF","DST")) %>%
  mutate(
    full_name = tolower(paste(first_name, last_name)),
    display_name = paste(first_name, last_name)
  ) %>%
  filter(!is.na(full_name), full_name != "")

# =====================
# STAR PLAYERS
# =====================
star_players <- c(
  "caleb williams","joe burrow","patrick mahomes","josh allen",
  "jalen hurts","justin jefferson","ja'marr chase",
  "christian mccaffrey","bijan robinson","travis kelce"
)

# =====================
# POSITION BALANCE
# =====================
players_by_position <- bind_rows(lapply(names(POSITION_LIMITS), function(pos) {

  limit <- POSITION_LIMITS[[pos]]

  active_players %>%
    filter(position == pos | (pos == "DEF" & position %in% c("DEF","DST"))) %>%
    slice_head(n = limit)

}))

balanced_players <- unique(players_by_position$display_name)

# =====================
# LOOKUP TABLE
# =====================
player_lookup <- setNames(active_players$display_name, active_players$full_name)
player_names  <- names(player_lookup)

# =====================
# PLAYER DETECTION
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
  if (grepl("questionable|limited|practice", t)) return("slightly_negative")
  if (grepl("signed|trade|contract|released|cut", t)) return("roster_move")
  if (grepl("breakout|dominant|career-high|starter", t)) return("positive")

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

    # 🔥 HARD DATE FILTER (FIXES OLD NEWS ISSUE)
    if (!is.na(parsed) && parsed < SEASON_START) return(NULL)

    clean_title <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))

    if (!is_fantasy_relevant(clean_title)) return(NULL)

    list(
      title = clean_title,
      summary = str_trunc(clean_title, 160),
      link = link,
      published = parsed,
      impact = get_impact(clean_title),
      players_mentioned = detect_players(clean_title),
      raw_date = parsed
    )
  })
}

# =====================
# QUERY BUILDER
# =====================
build_query <- function(name) {
  paste(name, "NFL injury OR trade OR fantasy OR depth chart OR update OR news")
}

nfl_teams <- c(
  "Chicago Bears","Kansas City Chiefs","Buffalo Bills",
  "San Francisco 49ers","Philadelphia Eagles","Dallas Cowboys",
  "Miami Dolphins","Baltimore Ravens","Detroit Lions"
)

team_queries <- paste(nfl_teams, "defense NFL news")

# =====================
# FETCH ALL NEWS
# =====================
message("Fetching news...")

queries <- unique(c(
  sapply(star_players, build_query),
  sapply(balanced_players, build_query),
  team_queries
))

all_news <- list()

for (q in queries) {
  message("Query: ", q)
  Sys.sleep(REQUEST_DELAY)

  res <- fetch_google(q)
  all_news <- c(all_news, res)
}

# =====================
# CLEAN
# =====================
all_news <- Filter(Negate(is.null), all_news)

all_news <- Filter(function(x) !is.null(x$title) && x$title != "", all_news)

# =====================
# FINAL DATE ENFORCEMENT
# =====================
all_news <- Filter(function(x) {
  if (is.na(x$published)) return(FALSE)
  x$published >= SEASON_START && x$published <= NOW_TIME
}, all_news)

# =====================
# GROUP BY PLAYER
# =====================
player_groups <- list()

for (article in all_news) {

  players <- article$players_mentioned

  if (length(players) == 0) next

  for (p in players) {
    player_groups[[p]] <- c(player_groups[[p]], list(article))
  }
}

# =====================
# KEEP ONLY TOP 3 PER PLAYER (RECENT + RELEVANT)
# =====================
final_news <- list()

score_article <- function(a) {
  impact_score <- c(
    "negative" = 5,
    "slightly_negative" = 4,
    "roster_move" = 3,
    "positive" = 2,
    "neutral" = 1
  )[a$impact]

  time_score <- as.numeric(a$published)

  impact_score * 1000000000 + time_score
}

for (player in names(player_groups)) {

  items <- player_groups[[player]]

  items <- items[order(sapply(items, score_article), decreasing = TRUE)]

  final_news <- c(final_news, head(items, MAX_PER_PLAYER))
}

# =====================
# FINAL SORT
# =====================
final_news <- final_news[order(sapply(final_news, score_article), decreasing = TRUE)]

final_news <- head(final_news, MAX_ARTICLES)

# =====================
# SAVE
# =====================
if (!dir.exists("data")) dir.create("data")

write_json(final_news, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)

message("✅ DONE — articles:", length(final_news))
