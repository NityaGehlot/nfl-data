# =====================
# scripts/generate_nfl_news.R (PRODUCTION FIXED + POSITION FILES)
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

SEASON_START <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
NOW_TIME     <- Sys.time()

REQUEST_DELAY <- 1
MAX_PER_PLAYER <- 3

POSITION_LIMITS <- list(
  QB  = 25,
  RB  = 40,
  WR  = 60,
  TE  = 25,
  DEF = 25
)

# =====================
# HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_parse_date <- function(x) {
  tryCatch({
    parse_date_time(x,
      orders = c("a, d b Y H:M:S z","ymd HMS","Y-m-dTH:M:SZ"),
      tz = "UTC"
    )
  }, error = function(e) NA)
}

# =====================
# LOAD PLAYERS
# =====================
message("Loading players...")

players_raw <- fromJSON("https://api.sleeper.app/v1/players/nfl",
                        simplifyDataFrame = FALSE)

players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  tibble(
    first_name = p$first_name %||% NA,
    last_name  = p$last_name %||% NA,
    status     = p$status %||% NA,
    position   = p$position %||% NA,
    fantasy_pts = as.numeric(p$fantasy_points_ppr %||% 0)
  )
}))

# =====================
# KEEP ONLY RELEVANT POSITIONS
# =====================
players_df <- players_df %>%
  filter(position %in% c("QB","RB","WR","TE","DEF","DST")) %>%
  filter(!is.na(first_name), !is.na(last_name))

players_df <- players_df %>%
  mutate(
    full_name = tolower(paste(first_name, last_name)),
    display_name = paste(first_name, last_name)
  )

# =====================
# KEEP ONLY TOP PLAYERS PER POSITION (KEY FIX)
# =====================
active_players <- players_df %>%
  group_by(position) %>%
  arrange(desc(fantasy_pts)) %>%
  slice_head(n = 100) %>%   # big pool first
  ungroup()

# =====================
# STAR PLAYERS
# =====================
star_players <- c(
  "caleb williams","joe burrow","patrick mahomes","josh allen",
  "jalen hurts","justin jefferson","ja'marr chase",
  "christian mccaffrey","bijan robinson","travis kelce"
)

# =====================
# BUILD POSITION GROUPS
# =====================
players_by_position <- bind_rows(lapply(names(POSITION_LIMITS), function(pos) {

  limit <- POSITION_LIMITS[[pos]]

  active_players %>%
    filter(position == pos | (pos == "DEF" & position %in% c("DEF","DST"))) %>%
    slice_head(n = limit)

}))

# =====================
# PLAYER LOOKUP
# =====================
player_lookup <- setNames(active_players$display_name,
                          active_players$full_name)
player_names <- names(player_lookup)

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
# FANTASY FILTER (RELAXED BUT USEFUL)
# =====================
is_fantasy_relevant <- function(text) {
  t <- tolower(text)

  grepl(
    "injur|out|ir|trade|signed|contract|depth chart|starter|
     breakout|practice|role|snap|target|carry|update|news",
    t, perl = TRUE
  )
}

# =====================
# IMPACT
# =====================
get_impact <- function(text) {
  t <- tolower(text)

  if (grepl("injur|out|ir", t)) return("negative")
  if (grepl("trade|signed|contract|release", t)) return("roster_move")
  if (grepl("breakout|starter|dominant", t)) return("positive")

  "neutral"
}

# =====================
# FETCH GOOGLE NEWS
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

    # STRICT DATE FILTER FIRST
    if (!is.na(parsed) && parsed < SEASON_START) return(NULL)

    clean <- str_trim(str_replace(title, "\\s*-\\s*[^-]+$", ""))

    if (!is_fantasy_relevant(clean)) return(NULL)

    list(
      title = clean,
      summary = str_trunc(clean, 160),
      link = link,
      published = parsed,
      players = detect_players(clean),
      impact = get_impact(clean)
    )
  })
}

# =====================
# QUERY BUILDER
# =====================
build_query <- function(name) {
  paste(name, "NFL injury OR trade OR depth chart OR fantasy OR update")
}

# =====================
# FETCH ALL NEWS
# =====================
message("Fetching news...")

queries <- unique(c(
  sapply(star_players, build_query),
  sapply(players_by_position$display_name, build_query)
))

all_news <- list()

for (q in queries) {
  message("Query:", q)
  Sys.sleep(REQUEST_DELAY)
  all_news <- c(all_news, fetch_google(q))
}

all_news <- Filter(Negate(is.null), all_news)

# =====================
# GROUP BY PLAYER
# =====================
grouped <- list()

for (a in all_news) {
  pls <- a$players

  if (length(pls) == 0) next

  for (p in pls) {
    grouped[[p]] <- c(grouped[[p]], list(a))
  }
}

# =====================
# CAP 3 ARTICLES PER PLAYER (KEEP NEWEST)
# =====================
final <- list()

for (p in names(grouped)) {

  items <- grouped[[p]]

  items <- items[order(sapply(items, function(x) x$published),
                       decreasing = TRUE)]

  final <- c(final, head(items, MAX_PER_PLAYER))
}

# =====================
# SPLIT BY POSITION FILES
# =====================
split_by_position <- function(data, pos_list) {
  Filter(function(x) {
    any(grepl(paste(pos_list, collapse="|"), x$title, ignore.case=TRUE))
  }, data)
}

qb  <- split_by_position(final, c("QB","quarterback"))
rb  <- split_by_position(final, c("RB","running back","rush"))
wr  <- split_by_position(final, c("WR","receiver","wide"))
te  <- split_by_position(final, c("TE","tight end"))
def <- split_by_position(final, c("defense","defensive","DST"))

# =====================
# SAVE FILES
# =====================
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

write_json(qb,  file.path(OUTPUT_DIR, "news_qb.json"),  pretty=TRUE, auto_unbox=TRUE)
write_json(rb,  file.path(OUTPUT_DIR, "news_rb.json"),  pretty=TRUE, auto_unbox=TRUE)
write_json(wr,  file.path(OUTPUT_DIR, "news_wr.json"),  pretty=TRUE, auto_unbox=TRUE)
write_json(te,  file.path(OUTPUT_DIR, "news_te.json"),  pretty=TRUE, auto_unbox=TRUE)
write_json(def, file.path(OUTPUT_DIR, "news_def.json"), pretty=TRUE, auto_unbox=TRUE)

message("✅ DONE")
