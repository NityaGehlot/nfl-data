# =====================
# scripts/generate_nfl_news.R (OFFENSE + DEFENSE + OL CLEAN VERSION)
# =====================

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(xml2)
library(stringdist)

# =====================
# CONFIG
# =====================
OUTPUT_DIR   <- "data/news"
MAX_PER_PLAYER <- 2
REQUEST_DELAY  <- 0.3
SEASON_START   <- as.Date("2025-09-04")

# =====================
# CHECK FILE
# =====================
if (!file.exists("data/sleeperAPI/sleeper_players.json")) {
  stop("❌ sleeper_players.json not found in data/sleeperAPI/")
}

# =====================
# LOAD DATA
# =====================
message("Loading Sleeper players...")

players_raw <- fromJSON(
  "data/sleeperAPI/sleeper_players.json",
  simplifyVector = FALSE
)

# =====================
# HELPER
# =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b

# =====================
# POSITION NORMALIZATION
# =====================
normalize_position <- function(pos, fantasy_positions) {

  # Defensive: %||% only catches NULL, not NA. Since update_players.R can
  # legitimately produce NA here (player_id resolved but no position match),
  # collapse both NULL and NA down to "" before anything else, so none of
  # the `==` comparisons below ever get handed a literal NA.
  pos <- pos %||% NA_character_
  if (length(pos) == 0 || is.na(pos)) pos <- ""
  pos <- toupper(pos)

  # Defensive line
  if (pos %in% c("DE", "DT", "LDT", "RDT")) return("DL")

  # Linebackers
  if (pos == "LB") return("LB")

  # Defensive backs
  if (pos == "CB") return("CB")
  if (pos == "S")  return("S")
  if (pos == "DB") return("DB")

  # Offensive line — granular slots pass through as-is
  if (pos %in% c("LT", "LG", "C", "RG", "RT")) return(pos)

  # Generic OL tags Sleeper uses when side is not specified
  if (pos %in% c("OT", "OG", "OL", "T", "G")) return(pos)

  # Everything else (QB, RB, WR, TE, K, etc.)
  return(pos)
}

# =====================
# BUILD CLEAN PLAYER TABLE
# =====================
players_list <- lapply(players_raw, function(p) {

  if (is.null(p$first_name) || is.null(p$last_name)) return(NULL)

  # update_players.R now writes position_listed_on_sleeper /
  # position_listed_on_nflreadr instead of a plain "position" field.
  # Prefer the nflreadr-sourced tag (more accurate, ESPN-backed per
  # generate_weekly_stats.R's logic) and fall back to Sleeper's own tag
  # when nflreadr has no match yet (e.g. very recent rookies).
  pos_raw  <- p$position_listed_on_nflreadr %||% p$position_listed_on_sleeper %||% NA_character_
  norm_pos <- normalize_position(pos_raw, p$fantasy_positions)

  data.frame(
    player_id         = p$player_id %||% NA,
    player_name       = paste(p$first_name, p$last_name),
    position_raw      = pos_raw,
    position          = norm_pos,
    team              = p$team %||% NA,
    status            = p$status %||% NA,
    depth_chart_order = {
      d <- suppressWarnings(as.numeric(p$depth_chart_order))
      if (is.na(d)) 99 else d
    },
    stringsAsFactors = FALSE
  )
})

players <- bind_rows(Filter(Negate(is.null), players_list))

message("Total players loaded: ", nrow(players))

# =====================
# CLEAN
# =====================
players <- players %>%
  filter(!is.na(player_name), !is.na(position)) %>%
  filter(status == "Active" | is.na(status))

# =====================
# DIAGNOSTIC — see exactly what OL tags Sleeper uses
# =====================
message("OL position tags found in Sleeper data: ",
  paste(
    players %>%
      filter(position_raw %in% c("LT","LG","C","RG","RT",
                                  "OT","OG","OL","T","G")) %>%
      count(position_raw) %>%
      mutate(label = paste0(position_raw, "(", n, ")")) %>%
      pull(label),
    collapse = ", "
  )
)

# =====================
# OFFENSIVE LINE FILTERING
# =====================
ol_specific_positions <- c("LT", "LG", "C", "RG", "RT")
ol_generic_positions  <- c("OT", "OG", "OL", "T", "G")

# Granular slot tags — top 2 per slot per team
ol_specific <- players %>%
  filter(position %in% ol_specific_positions, !is.na(team)) %>%
  group_by(team, position) %>%
  slice_min(order_by = depth_chart_order, n = 2, with_ties = FALSE) %>%
  ungroup()

# Generic OT/OG tags — top 4 OTs per team (covers LT+RT starters+backups),
# top 2 OGs per team (covers interior starters)
ol_generic <- players %>%
  filter(position %in% ol_generic_positions, !is.na(team)) %>%
  group_by(team, position) %>%
  slice_min(order_by = depth_chart_order, n = 4, with_ties = FALSE) %>%
  ungroup()

ol_filtered <- bind_rows(ol_specific, ol_generic) %>%
  distinct(player_id, .keep_all = TRUE)

message("OL players after filtering: ", nrow(ol_filtered))

# =====================
# SKILL POSITION + DEFENSE FILTERING
# =====================
skill_def_filtered <- players %>%
  filter(
    # OFFENSE (skill)
    (position == "QB" & depth_chart_order <= 2) |
    (position == "RB" & depth_chart_order <= 3) |
    (position == "WR" & depth_chart_order <= 6) |
    (position == "TE" & depth_chart_order <= 3) |
    (position == "K"  & depth_chart_order == 1) |

    # DEFENSE
    (position == "DL" & depth_chart_order <= 6) |
    (position == "LB" & depth_chart_order <= 4) |
    (position == "CB" & depth_chart_order <= 5) |
    (position == "S"  & depth_chart_order <= 3) |
    (position == "DB" & depth_chart_order <= 5)
  )

# =====================
# COMBINE AND DEDUPLICATE
# =====================
filtered_players <- bind_rows(skill_def_filtered, ol_filtered) %>%
  distinct(player_id, .keep_all = TRUE)

message("After filtering: ", nrow(filtered_players))

# =====================
# SPLIT GROUPS
# =====================
groups <- split(filtered_players, filtered_players$position)

message("Position groups: ", paste(names(groups), collapse = ", "))

# =====================
# RSS HELPERS
# =====================
safe_parse_date <- function(x) {
  tryCatch({
    parsed <- parse_date_time(
      x,
      orders = c("a, d b Y H:M:S z", "Y-m-d"),
      tz = "UTC"
    )
    if (length(parsed) == 0 || all(is.na(parsed))) return(NA)
    as.Date(parsed[[1]])
  }, error = function(e) NA)
}

get_impact <- function(text) {
  t <- tolower(text)
  if (grepl("injur|out|ir|surgery", t))      return("negative")
  if (grepl("signed|trade|cut|released", t)) return("roster_move")
  if (grepl("breakout|huge|dominant", t))    return("positive")
  "neutral"
}

fetch_google <- function(player_name) {

  url <- paste0(
    "https://news.google.com/rss/search?q=",
    URLencode(paste(player_name, "NFL")),
    "&hl=en-US&gl=US&ceid=US:en"
  )

  xml <- tryCatch(read_xml(url), error = function(e) NULL)
  if (is.null(xml)) return(list())

  items    <- xml_find_all(xml, "//item")
  articles <- list()

  for (item in items) {

    title  <- xml_text(xml_find_first(item, "title"))
    link   <- xml_text(xml_find_first(item, "link"))
    pub    <- xml_text(xml_find_first(item, "pubDate"))

    parsed <- safe_parse_date(pub)
    if (is.na(parsed)) parsed <- Sys.Date()
    if (parsed < SEASON_START) next

    articles <- c(articles, list(list(
      title     = title,
      link      = link,
      published = as.character(parsed),
      player    = player_name,
      impact    = get_impact(title)
    )))
  }

  articles[1:min(length(articles), MAX_PER_PLAYER)]
}

build_news <- function(df) {
  result <- list()

  for (i in seq_len(nrow(df))) {
    Sys.sleep(REQUEST_DELAY)
    news   <- fetch_google(df$player_name[i])
    result <- c(result, news)
  }

  result
}

# =====================
# GENERATE ALL GROUPS
# =====================
news_by_pos <- lapply(groups, build_news)

# =====================
# SAVE
# =====================
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

for (pos in names(news_by_pos)) {
  # Sanitize position name for filename (e.g. OL_OTHER -> ol_other, OT -> ot)
  safe_pos <- tolower(gsub("[^a-zA-Z0-9]", "_", pos))
  write_json(
    news_by_pos[[pos]],
    file.path(OUTPUT_DIR, paste0("news_", safe_pos, ".json")),
    pretty     = TRUE,
    auto_unbox = TRUE
  )
}

message("✅ DONE — offense + defense + OL news generated")
