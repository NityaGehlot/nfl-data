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

# Date parsing, headline/source split, relevance check and classification
source("scripts/news_helpers.R")

# =====================
# CONFIG
# =====================
OUTPUT_DIR   <- "data/news"
MAX_PER_PLAYER <- 3
REQUEST_DELAY  <- 0.3
SEASON_START   <- current_season_start()  # Sept 1 of the current NFL season

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

  # Kickers — nflreadr tags these "PK" while Sleeper's own tag is "K".
  # Without this, position_listed_on_nflreadr ("PK") wins via %||% in the
  # caller, normalize_position falls through to the generic `return(pos)`,
  # and almost every kicker ends up classified as "PK" instead of "K" —
  # silently failing every `position == "K"` filter downstream.
  if (pos == "PK") return("K")

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
    (position == "K"  & depth_chart_order <= 2) |

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

message("Kickers selected: ", sum(filtered_players$position == "K"),
        " across ", n_distinct(filtered_players$team[filtered_players$position == "K"]), " teams")

message("After filtering: ", nrow(filtered_players))

# =====================
# SPLIT GROUPS
# =====================
groups <- split(filtered_players, filtered_players$position)

message("Position groups: ", paste(names(groups), collapse = ", "))

# =====================
# RSS HELPERS
# =====================
fetch_google <- function(player_name) {

  # Quote the name so Google returns articles about this exact player.
  url <- paste0(
    "https://news.google.com/rss/search?q=",
    URLencode(paste0('"', player_name, '" NFL'), reserved = TRUE),
    "&hl=en-US&gl=US&ceid=US:en"
  )

  xml <- tryCatch(read_xml(url), error = function(e) NULL)
  if (is.null(xml)) return(list())

  items    <- xml_find_all(xml, "//item")
  articles <- list()

  for (item in items) {

    raw_title <- xml_text(xml_find_first(item, "title"))
    link      <- xml_text(xml_find_first(item, "link"))
    pub       <- xml_text(xml_find_first(item, "pubDate"))

    parts  <- split_title_source(raw_title)
    source <- parts$source
    if (is.na(source)) source <- xml_text(xml_find_first(item, "source"))

    # Skip articles whose headline doesn't actually name this player
    if (!mentions_player(parts$headline, player_name)) next

    parsed <- parse_rss_date(pub)
    if (!is.na(parsed) && parsed < SEASON_START) next

    info <- classify_headline(parts$headline, player_name)
    info$score <- NULL

    article <- list(
      title     = parts$headline,
      source    = if (is.na(source) || !nzchar(source)) NULL else source,
      link      = link,
      published = if (is.na(parsed)) NULL else as.character(parsed),
      player    = player_name,
      impact    = info$impact,
      category  = info$category,
      summary   = info$summary
    )
    # Drop missing fields (jsonlite would write NULL as {})
    articles <- c(articles, list(Filter(Negate(is.null), article)))
  }

  # Newest first, keeping up to MAX_PER_PLAYER articles that cover different stories
  select_diverse(articles, MAX_PER_PLAYER)
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

  # Drop exact duplicates (the same article saved twice for the same player).
  articles <- news_by_pos[[pos]]
  links    <- vapply(articles, function(a) paste(a$link, a$player), character(1))
  articles <- articles[!duplicated(links)]

  write_json(
    articles,
    file.path(OUTPUT_DIR, paste0("news_", safe_pos, ".json")),
    pretty     = TRUE,
    auto_unbox = TRUE
  )
}

# When this run finished, so the app can show how fresh the news is.
write_json(
  list(generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       articles = sum(vapply(news_by_pos, length, integer(1)))),
  file.path(OUTPUT_DIR, "news_meta.json"),
  auto_unbox = TRUE, pretty = TRUE
)

message("✅ DONE — offense + defense + OL news generated")
