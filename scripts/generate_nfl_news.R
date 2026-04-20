# =====================
# scripts/generate_nfl_news.R
# Player-targeted NFL news for fantasy football app
# Enhanced version — no Google News (blocked on CI)
# Sources: ESPN General + All 32 ESPN Team Feeds + ESPN Athlete Feeds +
#          PFT + CBS + NFL.com + Rotoworld + FantasyPros
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
OUTPUT_FILE   <- "data/nfl_news.json"
MAX_ARTICLES  <- 300
REQUEST_DELAY <- 0.3   # seconds between requests

# Hard floor — never show anything older than this.
# Currently set to start of 2026 NFL offseason (free agency open).
# Update to the 2026 season opener (~Sep 3 2026) once the season starts.
OFFSEASON_FLOOR <- as.POSIXct("2026-03-11 00:00:00", tz = "UTC")
DAYS_BACK       <- 45   # rolling window; tighten to 14 during regular season
cutoff          <- max(OFFSEASON_FLOOR, Sys.time() - days(DAYS_BACK))

message(paste("Date cutoff:", format(cutoff, "%Y-%m-%d %H:%M UTC")))

# =====================
# HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

safe_parse_date <- function(x) {
  if (is.null(x) || is.na(x) || x == "") return(NA_real_)
  tryCatch({
    parsed <- parse_date_time(x, orders = c(
      "a, d b Y H:M:S z", "a, d b Y H:M:S",
      "ymd HMS", "ymd HM",
      "Y-m-dTH:M:SZ", "Y-m-dTH:M:S", "Y-m-d H:M:S"
    ), quiet = TRUE, tz = "UTC")
    if (length(parsed) == 0 || all(is.na(parsed))) return(NA_real_)
    as.numeric(parsed[[1]])
  }, error = function(e) NA_real_)
}

is_recent_enough <- function(pub) {
  ts <- safe_parse_date(pub)
  if (is.na(ts)) return(FALSE)   # unparseable = reject
  ts >= as.numeric(cutoff)
}

# Hard exclusions only — pure draft prospect profiles
is_excluded <- function(title) {
  t <- tolower(title)
  if (grepl("^[a-z ']+'s nfl draft profile$", t))       return(TRUE)
  if (grepl("nfl draft profile$", t))                    return(TRUE)
  if (grepl("top \\d+ prospects|mock draft ranking", t)) return(TRUE)
  if (grepl("college prospect|high school|ncaa recruit", t)) return(TRUE)
  FALSE
}

# =====================
# LOAD SLEEPER PLAYERS
# =====================
message("Loading Sleeper players...")
players_raw <- tryCatch(
  fromJSON("https://api.sleeper.app/v1/players/nfl", simplifyDataFrame = FALSE),
  error = function(e) { message("WARNING: Sleeper failed: ", e$message); list() }
)
players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  p_clean <- lapply(p, function(val) {
    if (is.null(val) || length(val) == 0) return(NA)
    if (length(val) > 1) return(val[[1]])
    val
  })
  tryCatch(as.data.frame(p_clean, stringsAsFactors = FALSE), error = function(e) NULL)
}))

active_players <- players_df %>%
  filter(!is.na(status), status == "Active") %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "K")) %>%
  mutate(
    fn = ifelse(is.na(first_name), "", trimws(first_name)),
    ln = ifelse(is.na(last_name),  "", trimws(last_name)),
    full_name    = tolower(paste(fn, ln)),
    display_name = paste(fn, ln)
  ) %>%
  filter(nchar(trimws(full_name)) > 1)

player_lookup <- setNames(active_players$display_name, active_players$full_name)
player_names  <- names(player_lookup)
message(paste("Players loaded:", length(player_names)))

# =====================
# PLAYER DETECTION
# =====================
detect_players <- function(text) {
  if (is.null(text) || is.na(text) || text == "") return(character(0))
  tl <- tolower(text)
  matched <- player_names[vapply(player_names, function(nm) {
    grepl(paste0("\\b", gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", nm), "\\b"), tl)
  }, logical(1))]
  unique(unname(player_lookup[matched]))
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  if (is.null(text) || is.na(text)) return("neutral")
  t <- tolower(text)
  if (grepl("injur|placed on ir|season-ending|surgery|torn|fracture|concussion|doubtful|ruled out|pup list|non-football injury", t)) return("negative")
  if (grepl("questionable|limited|day-to-day|sore|ailing|monitor|missed practice|did not practice|dnp|didn't practice", t))          return("slightly_negative")
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cut |released|restructure|agrees|joining|visits|leaving|franchise tag|fifth-year option", t)) return("roster_move")
  if (grepl("breakout|career-high|record|mvp|pro bowl|comeback|activated|off ir|re-signs|named starter|retained", t))                return("positive")
  "neutral"
}

# =====================
# SHARED ARTICLE BUILDER
# =====================
make_article <- function(title, desc, link, published, source) {
  if (is.null(title) || is.na(title) || trimws(title) == "") return(NULL)
  if (is.null(link)  || is.na(link)  || trimws(link)  == "") return(NULL)
  if (!is_recent_enough(published)) return(NULL)
  if (is_excluded(title))           return(NULL)
  full_text <- paste(title, desc)
  list(
    title             = trimws(title),
    summary           = str_trunc(ifelse(!is.na(desc) & nchar(trimws(desc)) > 10,
                                         trimws(desc), trimws(title)), 250),
    link              = trimws(link),
    published         = published,
    source            = source,
    players_mentioned = detect_players(full_text),
    impact            = get_impact(full_text)
  )
}

# =====================
# SOURCE 1: ESPN GENERAL API
# =====================
fetch_espn_general <- function() {
  message("\n[ESPN General]")
  tryCatch({
    res  <- GET("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?limit=100",
                timeout(30))
    message("  Status: ", res$status_code)
    if (res$status_code != 200) return(list())
    data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    arts <- data$articles
    if (is.null(arts) || nrow(arts) == 0) { message("  0 articles"); return(list()) }
    message("  Raw: ", nrow(arts))
    if (is.data.frame(arts)) arts <- split(arts, seq(nrow(arts)))

    results <- lapply(arts, function(a) {
      tryCatch({
        title <- a$headline %||% ""
        desc  <- a$description %||% ""
        link  <- tryCatch(a$links$web$href %||% "", error = function(e) "")
        pub   <- a$published %||% ""
        make_article(title, desc, link, pub, "ESPN")
      }, error = function(e) NULL)
    })
    kept <- Filter(Negate(is.null), results)
    message("  Kept: ", length(kept))
    kept
  }, error = function(e) { message("  ERROR: ", e$message); list() })
}

# =====================
# SOURCE 2: ESPN PER-TEAM API (all 32 teams)
# Each team feed surfaces player-specific injury/transaction items
# that the general feed misses — e.g. Caleb Williams via the Bears feed.
# =====================
fetch_espn_teams <- function() {
  teams <- c(
    "buf","mia","ne","nyj",
    "bal","cin","cle","pit",
    "hou","ind","jax","ten",
    "den","kc","lv","lac",
    "dal","nyg","phi","wsh",
    "chi","det","gb","min",
    "atl","car","no","tb",
    "ari","lar","sf","sea"
  )
  message("\n[ESPN Team Feeds — all 32 teams]")
  results <- list()

  for (team in teams) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      url  <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?team=",
                     team, "&limit=30")
      res  <- GET(url, timeout(15))
      if (res$status_code != 200) next
      data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      arts <- data$articles
      if (is.null(arts) || nrow(arts) == 0) next
      if (is.data.frame(arts)) arts <- split(arts, seq(nrow(arts)))

      n_kept <- 0
      for (a in arts) {
        tryCatch({
          title <- a$headline %||% ""
          desc  <- a$description %||% ""
          link  <- tryCatch(a$links$web$href %||% "", error = function(e) "")
          pub   <- a$published %||% ""
          art   <- make_article(title, desc, link, pub, "ESPN")
          if (!is.null(art)) { results <- c(results, list(art)); n_kept <- n_kept + 1 }
        }, error = function(e) NULL)
      }
      message("  [", toupper(team), "] kept ", n_kept)
    }, error = function(e) NULL)
  }
  message("  Team total: ", length(results))
  results
}

# =====================
# SOURCE 3: ESPN ATHLETE NEWS API
# Fetches news for individual top fantasy players by ESPN athlete ID.
# This is the most reliable way to guarantee coverage for star players.
# Add/update IDs as needed each season.
# =====================
fetch_espn_athletes <- function() {
  athlete_ids <- c(
    "3139477",  # Patrick Mahomes
    "3054211",  # Josh Allen
    "4362887",  # Lamar Jackson
    "4430807",  # Caleb Williams
    "3915511",  # Joe Burrow
    "4035538",  # Jalen Hurts
    "4040715",  # Justin Herbert
    "4360310",  # Brock Purdy
    "4689695",  # CJ Stroud
    "4374302",  # Anthony Richardson
    "3054273",  # Dak Prescott
    "3052875",  # Kyler Murray
    "3042519",  # Tua Tagovailoa
    "4047365",  # Trevor Lawrence
    "4567048",  # Jordan Love
    "3054098",  # Christian McCaffrey
    "4258173",  # Saquon Barkley
    "4569618",  # Bijan Robinson
    "4379399",  # De'Von Achane
    "4427366",  # Jahmyr Gibbs
    "4373678",  # Jonathan Taylor
    "4240021",  # Tony Pollard
    "4362628",  # Breece Hall
    "4429795",  # Puka Nacua
    "4262921",  # Ja'Marr Chase
    "3054015",  # Stefon Diggs
    "4361307",  # CeeDee Lamb
    "4035004",  # Tyreek Hill
    "4372016",  # Davante Adams
    "3054380",  # DeAndre Hopkins
    "4360438",  # Amon-Ra St. Brown
    "4035671",  # Justin Jefferson
    "4362490",  # Garrett Wilson
    "4430278",  # Marvin Harrison Jr.
    "3054100",  # Travis Kelce
    "4241389",  # Sam LaPorta
    "4047783",  # Mark Andrews
    "4035672",  # T.J. Hockenson
    "4426366"   # Brock Bowers
  )

  message("\n[ESPN Athlete Feeds — ", length(athlete_ids), " players]")
  results <- list()

  for (aid in athlete_ids) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      url <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/athletes/",
                    aid, "/news?limit=10")
      res <- GET(url, timeout(15))
      if (res$status_code != 200) next
      data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      arts <- data$articles
      if (is.null(arts) || nrow(arts) == 0) next
      if (is.data.frame(arts)) arts <- split(arts, seq(nrow(arts)))

      n_kept <- 0
      for (a in arts) {
        tryCatch({
          title <- a$headline %||% ""
          desc  <- a$description %||% ""
          link  <- tryCatch(a$links$web$href %||% "", error = function(e) "")
          pub   <- a$published %||% ""
          art   <- make_article(title, desc, link, pub, "ESPN")
          if (!is.null(art)) { results <- c(results, list(art)); n_kept <- n_kept + 1 }
        }, error = function(e) NULL)
      }
      if (n_kept > 0) message("  [athlete ", aid, "] kept ", n_kept)
    }, error = function(e) NULL)
  }
  message("  Athlete total: ", length(results))
  results
}

# =====================
# SOURCE 4: ProFootballTalk RSS
# read_html() used because PFT feeds contain malformed HTML attributes
# that crash read_xml().
# =====================
fetch_pft <- function() {
  message("\n[ProFootballTalk RSS]")
  urls <- c(
    "https://profootballtalk.nbcsports.com/feed/",
    "https://profootballtalk.nbcsports.com/category/transactions/feed/",
    "https://profootballtalk.nbcsports.com/category/injuries/feed/"
  )
  results <- list()

  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(20),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code, " | ", url)
      if (res$status_code != 200) next
      raw   <- content(res, "text", encoding = "UTF-8")
      doc   <- read_html(raw)
      items <- xml_find_all(doc, "//item")
      message("  items found: ", length(items))

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- str_squish(gsub("<[^>]+>", " ", desc))
          art   <- make_article(title, desc, link, pub, "ProFootballTalk")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  PFT kept: ", length(results))
  results
}

# =====================
# SOURCE 5: CBS Sports RSS
# =====================
fetch_cbs <- function() {
  message("\n[CBS Sports RSS]")
  urls <- c(
    "https://www.cbssports.com/rss/headlines/nfl/",
    "https://www.cbssports.com/rss/headlines/fantasy/nfl/"
  )
  results <- list()

  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code, " | ", url)
      if (res$status_code != 200) next
      raw   <- content(res, "text", encoding = "UTF-8")
      doc   <- read_html(raw)
      items <- xml_find_all(doc, "//item")
      message("  items found: ", length(items))

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- str_squish(gsub("<[^>]+>", " ", desc))
          clean <- str_trim(str_replace(title, "\\s*[-|]\\s*(CBS Sports|NFL).*$", ""))
          art   <- make_article(clean, desc, link, pub, "CBSSports")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  CBS kept: ", length(results))
  results
}

# =====================
# SOURCE 6: NFL.com RSS
# =====================
fetch_nfl_com <- function() {
  message("\n[NFL.com RSS]")
  urls <- c(
    "https://www.nfl.com/rss/rsslanding?searchString=news",
    "https://www.nfl.com/rss/rsslanding?searchString=injuries",
    "https://www.nfl.com/rss/rsslanding?searchString=transactions"
  )
  results <- list()

  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code, " | ", url)
      if (res$status_code != 200) next
      raw   <- content(res, "text", encoding = "UTF-8")
      doc   <- read_html(raw)
      items <- xml_find_all(doc, "//item")
      message("  items found: ", length(items))

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- str_squish(gsub("<[^>]+>", " ", desc))
          art   <- make_article(title, desc, link, pub, "NFL.com")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  NFL.com kept: ", length(results))
  results
}

# =====================
# SOURCE 7: Rotoworld RSS
# Best source for individual player blurbs — injuries, snap counts,
# role changes. Highly recommended for fantasy apps.
# =====================
fetch_rotoworld <- function() {
  message("\n[Rotoworld RSS]")
  urls <- c(
    "https://www.rotoworld.com/football/nfl/player-news/rss",
    "https://www.nbcsportsedge.com/football/nfl/rss"
  )
  results <- list()

  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(20),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code, " | ", url)
      if (res$status_code != 200) next
      raw   <- content(res, "text", encoding = "UTF-8")
      doc   <- read_html(raw)
      items <- xml_find_all(doc, "//item")
      message("  items found: ", length(items))

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- str_squish(gsub("<[^>]+>", " ", desc))
          art   <- make_article(title, desc, link, pub, "Rotoworld")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  Rotoworld kept: ", length(results))
  results
}

# =====================
# SOURCE 8: FantasyPros RSS — per-position feeds
# Fantasy-angled player news; great for role changes and start/sit signals.
# =====================
fetch_fantasypros <- function() {
  message("\n[FantasyPros RSS]")
  urls <- c(
    "https://www.fantasypros.com/nfl/news.php?rss=1",
    "https://www.fantasypros.com/nfl/news/qb.php?rss=1",
    "https://www.fantasypros.com/nfl/news/rb.php?rss=1",
    "https://www.fantasypros.com/nfl/news/wr.php?rss=1",
    "https://www.fantasypros.com/nfl/news/te.php?rss=1"
  )
  results <- list()

  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(20),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code, " | ", url)
      if (res$status_code != 200) next
      raw <- content(res, "text", encoding = "UTF-8")
      # Try read_xml first; fall back to read_html for malformed feeds
      doc <- tryCatch(read_xml(raw), error = function(e) read_html(raw))
      items <- xml_find_all(doc, "//item")
      message("  items found: ", length(items))

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- str_squish(gsub("<[^>]+>", " ", desc))
          art   <- make_article(title, desc, link, pub, "FantasyPros")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  FantasyPros kept: ", length(results))
  results
}

# =====================
# RUN ALL SOURCES
# =====================
message("\n========== FETCH START ==========\n")

all_news <- c(
  fetch_espn_general(),
  fetch_espn_teams(),
  fetch_espn_athletes(),
  fetch_pft(),
  fetch_cbs(),
  fetch_nfl_com(),
  fetch_rotoworld(),
  fetch_fantasypros()
)

all_news <- Filter(Negate(is.null), all_news)
message("\n========== RESULTS ==========")
message("Total raw: ", length(all_news))

# Belt-and-suspenders date pass after make_article already checked
before   <- length(all_news)
all_news <- Filter(function(x) is_recent_enough(x$published), all_news)
message("Dropped by date: ", before - length(all_news), " | After: ", length(all_news))

# Dedup by normalised title
seen <- character(0)
all_news <- Filter(function(x) {
  key <- tolower(trimws(x$title))
  if (key %in% seen) return(FALSE)
  seen <<- c(seen, key)
  TRUE
}, all_news)
message("After dedup: ", length(all_news))

# Sort: impact tier first, then newest within tier
impact_rank <- c(negative = 5, slightly_negative = 4, roster_move = 3, positive = 2, neutral = 1)
pub_ts      <- sapply(all_news, function(x) safe_parse_date(x$published) %||% 0)
all_news    <- all_news[order(
  sapply(all_news, function(x) impact_rank[x$impact %||% "neutral"]),
  pub_ts,
  decreasing = TRUE
)]

all_news <- head(all_news, MAX_ARTICLES)

# =====================
# FALLBACK
# =====================
if (length(all_news) == 0) {
  message("WARNING: 0 articles — all sources failed or were filtered out")
  all_news <- list(list(
    title             = "No recent NFL news available",
    summary           = "Check back soon for the latest player updates.",
    link              = "https://www.espn.com/nfl/",
    published         = format(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"),
    source            = "SYSTEM",
    players_mentioned = character(0),
    impact            = "neutral"
  ))
}

# =====================
# SAVE
# =====================
if (!dir.exists("data")) dir.create("data")
write_json(all_news, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)
message("\n✅ Saved ", length(all_news), " articles | ",
        format(Sys.time(), "%Y-%m-%d %H:%M UTC"))
