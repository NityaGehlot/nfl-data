# =====================
# scripts/generate_nfl_news.R
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
REQUEST_DELAY <- 0.3

# Accept everything from Sep 4 2025 onward
HARD_FLOOR <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")

message(paste("Window:", format(HARD_FLOOR, "%Y-%m-%d"), "to", format(Sys.time(), "%Y-%m-%d")))

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
  if (is.na(ts)) return(FALSE)
  ts >= as.numeric(HARD_FLOOR)
}

# =====================
# LOAD SLEEPER PLAYERS
# =====================
message("Loading Sleeper players...")
players_raw <- tryCatch(
  fromJSON("https://api.sleeper.app/v1/players/nfl", simplifyDataFrame = FALSE),
  error = function(e) { message("Sleeper failed: ", e$message); list() }
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
# IMPACT
# =====================
get_impact <- function(text) {
  if (is.null(text) || is.na(text)) return("neutral")
  t <- tolower(text)
  if (grepl("injur|placed on ir|season-ending|surgery|torn|fracture|concussion|doubtful|ruled out|pup list|non-football injury", t)) return("negative")
  if (grepl("questionable|limited|day-to-day|sore|monitor|missed practice|did not practice|dnp", t))                                 return("slightly_negative")
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cut |released|restructure|agrees|joining", t)) return("roster_move")
  if (grepl("breakout|career-high|record|mvp|pro bowl|comeback|activated|off ir|re-signs|named starter", t))                         return("positive")
  "neutral"
}

# =====================
# BUILD ARTICLE
# =====================
make_article <- function(title, desc, link, published, source) {
  if (is.null(title) || is.na(title) || trimws(title) == "") return(NULL)
  if (is.null(link)  || is.na(link)  || trimws(link)  == "") return(NULL)
  if (!is_recent_enough(published)) return(NULL)
  full_text <- paste(title, desc)
  list(
    title             = trimws(title),
    summary           = str_trunc(ifelse(!is.na(desc) & nchar(trimws(desc)) > 10, desc, title), 250),
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
    res <- GET("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?limit=100",
               timeout(30))
    message("  Status: ", res$status_code)
    if (res$status_code != 200) return(list())
    data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
    arts <- data$articles
    if (is.null(arts) || nrow(arts) == 0) { message("  0 articles"); return(list()) }
    message("  Raw articles: ", nrow(arts))
    if (is.data.frame(arts)) arts <- split(arts, seq(nrow(arts)))
    results <- lapply(arts, function(a) {
      tryCatch({
        title <- a$headline %||% ""
        desc  <- a$description %||% ""
        link  <- tryCatch(a$links$web$href %||% "", error = function(e) "")
        pub   <- a$published %||% ""
        art   <- make_article(title, desc, link, pub, "ESPN")
        if (!is.null(art)) message("  KEPT: ", str_trunc(title, 70))
        else                message("  skip: ", str_trunc(title, 50), " [", pub, "]")
        art
      }, error = function(e) NULL)
    })
    kept <- Filter(Negate(is.null), results)
    message("  Kept: ", length(kept))
    kept
  }, error = function(e) { message("  ERROR: ", e$message); list() })
}

# =====================
# SOURCE 2: ESPN PER-TEAM API (all 32 teams)
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
  message("\n[ESPN Team Feeds — ", length(teams), " teams]")
  results <- list()

  for (team in teams) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      url <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?team=",
                    team, "&limit=20")
      res  <- GET(url, timeout(15))
      if (res$status_code != 200) next
      data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      arts <- data$articles
      if (is.null(arts) || nrow(arts) == 0) next
      if (is.data.frame(arts)) arts <- split(arts, seq(nrow(arts)))

      team_kept <- 0
      for (a in arts) {
        tryCatch({
          title <- a$headline %||% ""
          desc  <- a$description %||% ""
          link  <- tryCatch(a$links$web$href %||% "", error = function(e) "")
          pub   <- a$published %||% ""
          art   <- make_article(title, desc, link, pub, "ESPN")
          if (!is.null(art)) { results <- c(results, list(art)); team_kept <- team_kept + 1 }
        }, error = function(e) NULL)
      }
      message("  [", toupper(team), "] kept ", team_kept, " / ", length(arts))
    }, error = function(e) NULL)
  }
  message("  Team total kept: ", length(results))
  results
}

# =====================
# SOURCE 3: ProFootballTalk (best for transactions + injuries)
# =====================
fetch_pft <- function() {
  message("\n[ProFootballTalk]")
  feeds <- c(
    "https://profootballtalk.nbcsports.com/feed/",
    "https://profootballtalk.nbcsports.com/category/news/feed/",
    "https://profootballtalk.nbcsports.com/category/transactions/feed/",
    "https://profootballtalk.nbcsports.com/category/injuries/feed/"
  )
  results <- list()
  for (url in feeds) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (compatible; bot/1.0)"))
      message("  ", basename(dirname(url)), " status: ", res$status_code)
      if (res$status_code != 200) next
      xml   <- read_xml(content(res, "text", encoding = "UTF-8"))
      items <- xml_find_all(xml, "//item")
      message("  items: ", length(items))
      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, "title"))
          link  <- xml_text(xml_find_first(item, "link"))
          pub   <- xml_text(xml_find_first(item, "pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
          clean <- str_trim(str_replace(title, "\\s*[-|]\\s*[^-|]{1,40}$", ""))
          art   <- make_article(clean, desc, link, pub, "ProFootballTalk")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  PFT total kept: ", length(results))
  results
}

# =====================
# SOURCE 4: CBS Sports NFL RSS
# =====================
fetch_cbs <- function() {
  message("\n[CBS Sports]")
  feeds <- c(
    "https://www.cbssports.com/rss/headlines/nfl/",
    "https://www.cbssports.com/rss/headlines/fantasy/nfl/"
  )
  results <- list()
  for (url in feeds) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (compatible; bot/1.0)"))
      message("  status: ", res$status_code)
      if (res$status_code != 200) next
      xml   <- read_xml(content(res, "text", encoding = "UTF-8"))
      items <- xml_find_all(xml, "//item")
      message("  items: ", length(items))
      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, "title"))
          link  <- xml_text(xml_find_first(item, "link"))
          pub   <- xml_text(xml_find_first(item, "pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
          clean <- str_trim(str_replace(title, "\\s*[-|]\\s*[^-|]{1,40}$", ""))
          art   <- make_article(clean, desc, link, pub, "CBSSports")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  CBS total kept: ", length(results))
  results
}

# =====================
# SOURCE 5: NFL.com RSS
# =====================
fetch_nfl_com <- function() {
  message("\n[NFL.com RSS]")
  feeds <- c(
    "https://www.nfl.com/rss/rsslanding?searchString=news",
    "https://www.nfl.com/rss/rsslanding?searchString=injuries",
    "https://www.nfl.com/rss/rsslanding?searchString=transactions"
  )
  results <- list()
  for (url in feeds) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (compatible; bot/1.0)"))
      message("  status: ", res$status_code)
      if (res$status_code != 200) next
      xml   <- read_xml(content(res, "text", encoding = "UTF-8"))
      items <- xml_find_all(xml, "//item")
      message("  items: ", length(items))
      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, "title"))
          link  <- xml_text(xml_find_first(item, "link"))
          pub   <- xml_text(xml_find_first(item, "pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
          art   <- make_article(title, desc, link, pub, "NFL.com")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  NFL.com total kept: ", length(results))
  results
}

# =====================
# SOURCE 6: Bleacher Report NFL RSS
# =====================
fetch_bleacher <- function() {
  message("\n[Bleacher Report]")
  results <- list()
  tryCatch({
    res <- GET("https://bleacherreport.com/articles/feed?tag_id=16", timeout(15),
               add_headers("User-Agent" = "Mozilla/5.0 (compatible; bot/1.0)"))
    message("  status: ", res$status_code)
    if (res$status_code != 200) return(list())
    xml   <- read_xml(content(res, "text", encoding = "UTF-8"))
    items <- xml_find_all(xml, "//item")
    message("  items: ", length(items))
    for (item in items) {
      tryCatch({
        title <- xml_text(xml_find_first(item, "title"))
        link  <- xml_text(xml_find_first(item, "link"))
        pub   <- xml_text(xml_find_first(item, "pubDate"))
        desc  <- tryCatch(xml_text(xml_find_first(item, "description")), error = function(e) "")
        art   <- make_article(title, desc, link, pub, "BleacherReport")
        if (!is.null(art)) results <- c(results, list(art))
      }, error = function(e) NULL)
    }
  }, error = function(e) message("  ERROR: ", e$message))
  message("  BR kept: ", length(results))
  results
}

# =====================
# RUN ALL SOURCES
# =====================
message("\n========== FETCH START ==========\n")

all_news <- c(
  fetch_espn_general(),
  fetch_espn_teams(),
  fetch_pft(),
  fetch_cbs(),
  fetch_nfl_com(),
  fetch_bleacher()
)

all_news <- Filter(Negate(is.null), all_news)
message("\n========== RESULTS ==========")
message("Total before dedup: ", length(all_news))

# =====================
# FINAL DATE CHECK
# =====================
before   <- length(all_news)
all_news <- Filter(function(x) is_recent_enough(x$published), all_news)
message("Dropped by date: ", before - length(all_news))
message("After date filter: ", length(all_news))

# =====================
# DEDUPLICATE
# =====================
seen <- character(0)
all_news <- Filter(function(x) {
  key <- tolower(trimws(x$title))
  if (key %in% seen) return(FALSE)
  seen <<- c(seen, key)
  TRUE
}, all_news)
message("After dedup: ", length(all_news))

# =====================
# SORT — impact tier first, then newest within tier
# =====================
impact_rank <- c(negative=5, slightly_negative=4, roster_move=3, positive=2, neutral=1)
pub_ts      <- sapply(all_news, function(x) safe_parse_date(x$published) %||% 0)
all_news    <- all_news[order(
  sapply(all_news, function(x) impact_rank[x$impact %||% "neutral"]),
  pub_ts,
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
  message("WARNING: 0 articles after all filtering — check source logs above")
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
message("\n✅ Saved ", length(all_news), " articles | Run: ",
        format(Sys.time(), "%Y-%m-%d %H:%M UTC"))
