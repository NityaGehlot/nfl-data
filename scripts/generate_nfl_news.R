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

HARD_FLOOR <- as.POSIXct("2025-09-04 00:00:00", tz = "UTC")
message(paste("Window:", format(HARD_FLOOR, "%Y-%m-%d"), "to", format(Sys.time(), "%Y-%m-%d")))

# =====================
# HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[[1]])) a else b

safe_parse_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  x <- trimws(as.character(x[[1]]))
  if (is.na(x) || x == "") return(NA_real_)

  # Remove timezone abbreviations that confuse parsers (e.g. "EDT", "PST")
  x_clean <- gsub("\\s+[A-Z]{2,4}$", "", x)

  tryCatch({
    # Try lubridate with many format orders
    parsed <- parse_date_time(x_clean, orders = c(
      "a, d b Y H:M:S",   # RFC 822: Mon, 20 Apr 2026 01:00:00
      "a, d b Y H:M",
      "d b Y H:M:S",
      "d b Y H:M",
      "Y-m-dTH:M:SZ",     # ISO 8601 with Z
      "Y-m-dTH:M:S",      # ISO 8601
      "Y-m-d H:M:S",
      "Y-m-d",
      "ymd HMS",
      "ymd HM",
      "ymd"
    ), quiet = TRUE, tz = "UTC")

    if (length(parsed) > 0 && !is.na(parsed[[1]])) {
      return(as.numeric(parsed[[1]]))
    }

    # Fallback: try base R's strptime formats
    formats <- c(
      "%a, %d %b %Y %H:%M:%S",
      "%a, %d %b %Y %H:%M",
      "%Y-%m-%dT%H:%M:%SZ",
      "%Y-%m-%dT%H:%M:%S",
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d"
    )
    for (fmt in formats) {
      r <- suppressWarnings(as.POSIXct(strptime(x_clean, fmt, tz = "UTC")))
      if (!is.na(r)) return(as.numeric(r))
    }

    NA_real_
  }, error = function(e) NA_real_)
}

is_recent_enough <- function(pub) {
  if (is.null(pub) || is.na(pub) || trimws(pub) == "") return(FALSE)
  ts <- safe_parse_date(pub)
  if (is.na(ts)) {
    message("    [date-skip] unparseable: '", pub, "'")
    return(FALSE)
  }
  result <- ts >= as.numeric(HARD_FLOOR)
  if (!result) {
    message("    [date-skip] too old: '", pub, "' -> ", format(as.POSIXct(ts, origin="1970-01-01", tz="UTC"), "%Y-%m-%d"))
  }
  result
}

# Hard exclusions only
is_excluded <- function(title) {
  t <- tolower(title)
  if (grepl("^[a-z ']+'s nfl draft profile$", t))        return(TRUE)
  if (grepl("nfl draft profile$", t))                     return(TRUE)
  if (grepl("top \\d+ prospects|mock draft ranking", t))  return(TRUE)
  if (grepl("college prospect|high school|ncaa recruit",  t)) return(TRUE)
  FALSE
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
  if (grepl("signs|signed|trade|traded|free agent|deal|contract|extension|acquires|claims|waiver|cut |released|restructure|agrees|joining|fifth-year option|franchise tag", t)) return("roster_move")
  if (grepl("breakout|career-high|record|mvp|pro bowl|comeback|activated|off ir|re-signs|named starter", t))                         return("positive")
  "neutral"
}

# =====================
# BUILD ARTICLE
# =====================
make_article <- function(title, desc, link, published, source) {
  if (is.null(title) || is.na(title) || trimws(title) == "") return(NULL)
  if (is.null(link)  || is.na(link)  || trimws(link)  == "") return(NULL)
  if (!is_recent_enough(published))  return(NULL)
  if (is_excluded(title))            return(NULL)
  full_text <- paste(title, desc)
  list(
    title             = trimws(title),
    summary           = str_trunc(ifelse(!is.na(desc) & nchar(trimws(desc)) > 10, trimws(desc), trimws(title)), 250),
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
    raw_text <- content(res, "text", encoding = "UTF-8")
    data <- fromJSON(raw_text, flatten = TRUE)
    arts <- data$articles
    if (is.null(arts) || nrow(arts) == 0) { message("  0 articles"); return(list()) }
    message("  Raw: ", nrow(arts))

    # Debug: show first date to verify format
    if (!is.null(arts$published) && length(arts$published) > 0) {
      message("  Sample date: '", arts$published[[1]], "'")
      message("  Parsed: ", safe_parse_date(arts$published[[1]]))
    }

    results <- list()
    for (i in seq_len(nrow(arts))) {
      tryCatch({
        a     <- arts[i, ]
        title <- as.character(a$headline %||% "")
        desc  <- as.character(a$description %||% "")
        # ESPN links can be nested
        link  <- tryCatch({
          lw <- a$links.web.href
          if (!is.null(lw) && !is.na(lw) && lw != "") lw
          else as.character(a$links %||% "")
        }, error = function(e) "")
        pub <- as.character(a$published %||% "")
        art <- make_article(title, desc, link, pub, "ESPN")
        if (!is.null(art)) results <- c(results, list(art))
      }, error = function(e) message("  row error: ", e$message))
    }
    message("  Kept: ", length(results))
    results
  }, error = function(e) { message("  ERROR: ", e$message); list() })
}

# =====================
# SOURCE 2: ESPN PER-TEAM API
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
  message("\n[ESPN Team Feeds]")
  results <- list()

  for (team in teams) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      url  <- paste0("https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?team=",
                     team, "&limit=20")
      res  <- GET(url, timeout(15))
      if (res$status_code != 200) next
      data <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      arts <- data$articles
      if (is.null(arts) || nrow(arts) == 0) next

      n_kept <- 0
      for (i in seq_len(nrow(arts))) {
        tryCatch({
          a     <- arts[i, ]
          title <- as.character(a$headline %||% "")
          desc  <- as.character(a$description %||% "")
          link  <- tryCatch({
            lw <- a$links.web.href
            if (!is.null(lw) && !is.na(lw) && lw != "") lw
            else ""
          }, error = function(e) "")
          pub <- as.character(a$published %||% "")
          art <- make_article(title, desc, link, pub, "ESPN")
          if (!is.null(art)) { results <- c(results, list(art)); n_kept <- n_kept + 1 }
        }, error = function(e) NULL)
      }
      message("  [", toupper(team), "] kept ", n_kept, "/", nrow(arts))
    }, error = function(e) NULL)
  }
  message("  Team total: ", length(results))
  results
}

# =====================
# SOURCE 3: ProFootballTalk RSS
# =====================
fetch_pft <- function() {
  message("\n[ProFootballTalk]")
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
      message("  status: ", res$status_code, " | url: ", url)
      if (res$status_code != 200) next
      raw <- content(res, "text", encoding = "UTF-8")

      doc   <- read_html(raw)
      items <- xml_find_all(doc, "//item")
      message("  items: ", length(items))

      # Debug first item's date
      if (length(items) > 0) {
        first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubdate")), error = function(e) "")
        if (is.na(first_pub) || first_pub == "")
          first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubDate")), error = function(e) "")
        message("  Sample pubdate: '", first_pub, "' -> parsed: ", safe_parse_date(first_pub))
      }

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- gsub("<[^>]+>", " ", desc)
          desc  <- str_squish(desc)
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
# SOURCE 4: CBS Sports RSS
# =====================
fetch_cbs <- function() {
  message("\n[CBS Sports]")
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
      message("  status: ", res$status_code, " | url: ", url)
      if (res$status_code != 200) next
      raw   <- content(res, "text", encoding = "UTF-8")
      doc   <- read_html(raw)
      items <- xml_find_all(doc, "//item")
      message("  items: ", length(items))

      if (length(items) > 0) {
        first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubdate")), error = function(e) "")
        if (is.na(first_pub) || first_pub == "")
          first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubDate")), error = function(e) "")
        message("  Sample pubdate: '", first_pub, "' -> parsed: ", safe_parse_date(first_pub))
      }

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- xml_text(xml_find_first(item, ".//pubdate"))
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubDate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- gsub("<[^>]+>", " ", desc)
          desc  <- str_squish(desc)
          clean_title <- str_trim(str_replace(title, "\\s*[-|]\\s*(CBS Sports|NFL).*$", ""))
          art   <- make_article(clean_title, desc, link, pub, "CBSSports")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  CBS kept: ", length(results))
  results
}

# =====================
# SOURCE 5: NFL.com RSS — try multiple XPath approaches
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
      message("  status: ", res$status_code, " | url: ", url)
      if (res$status_code != 200) next
      raw   <- content(res, "text", encoding = "UTF-8")

      # Try XML first, fall back to HTML
      items <- tryCatch({
        doc <- read_xml(raw)
        xml_find_all(doc, "//item")
      }, error = function(e) {
        tryCatch({
          doc <- read_html(raw)
          xml_find_all(doc, "//item")
        }, error = function(e2) list())
      })
      message("  items: ", length(items))

      if (length(items) > 0) {
        first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubDate")), error = function(e) "")
        message("  Sample pubdate: '", first_pub, "' -> parsed: ", safe_parse_date(first_pub))
      }

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- tryCatch(xml_text(xml_find_first(item, ".//pubDate")), error = function(e) "")
          if (is.na(pub) || pub == "") pub <- tryCatch(xml_text(xml_find_first(item, ".//pubdate")), error = function(e) "")
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- gsub("<[^>]+>", " ", desc)
          desc  <- str_squish(desc)
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
# SOURCE 6: RotoBaller RSS (fantasy-focused, reliable)
# =====================
fetch_rotoballer <- function() {
  message("\n[RotoBaller]")
  urls <- c(
    "https://www.rotoballer.com/feed",
    "https://www.rotoballer.com/category/nfl/feed"
  )
  results <- list()
  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code)
      if (res$status_code != 200) next
      raw <- content(res, "text", encoding = "UTF-8")
      doc <- tryCatch(read_xml(raw), error = function(e) read_html(raw))
      items <- xml_find_all(doc, "//item")
      message("  items: ", length(items))

      if (length(items) > 0) {
        first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubDate")), error = function(e) "")
        message("  Sample pubdate: '", first_pub, "' -> parsed: ", safe_parse_date(first_pub))
      }

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- tryCatch(xml_text(xml_find_first(item, ".//pubDate")), error = function(e) "")
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubdate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- gsub("<[^>]+>", " ", desc)
          desc  <- str_squish(desc)
          art   <- make_article(title, desc, link, pub, "RotoBaller")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  RotoBaller kept: ", length(results))
  results
}

# =====================
# SOURCE 7: Rotoworld / NBC Sports RSS
# =====================
fetch_rotoworld <- function() {
  message("\n[Rotoworld/NBC]")
  urls <- c(
    "https://www.nbcsports.com/rss/nfl/news",
    "https://www.nbcsports.com/rss/fantasy/football"
  )
  results <- list()
  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code)
      if (res$status_code != 200) next
      raw <- content(res, "text", encoding = "UTF-8")
      doc <- tryCatch(read_xml(raw), error = function(e) read_html(raw))
      items <- xml_find_all(doc, "//item")
      message("  items: ", length(items))

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- tryCatch(xml_text(xml_find_first(item, ".//pubDate")), error = function(e) "")
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubdate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- gsub("<[^>]+>", " ", desc)
          desc  <- str_squish(desc)
          art   <- make_article(title, desc, link, pub, "NBCSports")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
  message("  NBC kept: ", length(results))
  results
}

# =====================
# SOURCE 8: The Athletic (via RSS if available) / Bleacher Report
# =====================
fetch_bleacher_report <- function() {
  message("\n[Bleacher Report]")
  urls <- c(
    "https://bleacherreport.com/articles/feed?tag_id=16",  # NFL
    "https://bleacherreport.com/articles/feed?tag_id=1282" # Fantasy football
  )
  results <- list()
  for (url in urls) {
    Sys.sleep(REQUEST_DELAY)
    tryCatch({
      res <- GET(url, timeout(15),
                 add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64)"))
      message("  status: ", res$status_code)
      if (res$status_code != 200) next
      raw <- content(res, "text", encoding = "UTF-8")
      doc <- tryCatch(read_xml(raw), error = function(e) read_html(raw))
      items <- xml_find_all(doc, "//item")
      message("  items: ", length(items))

      if (length(items) > 0) {
        first_pub <- tryCatch(xml_text(xml_find_first(items[[1]], ".//pubDate")), error = function(e) "")
        message("  Sample pubdate: '", first_pub, "' -> parsed: ", safe_parse_date(first_pub))
      }

      for (item in items) {
        tryCatch({
          title <- xml_text(xml_find_first(item, ".//title"))
          link  <- xml_text(xml_find_first(item, ".//link"))
          pub   <- tryCatch(xml_text(xml_find_first(item, ".//pubDate")), error = function(e) "")
          if (is.na(pub) || pub == "") pub <- xml_text(xml_find_first(item, ".//pubdate"))
          desc  <- tryCatch(xml_text(xml_find_first(item, ".//description")), error = function(e) "")
          desc  <- gsub("<[^>]+>", " ", desc)
          desc  <- str_squish(desc)
          art   <- make_article(title, desc, link, pub, "BleacherReport")
          if (!is.null(art)) results <- c(results, list(art))
        }, error = function(e) NULL)
      }
    }, error = function(e) message("  ERROR: ", e$message))
  }
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
  fetch_rotoballer(),
  fetch_rotoworld(),
  fetch_bleacher_report()
)

all_news <- Filter(Negate(is.null), all_news)
message("\n========== RESULTS ==========")
message("Total raw: ", length(all_news))

# Final date pass (safety net)
before   <- length(all_news)
all_news <- Filter(function(x) is_recent_enough(x$published), all_news)
message("Dropped by date: ", before - length(all_news), " | After: ", length(all_news))

# Dedup by title
seen <- character(0)
all_news <- Filter(function(x) {
  key <- tolower(trimws(x$title))
  if (key %in% seen) return(FALSE)
  seen <<- c(seen, key)
  TRUE
}, all_news)
message("After dedup: ", length(all_news))

# Sort: impact tier first, then newest within tier
impact_rank <- c(negative=5, slightly_negative=4, roster_move=3, positive=2, neutral=1)
pub_ts      <- sapply(all_news, function(x) safe_parse_date(x$published) %||% 0)
all_news    <- all_news[order(
  sapply(all_news, function(x) impact_rank[x$impact %||% "neutral"]),
  pub_ts,
  decreasing = TRUE
)]

all_news <- head(all_news, MAX_ARTICLES)

# =====================
# MERGE WITH EXISTING JSON (keep old articles not in new batch, up to MAX)
# =====================
if (file.exists(OUTPUT_FILE) && length(all_news) < MAX_ARTICLES) {
  tryCatch({
    existing <- fromJSON(OUTPUT_FILE, simplifyVector = FALSE)
    existing <- Filter(Negate(is.null), existing)
    new_titles <- tolower(trimws(sapply(all_news, function(x) x$title)))
    old_only   <- Filter(function(x) {
      !(tolower(trimws(x$title)) %in% new_titles)
    }, existing)
    combined  <- c(all_news, old_only)
    all_news  <- head(combined, MAX_ARTICLES)
    message("After merging old articles: ", length(all_news))
  }, error = function(e) message("Could not merge with existing JSON: ", e$message))
}

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
