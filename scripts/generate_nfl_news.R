# =====================
# scripts/generate_nfl_news.R (STABLE PRODUCTION VERSION)
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
OUTPUT_FILE <- "data/nfl_news.json"
MAX_ARTICLES <- 100
HOURS_BACK <- 24 * 7  # 7 days

# =====================
# SAFE HELPERS
# =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_parse_date <- function(x) {
  parsed <- tryCatch({
    parse_date_time(x, orders = c(
      "ymd HMS",
      "ymd HM",
      "Ymd HMS",
      "a, d b Y H:M:S",
      "a d b Y H:M:S",
      "Y-m-d\\TH:M:S",
      "Y-m-dTH:M:SZ",   # ISO 8601 with Z (ESPN format)
      "Y-m-dTH:M:S"     # ISO 8601 without Z
    ), quiet = TRUE, tz = "UTC")
  }, error = function(e) NA)

  if (length(parsed) == 0 || all(is.na(parsed))) return(NA)
  parsed
}

# =====================
# LOAD SLEEPER PLAYERS (SAFE VERSION)
# =====================
message("Loading Sleeper players...")

sleeper_url <- "https://api.sleeper.app/v1/players/nfl"
players_raw <- tryCatch(
  fromJSON(sleeper_url, simplifyDataFrame = FALSE),
  error = function(e) {
    message("WARNING: Failed to load Sleeper players: ", e$message)
    list()
  }
)

# keep only valid entries
players_raw <- players_raw[!sapply(players_raw, is.null)]

players_df <- bind_rows(lapply(players_raw, function(p) {
  # Flatten: replace length-0 fields with NA, collapse vectors to first element
  p_clean <- lapply(p, function(val) {
    if (is.null(val) || length(val) == 0) return(NA)
    if (length(val) > 1) return(val[[1]])  # take first element if multiple
    val
  })
  as.data.frame(p_clean, stringsAsFactors = FALSE)
}))

# keep only real active NFL players
active_players <- players_df %>%
  filter(!is.na(status)) %>%
  filter(status == "Active") %>%
  filter(position %in% c("QB", "RB", "WR", "TE", "K", "DEF")) %>%
  mutate(
    full_name = tolower(trimws(paste(
      ifelse(is.na(first_name), "", first_name),
      ifelse(is.na(last_name), "", last_name)
    )))
  ) %>%
  filter(!is.na(full_name) & full_name != "")

player_names <- unique(active_players$full_name)

message(paste("Active players loaded:", length(player_names)))

# =====================
# PLAYER DETECTION (SAFE + STRICT)
# =====================
detect_players <- function(text) {
  if (is.null(text) || text == "") return(character(0))
  text <- tolower(text)

  matched <- player_names[sapply(player_names, function(name) {
    grepl(paste0("\\b", gsub("\\.", "\\\\.", name), "\\b"), text)
  })]

  unique(matched)
}

# =====================
# IMPACT SCORING
# =====================
get_impact <- function(text) {
  if (is.null(text) || text == "") return("neutral")
  text <- tolower(text)

  if (grepl("injury|injured|out|surgery|ir|doubtful|torn|fracture|concussion", text)) {
    "negative"
  } else if (grepl("questionable|limited|monitor|day-to-day|sore|ailing", text)) {
    "slightly_negative"
  } else if (grepl("breakout|dominant|huge|career-high|impressive|star|record|touchdown|mvp", text)) {
    "positive"
  } else {
    "neutral"
  }
}

# =====================
# FILTER BAD CONTENT
# =====================
is_relevant <- function(text) {
  if (is.null(text) || text == "") return(FALSE)
  text <- tolower(text)

  !grepl("mock draft|college|prospect|simulation|2026 nfl draft", text)
}

# =====================
# ESPN NEWS
# =====================
fetch_espn <- function() {
  message("Fetching ESPN...")

  result <- tryCatch({
    url <- "https://site.api.espn.com/apis/site/v2/sports/football/nfl/news"
    res <- GET(url, timeout(15))

    if (res$status_code != 200) {
      message("WARNING: ESPN returned status ", res$status_code)
      return(list())
    }

    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    articles <- data$articles

    if (is.null(articles) || nrow(articles) == 0) {
      message("WARNING: No ESPN articles found")
      return(list())
    }

    if (is.data.frame(articles)) {
      articles <- split(articles, seq(nrow(articles)))
    }

    results <- lapply(articles, function(a) {
      tryCatch({
        title <- a$headline %||% ""
        desc  <- a$description %||% ""
        link  <- tryCatch(a$links$web$href %||% "", error = function(e) "")
        published <- a$published %||% ""

        if (title == "" || link == "") return(NULL)

        text <- paste(title, desc)
        if (!is_relevant(text)) return(NULL)

        list(
          title            = title,
          summary          = str_trunc(ifelse(desc != "", desc, title), 140),
          link             = link,
          published        = published,
          source           = "ESPN",
          players_mentioned = detect_players(text),
          impact           = get_impact(text)
        )
      }, error = function(e) {
        message("WARNING: Skipping malformed ESPN article: ", e$message)
        NULL
      })
    })

    Filter(Negate(is.null), results)

  }, error = function(e) {
    message("ERROR: ESPN fetch failed: ", e$message)
    list()
  })

  result
}

# =====================
# GOOGLE NEWS RSS
# =====================
fetch_google <- function() {
  message("Fetching Google News RSS...")

  result <- tryCatch({
    url <- "https://news.google.com/rss/search?q=NFL+football+players&hl=en-US&gl=US&ceid=US:en"
    xml <- read_xml(url)
    items <- xml_find_all(xml, "//item")

    if (length(items) == 0) {
      message("WARNING: No Google News items found")
      return(list())
    }

    results <- lapply(items, function(item) {
      tryCatch({
        title <- xml_text(xml_find_first(item, "title"))
        link  <- xml_text(xml_find_first(item, "link"))
        pub   <- xml_text(xml_find_first(item, "pubDate"))

        if (is.na(title) || title == "") return(NULL)
        if (!is_relevant(title)) return(NULL)

        list(
          title             = title,
          summary           = str_trunc(title, 140),
          link              = link,
          published         = pub,
          source            = "GoogleNews",
          players_mentioned = detect_players(title),
          impact            = get_impact(title)
        )
      }, error = function(e) {
        message("WARNING: Skipping malformed Google News item: ", e$message)
        NULL
      })
    })

    Filter(Negate(is.null), results)

  }, error = function(e) {
    message("ERROR: Google News fetch failed (this is common on CI): ", e$message)
    list()
  })

  result
}

# =====================
# RUN SOURCES
# =====================
news <- c(
  fetch_espn(),
  fetch_google()
)

news <- Filter(Negate(is.null), news)

message(paste("Total raw articles:", length(news)))

# =====================
# FILTER RECENT (SAFE)
# =====================
cutoff <- Sys.time() - hours(HOURS_BACK)

news <- Filter(function(x) {
  tryCatch({
    parsed <- safe_parse_date(x$published)
    if (length(parsed) == 0 || is.na(parsed)) {
      message("NOTE: Keeping article with unparseable date: ", x$title)
      return(TRUE)  # keep rather than silently drop
    }
    parsed >= cutoff
  }, error = function(e) TRUE)
}, news)

# =====================
# REMOVE EMPTY TITLES
# =====================
news <- Filter(function(x) {
  !is.null(x$title) && x$title != ""
}, news)

# =====================
# DEDUPLICATE BY TITLE
# =====================
seen_titles <- c()
news <- Filter(function(x) {
  key <- tolower(trimws(x$title))
  if (key %in% seen_titles) return(FALSE)
  seen_titles <<- c(seen_titles, key)
  TRUE
}, news)

message(paste("Articles after dedup:", length(news)))

# =====================
# SORT BY IMPACT
# =====================
impact_priority <- c(
  "negative"          = 3,
  "slightly_negative" = 2,
  "positive"          = 1,
  "neutral"           = 0
)

news <- news[order(
  sapply(news, function(x) impact_priority[x$impact]),
  decreasing = TRUE
)]

# =====================
# LIMIT
# =====================
news <- head(news, MAX_ARTICLES)

# =====================
# FALLBACK (PREVENT EMPTY JSON)
# =====================
if (length(news) == 0) {
  message("WARNING: No filtered news found — adding fallback entry")
  news <- list(list(
    title             = "No recent NFL news available",
    summary           = "System fallback entry",
    link              = "https://www.espn.com/nfl/",
    published         = as.character(Sys.time()),
    source            = "SYSTEM",
    players_mentioned = character(0),
    impact            = "neutral"
  ))
}

# =====================
# SAVE JSON
# =====================
if (!dir.exists("data")) dir.create("data")

write_json(news, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)

message(paste("✅ NFL news generated successfully! Articles saved:", length(news)))
