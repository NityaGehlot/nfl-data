# =====================
# scripts/news_helpers.R
# Pure helpers for generate_nfl_news.R: date parsing, headline/source splitting,
# relevance checks and headline classification. No network access, so these can be
# tested with scripts/tests/test_news_helpers.R.
# =====================

suppressPackageStartupMessages({
  library(lubridate)
  library(stringr)
})

# RSS pubDate looks like "Fri, 31 Oct 2026 14:05:00 GMT". lubridate's "z" order can't
# read zone names like GMT, which made every date fail and fall back to today's date.
parse_rss_date <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(as.Date(NA))
  cleaned <- str_trim(str_remove(x, "\\s+(GMT|UTC|UT|Z|[+-]\\d{4})$"))
  parsed <- suppressWarnings(parse_date_time(
    cleaned,
    orders = c("a, d b Y H:M:S", "d b Y H:M:S", "Y-m-d H:M:S", "Y-m-d"),
    tz = "UTC"
  ))
  if (length(parsed) == 0 || is.na(parsed[[1]])) return(as.Date(NA))
  as.Date(parsed[[1]])
}

# Only keep articles from the current NFL season. Seasons start in early September and
# are labeled by that year (Jan-Feb playoff games belong to the previous season).
current_season_start <- function(today = Sys.Date()) {
  year <- as.integer(format(today, "%Y"))
  month <- as.integer(format(today, "%m"))
  season <- if (month >= 3) year else year - 1
  as.Date(sprintf("%d-09-01", season))
}

# Google News titles end with " - Source Name". Split that off.
split_title_source <- function(title) {
  title <- str_squish(title %||% "")
  m <- str_match(title, "^(.*\\S)\\s+[-–—]\\s+([^-–—]{2,60})$")
  if (is.na(m[1, 1])) return(list(headline = title, source = NA_character_))
  list(headline = m[1, 2], source = str_trim(m[1, 3]))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

normalize_text <- function(x) {
  x <- tolower(iconv(x, to = "ASCII//TRANSLIT", sub = ""))
  x <- str_replace_all(x, "[^a-z0-9 ]", " ")
  str_squish(x)
}

# An article only counts as news about a player if the headline names him: full name,
# or last name when it is distinctive (4+ letters and not a common English word).
COMMON_LAST_NAMES <- c("will", "love", "white", "brown", "young", "allen", "johnson", "williams",
                       "smith", "jones", "davis", "moore", "hill", "jackson", "harris", "thomas",
                       "green", "king", "wright", "walker", "hall", "lee", "james", "mitchell",
                       "taylor", "brooks", "carter", "bell", "cook", "rice", "lamb", "ward")

mentions_player <- function(headline, player_name) {
  text <- paste0(" ", normalize_text(headline), " ")
  name <- normalize_text(str_remove(player_name, "\\s+(Jr\\.?|Sr\\.?|II|III|IV|V)$"))
  if (!nzchar(name)) return(FALSE)
  if (str_detect(text, fixed(paste0(" ", name, " ")))) return(TRUE)
  last <- tail(str_split(name, " ")[[1]], 1)
  nchar(last) >= 4 && !(last %in% COMMON_LAST_NAMES) && str_detect(text, fixed(paste0(" ", last, " ")))
}

# ---------------------
# Classification
# ---------------------
INJURY_PARTS <- c("hamstring", "ankle", "knee", "acl", "mcl", "concussion", "shoulder", "groin",
                  "calf", "foot", "back (injury|spasms|tightness)", "hip", "quad", "quadriceps", "wrist", "hand", "thumb",
                  "finger", "rib", "ribs", "neck", "achilles", "toe", "elbow", "chest", "oblique",
                  "pectoral", "illness", "abdomen", "heel")

has_word <- function(text, words) {
  any(str_detect(text, regex(paste0("\\b(", paste(words, collapse = "|"), ")\\b"), ignore_case = TRUE)))
}

first_word <- function(text, words) {
  m <- str_match(text, regex(paste0("\\b(", paste(words, collapse = "|"), ")\\b"), ignore_case = TRUE))
  if (is.na(m[1, 2])) NA_character_ else tolower(m[1, 2])
}

# Headlines often cover several players ("A set to play, B ruled out"). Keep only the
# clauses that name this player so B's injury isn't tagged on A.
focus_clause <- function(headline, player_name) {
  if (!nzchar(player_name)) return(headline)
  parts <- str_split(headline, "[,;]|\\s+(and|but|while|as|after)\\s+")[[1]]
  hits <- parts[vapply(parts, mentions_player, logical(1), player_name = player_name)]
  if (length(hits) == 0) headline else paste(hits, collapse = " ")
}

# Returns list(category, impact, summary). Impact values match the app's badges:
# positive / negative / slightly_negative / roster_move / neutral.
classify_headline <- function(headline, player_name = "") {
  t <- tolower(focus_clause(headline, player_name))
  who <- if (nzchar(player_name)) player_name else "The player"
  part <- first_word(t, INJURY_PARTS)
  part_txt <- if (is.na(part)) "" else paste0(" (", part, ")")

  # Order matters: the most specific / most fantasy-relevant signals first.
  if (has_word(t, c("suspended", "suspension", "suspend"))) {
    return(list(category = "suspension", impact = "negative",
                summary = paste0(who, " is facing a suspension. Plan to replace him in your lineup.")))
  }

  if (has_word(t, c("injured reserve", "season[- ]ending", "torn", "tears", "surgery", "out for (the )?season"))) {
    return(list(category = "injury", impact = "negative",
                summary = paste0("Serious injury news for ", who, part_txt,
                                 ". Expect him to miss significant time.")))
  }

  if (has_word(t, c("activated", "returns? to practice", "returning", "cleared", "back at practice",
                    "full practice", "full participant", "expected to play", "will play", "set to play", "set to return",
                    "off injury report", "no injury designation"))) {
    return(list(category = "injury_return", impact = "positive",
                summary = paste0("Positive health update for ", who, part_txt,
                                 ". He is trending toward playing.")))
  }

  if (has_word(t, c("ruled out", "won't play", "will not play", "out vs", "out against", "out (this|for) week",
                    "inactive", "doubtful", "did not practice", "dnp", "miss(es)? (practice|time|game)",
                    "sidelined", "carted off", "leaves game", "exits", "ir"))) {
    return(list(category = "injury", impact = "negative",
                summary = paste0(who, " is dealing with an injury", part_txt,
                                 " that could keep him out. Check his status before lineups lock.")))
  }

  if (has_word(t, c("questionable", "limited", "game[- ]time decision", "injury", "injured", "hurt",
                    "banged up", "tweaked", INJURY_PARTS))) {
    return(list(category = "injury", impact = "slightly_negative",
                summary = paste0(who, " has an injury note", part_txt,
                                 ". Monitor practice reports before kickoff.")))
  }

  if (has_word(t, c("traded", "trade", "acquire[sd]?", "signs?", "signed", "signing", "released", "waived",
                    "cut", "claimed", "re-sign(s|ed)?", "extension", "contract", "free agent", "practice squad",
                    "elevated", "promoted"))) {
    return(list(category = "transaction", impact = "roster_move",
                summary = paste0("Roster move involving ", who,
                                 ". His role and fantasy value could change with it.")))
  }

  if (has_word(t, c("benched", "demoted", "loses? (starting )?job", "backup", "fumbles?", "struggles?",
                    "struggling", "drops?", "bust", "sit him", "fade", "disappointing", "quiet"))) {
    return(list(category = "performance", impact = "negative",
                summary = paste0("Concerning news about ", who, "'s role or production.")))
  }

  if (has_word(t, c("breakout", "career[- ]high", "dominant", "dominates", "explodes", "huge game",
                    "touchdowns?", "td", "tds", "starter", "start(s|ing)? at", "named starter",
                    "lead back", "workhorse", "smash", "must[- ]start", "league[- ]leading", "record", "big game",
                    "shines", "stars?"))) {
    return(list(category = "performance", impact = "positive",
                summary = paste0("Good news for ", who, ": a strong performance or bigger role.")))
  }

  if (has_word(t, c("start/sit", "start or sit", "waiver", "rankings?", "projections?", "matchups?",
                    "sleepers?", "fantasy", "dfs", "busts?", "buy low", "sell high"))) {
    return(list(category = "fantasy_advice", impact = "neutral",
                summary = paste0("Fantasy advice article that mentions ", who, ".")))
  }

  list(category = "general", impact = "neutral",
       summary = paste0("General news mentioning ", who, "."))
}
