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
  names_someone_else <- function(p) {
    !mentions_player(p, player_name) && str_detect(p, "\\b[A-Z][a-z'.]+ [A-Z][a-zA-Z'.-]+")
  }
  keep <- parts[!vapply(parts, names_someone_else, logical(1))]
  if (length(keep) == 0 || !any(vapply(keep, mentions_player, logical(1), player_name = player_name))) {
    return(headline)
  }
  paste(keep, collapse = " ")
}

# ---------------------
# Sentiment scoring
# ---------------------
# Each cue found in the player's part of the headline adds its weight. Negations right before
# a cue ("not expected to play", "won't return") flip it. Specific phrases are matched first and
# removed so their words aren't counted twice ("no injury designation" isn't an "injury").
PHRASE_CUES <- c(
  # Practice absences first (longer phrases before shorter ones): "remains out of practice"
  # is a stronger signal than one missed session.
  "(remains|stays|still|again) (out of|absent from|away from) practice" = -3,
  "(remains|still) sidelined" = -3,
  "(out of|absent from|away from) practice" = -2, "not (spotted|seen|present) (at|during|on)" = -2, "not spotted" = -2,
  "(sits?|sat) out (of )?practice" = -2, "held out( of practice)?" = -2, "limited (in |at )?practice" = -1,
  "activated (off|from) (the )?(ir|injured reserve|pup|nfi)" = 3, "(designated|designates) .{0,20}to return" = 2,
  "drop(ped|s)? (a |two |three |another )?(pass|passes|ball|touchdown|td)" = -1,
  "(not|isn'?t|unlikely to be) expected to (play|return)" = -3, "(won'?t|will not|not) (play|return)" = -3,
  "no injury designation" = 2, "off (the )?injury report" = 2, "full participant" = 2, "full practice" = 2,
  "returns? to practice" = 2, "back at practice" = 2, "expected to play" = 2, "set to play" = 2, "will play" = 2,
  "set to return" = 2, "game[- ]time decision" = -1, "ruled out" = -3, "won'?t play" = -3, "will not play" = -3,
  "out for (the )?season" = -4, "season[- ]ending" = -4, "injured reserve" = -3, "carted off" = -3,
  "did not practice" = -2, "not practice" = -2, "miss(es)? (practice|time|the game|week)" = -2, "out (vs|against|for|this)" = -3,
  "leaves? (the )?game" = -2, "career[- ]high" = 2, "named (the )?starter" = 2, "lead back" = 2,
  "loses? (his )?(starting )?job" = -3, "trade request" = -1, "buy low" = 1, "sell high" = -1
)

WORD_CUES <- c(
  suspended = -3, suspension = -3, torn = -4, tears = -3, surgery = -3, ir = -3, sidelined = -2, inactive = -2,
  doubtful = -2, questionable = -1, limited = -1, injury = -1, injured = -1, hurt = -1, concussion = -2,
  benched = -3, demoted = -2, struggles = -1, struggling = -1, fumbles = -1, fumble = -1, quiet = -1,
  disappointing = -1, bust = -1, fade = -1, concern = -1, concerns = -1, setback = -2, aggravated = -2,
  dnp = -2, arrested = -3, charged = -2, lawsuit = -1, assault = -2,
  activated = 2, cleared = 2, returning = 2, returns = 1, healthy = 2, upgraded = 2, breakout = 2, explodes = 2,
  dominant = 2, dominates = 2, shines = 1, touchdown = 1, touchdowns = 1, td = 1, tds = 1, scores = 1,
  starter = 1, workhorse = 2, smash = 1,
  extension = 1, upgrade = 1, record = 1
)

NEGATIONS <- c("not", "no", "won't", "wont", "isn't", "isnt", "without", "unlikely", "never", "doesn't", "didn't")

# Score a lower-cased clause. Returns list(score, cues) where cues are the matched terms.
sentiment_score <- function(t) {
  score <- 0
  cues <- character(0)
  flip <- function(pos) {
    before <- str_trim(substr(t, max(1, pos - 25), pos - 1))
    words <- tail(str_split(before, "\\s+")[[1]], 3)
    any(words %in% NEGATIONS)
  }
  for (pat in names(PHRASE_CUES)) {
    rx <- regex(paste0("\\b", pat, "\\b"))
    loc <- str_locate(t, rx)
    if (!is.na(loc[1, 1])) {
      w <- PHRASE_CUES[[pat]]
      if (w > 0 && flip(loc[1, 1])) w <- -(w + 1)
      score <- score + w
      cues <- c(cues, str_sub(t, loc[1, 1], loc[1, 2]))
      t <- str_replace_all(t, rx, " ")
    }
  }
  words <- str_split(str_replace_all(t, "[^a-z' ]", " "), "\\s+")[[1]]
  for (i in seq_along(words)) {
    w <- WORD_CUES[words[i]]
    if (is.na(w)) next
    prev <- words[max(1, i - 2):max(1, i - 1)]
    if (i > 1 && any(prev %in% NEGATIONS)) w <- -w
    score <- score + w
    cues <- c(cues, words[i])
  }
  list(score = score, cues = cues)
}

impact_from_score <- function(score) {
  if (score >= 1) return("positive")
  if (score <= -3) return("negative")
  if (score < 0) return("slightly_negative")
  "neutral"
}

# Returns list(category, impact, summary, score).
# Category says what the story is about; impact says whether it's good or bad for the player.
classify_headline <- function(headline, player_name = "") {
  t <- tolower(focus_clause(headline, player_name))
  who <- if (nzchar(player_name)) player_name else "The player"
  part <- first_word(t, INJURY_PARTS)
  part_txt <- if (is.na(part)) "" else paste0(" (", part, ")")
  scored <- sentiment_score(t)
  score <- scored$score
  # A named injury ("back spasms", "hamstring") with no other signal is still a mild concern.
  if (!is.na(part) && score == 0) score <- -1
  impact <- impact_from_score(score)
  out <- function(category, impact, summary) list(category = category, impact = impact, summary = summary, score = score)

  if (has_word(t, c("suspended", "suspension", "suspend"))) {
    return(out("suspension", "negative", paste0(who, " is facing a suspension. Plan to replace him in your lineup.")))
  }

  injury_words <- has_word(t, c("spotted", "injury", "injured", "injuries", "hurt", "questionable", "doubtful", "limited",
                                "expected to (play|return)", "won'?t (play|return)", "will (not )?play",
                                "ruled out", "ir", "injured reserve", "practice", "status", "activated", "cleared",
                                "sidelined", "inactive", "game[- ]time decision", "carted off", INJURY_PARTS))
  # Not practicing is its own kind of injury news: say so plainly.
  practice_absence <- str_detect(t, "(out of|absent from|away from) practice|not (spotted|seen|present)|(sits?|sat) out|held out|did not practice|miss(es|ed)? practice|dnp")
  if (practice_absence && score < 0) {
    still <- str_detect(t, "(remains|stays|still|again) (out of|absent|away)")
    return(out("injury", if (still || score <= -3) "negative" else "slightly_negative",
               paste0(who, if (still) " is still not practicing" else " wasn't practicing", part_txt,
                      ". That's a bad sign for this week: check the final injury report on Friday before you start him.")))
  }

  if (injury_words || has_word(t, c("torn", "tears", "surgery", "season[- ]ending", "returns?", "returning"))) {
    if (score >= 1) {
      return(out("injury_return", "positive",
                 paste0("Positive health update for ", who, part_txt, ". He is trending toward playing.")))
    }
    if (score <= -4) {
      return(out("injury", "negative",
                 paste0("Serious injury news for ", who, part_txt, ". Expect him to miss significant time.")))
    }
    if (score <= -3) {
      return(out("injury", "negative",
                 paste0(who, " is dealing with an injury", part_txt,
                        " that could keep him out. Check his status before lineups lock.")))
    }
    if (score < 0) {
      return(out("injury", "slightly_negative",
                 paste0(who, " has an injury note", part_txt, ". Monitor practice reports before kickoff.")))
    }
  }

  if (has_word(t, c("traded", "trade", "acquire[sd]?", "signs?", "signed", "signing", "released", "waived",
                    "cut", "claimed", "re-sign(s|ed)?", "extension", "contract", "free agent", "practice squad",
                    "elevated", "promoted"))) {
    return(out("transaction", "roster_move",
               paste0("Roster move involving ", who, ". His role and fantasy value could change with it.")))
  }

  if (has_word(t, c("start/sit", "start or sit", "waiver", "rankings?", "projections?", "matchups?",
                    "sleepers?", "fantasy", "dfs", "buy low", "sell high")) && abs(score) < 2) {
    return(out("fantasy_advice", "neutral", paste0("Fantasy advice article that mentions ", who, ".")))
  }

  if (score >= 1) {
    return(out("performance", "positive", paste0("Good news for ", who, ": a strong performance or bigger role.")))
  }
  if (score <= -1) {
    return(out("performance", if (score <= -3) "negative" else "slightly_negative",
               paste0("Concerning news about ", who, "'s role or production.")))
  }

  out("general", "neutral", paste0("General news mentioning ", who, "."))
}

# ---------------------
# Picking varied articles
# ---------------------
STOPWORDS <- c("the", "a", "an", "and", "or", "of", "to", "in", "on", "for", "with", "vs", "at", "is", "his", "he",
               "as", "after", "from", "by", "be", "will", "week", "nfl", "s")

title_words <- function(title, ignore = character(0)) {
  w <- str_split(normalize_text(title), " ")[[1]]
  unique(w[nchar(w) > 1 & !(w %in% STOPWORDS) & !(w %in% ignore)])
}

# Word overlap between two headlines, ignoring the player's own name (every headline has it).
title_similarity <- function(a, b, player_name = "") {
  ignore <- str_split(normalize_text(player_name), " ")[[1]]
  wa <- title_words(a, ignore); wb <- title_words(b, ignore)
  if (length(wa) == 0 || length(wb) == 0) return(0)
  length(intersect(wa, wb)) / length(union(wa, wb))
}

# Pick up to n articles, newest first, skipping ones that repeat a story already picked:
# a near-duplicate headline (similarity >= 0.45) is always skipped, and a second article in the
# same category is only taken if nothing from a new category is left.
select_diverse <- function(articles, n = 3, max_similarity = 0.45) {
  if (length(articles) == 0) return(list())
  dates <- vapply(articles, function(a) substr(a$published %||% "", 1, 10), character(1))
  # Newest first; on the same day, articles with a picture first.
  has_image <- vapply(articles, function(a) !is.null(a[["image"]]), logical(1))
  articles <- articles[order(dates, has_image, decreasing = TRUE)]
  picked <- list()
  similar_to_picked <- function(a) {
    any(vapply(picked, function(p) title_similarity(p$title, a$title, a$player %||% "") >= max_similarity, logical(1)))
  }
  for (pass in 1:2) {
    for (a in articles) {
      if (length(picked) >= n) break
      if (any(vapply(picked, function(p) identical(p$link, a$link), logical(1)))) next
      if (length(picked) > 0 && similar_to_picked(a)) next
      used_categories <- vapply(picked, function(p) p$category %||% "", character(1))
      if (pass == 1 && (a$category %||% "") %in% used_categories) next
      picked <- c(picked, list(a))
    }
  }
  picked
}
