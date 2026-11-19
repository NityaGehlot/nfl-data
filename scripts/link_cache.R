# scripts/link_cache.R
# Permanent memory of article lookups (data/news/link_cache.json): for each article, its real
# address and cover image, or how many times the lookup failed. Kept across runs so no article is
# looked up twice, even after it rotates out of the news files.
#   key (Google article id, or the link itself) -> list(url, image, tries, last_try, seen)

suppressPackageStartupMessages(library(jsonlite))

MAX_LOOKUP_TRIES <- 4
CACHE_KEEP_DAYS <- 45

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

iso_time <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Google News links change their query string but keep the article id.
cache_key <- function(link) {
  m <- regmatches(link, regexec("^https?://news\\.google\\.com/(?:rss/)?articles/([^?/#]+)", link, perl = TRUE))[[1]]
  if (length(m) >= 2) m[2] else link
}

load_link_cache <- function(path) {
  if (!file.exists(path)) return(list())
  out <- tryCatch(fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(out) || (length(out) > 0 && is.null(names(out)))) list() else out
}

save_link_cache <- function(cache, path) {
  write_json(if (length(cache)) cache else structure(list(), names = character(0)), path, auto_unbox = TRUE, null = "null")
}

# Look this article up now? Never seen: yes. Has an image: never again. Failed: after 2^tries
# hours since the last try, up to MAX_LOOKUP_TRIES tries.
needs_lookup <- function(entry, now) {
  if (is.null(entry)) return(TRUE)
  if (!is.null(entry[["image"]])) return(FALSE)
  tries <- as.numeric(entry[["tries"]] %||% 0)
  if (tries >= MAX_LOOKUP_TRIES) return(FALSE)
  last <- as.POSIXct(entry[["last_try"]] %||% "1970-01-01T00:00:00Z", format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  as.numeric(difftime(now, last, units = "hours")) >= 2^tries
}

# Save one lookup's result. url/image NA when not found.
record_result <- function(cache, key, url, image, now) {
  e <- cache[[key]] %||% list()
  if (!is.na(url)) e$url <- url
  if (!is.na(image)) {
    e$image <- image
  } else {
    e$tries <- as.numeric(e[["tries"]] %||% 0) + 1
  }
  e$last_try <- iso_time(now)
  e$seen <- format(as.Date(now), "%Y-%m-%d")
  cache[[key]] <- e
  cache
}

# Bring over what the previous run's news files already know (url / image / failed tries).
seed_cache_from_articles <- function(cache, articles, now) {
  for (a in articles) {
    if (is.null(a$link)) next
    k <- cache_key(a$link)
    if (!is.null(cache[[k]])) next
    has <- !is.null(a[["url"]]) || !is.null(a[["image"]]) || !is.null(a[["image_tries"]])
    if (!has) next
    e <- list(seen = format(as.Date(now), "%Y-%m-%d"), last_try = iso_time(now))
    if (!is.null(a[["url"]])) e$url <- a[["url"]]
    if (!is.null(a[["image"]])) e$image <- a[["image"]]
    if (!is.null(a[["image_tries"]])) e$tries <- as.numeric(a[["image_tries"]])
    cache[[k]] <- e
  }
  cache
}

# Mark articles still in the news as seen today (so they aren't pruned).
touch_seen <- function(cache, keys, today) {
  for (k in intersect(keys, names(cache))) cache[[k]]$seen <- format(today, "%Y-%m-%d")
  cache
}

prune_cache <- function(cache, today, days = CACHE_KEEP_DAYS) {
  keep <- vapply(cache, function(e) {
    seen <- tryCatch(as.Date(e[["seen"]] %||% "1970-01-01"), error = function(err) as.Date("1970-01-01"))
    as.numeric(today - seen) <= days
  }, logical(1))
  cache[keep]
}
