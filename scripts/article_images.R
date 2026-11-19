# scripts/article_images.R
# Real links and cover images for news articles.
#
# Google News RSS gives redirect links (news.google.com/rss/articles/...) and no pictures.
# For each new article we ask Google for the real article address (the same lookup the
# Google News website does: read a signature from the article page, then call its
# "batchexecute" endpoint), open the publisher's page and read its share image
# (og:image / twitter:image), the picture you see when the article is shared.
# Results are reused from the previous run's files, so each article is looked up once.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})
source("scripts/link_cache.R")

IMG_UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"

decode_entities <- function(s) {
  s <- gsub("&amp;", "&", s, fixed = TRUE)
  s <- gsub("&quot;", "\"", s, fixed = TRUE)
  s <- gsub("&#39;", "'", s, fixed = TRUE)
  s
}

# Absolute https URL of the page's share image, or NA.
extract_share_image <- function(html, page_url = "") {
  if (is.null(html) || is.na(html) || !nzchar(html)) return(NA_character_)
  tags <- regmatches(html, gregexpr("<meta\\b[^>]*>", html, ignore.case = TRUE, perl = TRUE))[[1]]
  wanted <- c("og:image:secure_url", "og:image", "og:image:url", "twitter:image", "twitter:image:src")
  for (name in wanted) {
    for (tag in tags) {
      key <- tolower(sub(".*\\b(?:property|name)\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1", tag, perl = TRUE))
      if (identical(key, tolower(tag)) || key != name) next
      content <- sub(".*\\bcontent\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1", tag, perl = TRUE)
      if (identical(content, tag) || !nzchar(content)) next
      url <- decode_entities(trimws(content))
      if (startsWith(url, "//")) url <- paste0("https:", url)
      if (startsWith(url, "/")) {
        origin <- sub("^(https?://[^/]+).*", "\\1", page_url)
        url <- paste0(origin, url)
      }
      url <- sub("^http://", "https://", url)
      if (grepl("^https://", url) && !is_default_image(url)) return(url)
    }
  }
  other_image(html, page_url)
}

# A site's generic picture (logo, sprite, placeholder...), not the article's own.
is_default_image <- function(url) {
  file <- tolower(sub("[?#].*$", "", url))
  grepl("logo|sprite|placeholder|default|favicon|blank", file)
}

# The article's biggest picture, when the page has no share image: <img> src / data-src /
# srcset, preferring the largest width hint. Tiny images (icons) and site defaults don't count.
largest_article_img <- function(html, page_url) {
  # Look inside the article body when the page marks one (skips header/nav/author images).
  body <- regmatches(html, regexpr("(?is)<article\\b.*</article>", html, perl = TRUE))
  if (length(body) == 1 && nzchar(body)) html <- body
  tags <- regmatches(html, gregexpr("<img\\b[^>]*>", html, ignore.case = TRUE, perl = TRUE))[[1]]
  best <- NA_character_
  best_w <- -1
  attr_of <- function(tag, name) {
    m <- regmatches(tag, regexec(paste0("\\b", name, "\\s*=\\s*[\"']([^\"']+)[\"']"), tag, ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(m) >= 2) m[2] else NA_character_
  }
  for (tag in tags) {
    cands <- list()
    srcset <- attr_of(tag, "(?:data-)?srcset")
    if (!is.na(srcset)) {
      for (part in strsplit(srcset, ",")[[1]]) {
        bits <- strsplit(trimws(part), "\\s+")[[1]]
        w <- if (length(bits) >= 2 && grepl("w$", bits[2])) as.numeric(sub("w$", "", bits[2])) else 0
        cands[[length(cands) + 1]] <- list(url = bits[1], w = w)
      }
    }
    width <- suppressWarnings(as.numeric(attr_of(tag, "width")))
    for (name in c("data-src", "data-lazy-src", "src")) {
      u <- attr_of(tag, name)
      if (!is.na(u) && !startsWith(u, "data:")) cands[[length(cands) + 1]] <- list(url = u, w = if (is.na(width)) 0 else width)
    }
    for (c in cands) {
      if (!is.na(width) && width < 200 && c$w < 200) next # icons, avatars
      u <- absolute_https(c$url, page_url)
      if (is.na(u) || is_default_image(u)) next
      if (c$w > best_w) { best <- u; best_w <- c$w }
    }
  }
  best
}

absolute_https <- function(url, page_url) {
  url <- decode_entities(trimws(url))
  if (startsWith(url, "//")) url <- paste0("https:", url)
  if (startsWith(url, "/")) url <- paste0(sub("^(https?://[^/]+).*", "\\1", page_url), url)
  url <- sub("^http://", "https://", url)
  if (grepl("^https://", url)) url else NA_character_
}

# Fallbacks when there's no og/twitter image: <link rel="image_src">, itemprop="image",
# then the "image" in the page's JSON-LD (structured data most news sites include).
other_image <- function(html, page_url) {
  ok <- function(u) !is.na(u) && !is_default_image(u)
  link <- regmatches(html, regexec("<link[^>]+rel=[\"']image_src[\"'][^>]*href=[\"']([^\"']+)[\"']", html, ignore.case = TRUE))[[1]]
  if (length(link) >= 2 && ok(u <- absolute_https(link[2], page_url))) return(u)
  ip <- regmatches(html, regexec("itemprop=[\"']image[\"'][^>]*(?:content|src)=[\"']([^\"']+)[\"']", html, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(ip) >= 2 && ok(u <- absolute_https(ip[2], page_url))) return(u)
  ld <- regmatches(html, regexec("\"image\"\\s*:\\s*(?:\\{[^}]*?\"url\"\\s*:\\s*)?\\[?\\s*\"(https?:[^\"]+)\"", html, perl = TRUE))[[1]]
  if (length(ld) >= 2 && ok(u <- absolute_https(gsub("\\\\/", "/", ld[2]), page_url))) return(u)
  vc <- video_cover(html, page_url)
  if (ok(vc)) return(vc)
  largest_article_img(html, page_url)
}

# Pages built around a video often have no share image, only the video's cover: the JSON-LD
# VideoObject "thumbnailUrl", or the <video poster="..."> frame shown before it plays.
video_cover <- function(html, page_url) {
  th <- regmatches(html, regexec("\"thumbnailUrl\"\\s*:\\s*\\[?\\s*\"(https?:[^\"]+)\"", html, perl = TRUE))[[1]]
  if (length(th) >= 2) return(absolute_https(gsub("\\\\/", "/", th[2]), page_url))
  poster <- regmatches(html, regexec("<video\\b[^>]*\\bposter=[\"']([^\"']+)[\"']", html, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(poster) >= 2) return(absolute_https(poster[2], page_url))
  NA_character_
}

#' Is this link a video (so the app can mark the card with a play button)?
is_video_link <- function(url) {
  u <- tolower(url %||% "")
  grepl("youtu\\.be/|youtube\\.com/|vimeo\\.com/|/videos?/|/watch/|espn\\.com/video/|nfl\\.com/videos/", u, perl = TRUE)
}

# YouTube links: the video's thumbnail.
youtube_thumbnail <- function(url) {
  id <- regmatches(url, regexec("(?:youtu\\.be/|youtube\\.com/(?:watch\\?v=|shorts/|embed/))([A-Za-z0-9_-]{11})", url, perl = TRUE))[[1]]
  if (length(id) < 2) NA_character_ else paste0("https://i.ytimg.com/vi/", id[2], "/hqdefault.jpg")
}

is_google_news_link <- function(url) grepl("^https?://news\\.google\\.com/(rss/)?articles/", url)

google_article_id <- function(url) {
  m <- regmatches(url, regexec("/articles/([^?/#]+)", url))[[1]]
  if (length(m) < 2) NA_character_ else m[2]
}

extract_decoding_params <- function(html) {
  sg <- regmatches(html, regexec("data-n-a-sg=\"([^\"]+)\"", html))[[1]]
  ts <- regmatches(html, regexec("data-n-a-ts=\"([^\"]+)\"", html))[[1]]
  if (length(sg) < 2 || length(ts) < 2) return(NULL)
  list(signature = sg[2], timestamp = ts[2])
}

build_batch_body <- function(id, timestamp, signature) {
  inner <- sprintf(
    '["garturlreq",[["X","X",["X","X"],null,null,1,1,"US:en",null,1,null,null,null,null,null,0,1],"X","X",1,[1,1,1],1,1,null,0,0,null,0],"%s",%s,"%s"]',
    id, timestamp, signature
  )
  paste0("f.req=", URLencode(toJSON(list(list(list("Fbv4je", inner))), auto_unbox = TRUE), reserved = TRUE))
}

# The article URL in Google's batchexecute reply, or NA.
parse_batch_response <- function(text) {
  parts <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
  chunk <- if (length(parts) >= 2) parts[2] else sub("^\\)\\]\\}'\\s*", "", text)
  outer <- tryCatch(fromJSON(chunk, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(outer)) return(NA_character_)
  payload <- tryCatch(fromJSON(outer[[1]][[3]], simplifyVector = FALSE), error = function(e) NULL)
  url <- if (!is.null(payload) && length(payload) >= 2) payload[[2]] else NULL
  if (is.character(url) && grepl("^https?://", url)) url else NA_character_
}

http_text <- function(url, method = "GET", body = NULL) {
  res <- tryCatch(
    if (method == "POST") {
      POST(url, body = body, encode = "raw", timeout(10), user_agent(IMG_UA),
           add_headers(`Content-Type` = "application/x-www-form-urlencoded;charset=UTF-8"))
    } else {
      GET(url, timeout(10), user_agent(IMG_UA))
    },
    error = function(e) NULL
  )
  if (is.null(res)) return(list(status = 0, text = NA_character_, url = url))
  list(status = status_code(res), text = if (status_code(res) < 400) content(res, as = "text", encoding = "UTF-8") else NA_character_, url = res$url)
}

resolve_google_link <- function(url) {
  id <- google_article_id(url)
  if (is.na(id)) return(list(url = NA_character_, rate_limited = FALSE))
  page <- http_text(paste0("https://news.google.com/rss/articles/", id))
  if (page$status == 429) return(list(url = NA_character_, rate_limited = TRUE))
  if (!is.na(page$url) && !grepl("news\\.google\\.com", page$url)) return(list(url = page$url, rate_limited = FALSE))
  params <- if (!is.na(page$text)) extract_decoding_params(page$text) else NULL
  if (is.null(params)) return(list(url = NA_character_, rate_limited = FALSE))
  res <- http_text("https://news.google.com/_/DotsSplashUi/data/batchexecute", "POST",
                   build_batch_body(id, params$timestamp, params$signature))
  if (res$status == 429) return(list(url = NA_character_, rate_limited = TRUE))
  list(url = if (is.na(res$text)) NA_character_ else parse_batch_response(res$text), rate_limited = FALSE)
}

# Share image for a publisher page (a YouTube link uses the video thumbnail). Sites that block
# automated requests simply return no image; the app then shows its placeholder.
page_image <- function(url) {
  yt <- youtube_thumbnail(url)
  if (!is.na(yt)) return(yt)
  page <- http_text(url)
  extract_share_image(page$text, page$url %||% url)
}

# Real URL + share image for one link. NA fields when unknown.
fetch_article_image <- function(link) {
  real <- if (is_google_news_link(link)) resolve_google_link(link) else list(url = link, rate_limited = FALSE)
  if (is.na(real$url)) return(list(url = NA_character_, image = NA_character_, rate_limited = real$rate_limited))
  list(url = real$url, image = page_image(real$url), rate_limited = FALSE)
}


# The lookup budget for one run: how many articles, until when, and what happened.
new_image_budget <- function(n, minutes = Inf, clock = Sys.time) {
  b <- new.env()
  # A bad setting (e.g. "90m" typed into the workflow form) falls back to the normal 15 minutes.
  if (length(minutes) != 1 || is.na(minutes) || minutes < 0) minutes <- 15
  if (length(n) != 1 || is.na(n)) n <- 1500
  b$left <- n
  b$deadline <- clock() + minutes * 60
  b$clock <- clock
  b$google_stopped <- FALSE
  b$looked_up <- 0
  b$found <- 0
  b$no_image <- 0
  b$rate_limited <- 0
  b
}

# Which articles (cache keys) to look up this run: newest first across every position file,
# each article once, following the link cache's retry rules.
plan_image_lookups <- function(article_lists, cache, n, now) {
  all <- unlist(unname(article_lists), recursive = FALSE)
  if (length(all) == 0 || n <= 0) return(character(0))
  need <- Filter(function(a) is.null(a[["image"]]) && needs_lookup(cache[[cache_key(a$link)]], now), all)
  if (length(need) == 0) return(character(0))
  dates <- vapply(need, function(a) a$published %||% "", character(1))
  keys <- unique(vapply(need[order(dates, decreasing = TRUE)], function(a) cache_key(a$link), character(1)))
  head(keys, n)
}

# Adds `url` (real article link) and `image` to articles from the link cache, and looks up the
# planned articles (`lookups`, cache keys) while the budget lasts. Google lookups are paced
# (`delay` seconds) and stop for the run if Google says "slow down"; pages whose real address is
# already known are still read. Returns list(articles, cache).
attach_images <- function(articles, cache, budget, lookups, now = Sys.time(), fetch = fetch_article_image,
                          page = page_image, delay = 1, sleep = Sys.sleep) {
  for (i in seq_along(articles)) {
    a <- articles[[i]]
    a$image_tries <- NULL
    key <- cache_key(a$link)
    e <- cache[[key]]
    if (!is.null(e[["url"]]) && is.null(a[["url"]])) a$url <- e[["url"]]
    if (!is.null(e[["image"]]) && is.null(a[["image"]])) a$image <- e[["image"]]
    can_look <- is.null(a[["image"]]) && key %in% lookups && needs_lookup(e, now) &&
      budget$left > 0 && budget$clock() < budget$deadline
    known_url <- a[["url"]] %||% NA_character_
    if (can_look && (!is.na(known_url) || !budget$google_stopped || !is_google_news_link(a$link))) {
      budget$left <- budget$left - 1
      asks_google <- is.na(known_url) && is_google_news_link(a$link)
      got <- if (!is.na(known_url)) list(url = known_url, image = page(known_url), rate_limited = FALSE) else fetch(a$link)
      if (isTRUE(got$rate_limited)) {
        # Not the article's fault: don't count it as a failed try.
        budget$google_stopped <- TRUE
        budget$rate_limited <- budget$rate_limited + 1
      } else {
        budget$looked_up <- budget$looked_up + 1
        if (!is.na(got$image)) budget$found <- budget$found + 1 else budget$no_image <- budget$no_image + 1
        cache <- record_result(cache, key, got$url, got$image, now)
        if (!is.na(got$url)) a$url <- got$url
        if (!is.na(got$image)) a$image <- got$image
      }
      # Pace Google (it rate-limits); publisher pages are each a different site.
      if (asks_google && delay > 0) sleep(delay)
    }
    if (is_video_link(a[["url"]] %||% a$link)) a$video <- TRUE
    articles[[i]] <- a
  }
  list(articles = articles, cache = cache)
}

# One line for the run log: what was looked up and how many articles now have a picture.
image_summary <- function(budget, article_lists) {
  all <- unlist(unname(article_lists), recursive = FALSE)
  with_img <- sum(vapply(all, function(a) !is.null(a[["image"]]), logical(1)))
  total <- length(all)
  sprintf("Images: %d looked up (%d found, %d no image, %d rate-limited) - %d of %d articles have a picture (%d%%)",
          budget$looked_up, budget$found, budget$no_image, budget$rate_limited, with_img, total,
          if (total) round(100 * with_img / total) else 0L)
}
