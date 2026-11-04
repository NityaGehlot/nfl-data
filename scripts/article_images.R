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
      if (grepl("^https://", url)) return(url)
    }
  }
  other_image(html, page_url)
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
  link <- regmatches(html, regexec("<link[^>]+rel=[\"']image_src[\"'][^>]*href=[\"']([^\"']+)[\"']", html, ignore.case = TRUE))[[1]]
  if (length(link) >= 2) return(absolute_https(link[2], page_url))
  ip <- regmatches(html, regexec("itemprop=[\"']image[\"'][^>]*(?:content|src)=[\"']([^\"']+)[\"']", html, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(ip) >= 2) return(absolute_https(ip[2], page_url))
  ld <- regmatches(html, regexec("\"image\"\\s*:\\s*(?:\\{[^}]*?\"url\"\\s*:\\s*)?\\[?\\s*\"(https?:[^\"]+)\"", html, perl = TRUE))[[1]]
  if (length(ld) >= 2) return(absolute_https(gsub("\\\\/", "/", ld[2]), page_url))
  NA_character_
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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# link -> list(url, image) from the news files already on disk (last run).
previous_image_index <- function(dir) {
  index <- list()
  for (f in list.files(dir, pattern = "^news_.*\\.json$", full.names = TRUE)) {
    if (basename(f) == "news_meta.json") next
    arts <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) list())
    for (a in arts) {
      if (!is.null(a$link) && (!is.null(a[["image"]]) || !is.null(a[["url"]]) || !is.null(a[["image_tries"]]))) {
        index[[a$link]] <- list(url = a[["url"]], image = a[["image"]], image_tries = a[["image_tries"]])
      }
    }
  }
  index
}

MAX_IMAGE_TRIES <- 3

# Which links to look up this run: newest articles first (across every position file), those
# never looked up or whose picture failed before (up to MAX_IMAGE_TRIES attempts).
plan_image_lookups <- function(article_lists, previous, n) {
  all <- unlist(unname(article_lists), recursive = FALSE)
  if (length(all) == 0 || n <= 0) return(character(0))
  need <- Filter(function(a) {
    prev <- previous[[a$link]]
    is.null(prev) || (is.null(prev[["image"]]) && (prev[["image_tries"]] %||% 1) < MAX_IMAGE_TRIES)
  }, all)
  if (length(need) == 0) return(character(0))
  dates <- vapply(need, function(a) a$published %||% "", character(1))
  links <- unique(vapply(need[order(dates, decreasing = TRUE)], function(a) a$link, character(1)))
  head(links, n)
}

# Adds `url` (real article link) and `image` to articles: reuses the previous run's results and
# looks up the links in `lookups` (from plan_image_lookups; NULL = only never-seen links).
# Stops new lookups if Google rate-limits (budget$stopped).
attach_images <- function(articles, previous, budget, delay = 0.4, lookups = NULL) {
  for (i in seq_along(articles)) {
    a <- articles[[i]]
    prev <- previous[[a$link]]
    if (!is.null(prev)) {
      # [[ ]] not $: R's $ would match "image" to "image_tries".
      if (!is.null(prev[["url"]])) a$url <- prev[["url"]]
      if (!is.null(prev[["image"]])) a$image <- prev[["image"]]
      if (!is.null(prev[["image_tries"]])) a$image_tries <- prev[["image_tries"]]
    }
    wanted <- if (is.null(lookups)) is.null(prev) else a$link %in% lookups
    if (is.null(a[["image"]]) && wanted && budget$left > 0 && !budget$stopped) {
      budget$left <- budget$left - 1
      # A known real URL only needs the picture; otherwise resolve the Google link first.
      got <- if (!is.null(a[["url"]])) list(url = a[["url"]], image = page_image(a[["url"]]), rate_limited = FALSE) else fetch_article_image(a$link)
      if (isTRUE(got$rate_limited)) budget$stopped <- TRUE
      if (!is.na(got$url)) a$url <- got$url
      if (!is.na(got$image)) a$image <- got$image else a$image_tries <- (a[["image_tries"]] %||% 0) + 1
      Sys.sleep(delay)
    }
    articles[[i]] <- a
  }
  articles
}

new_image_budget <- function(n) {
  b <- new.env()
  b$left <- n
  b$stopped <- FALSE
  b
}
