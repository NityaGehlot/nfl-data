# Run from the repo root: Rscript scripts/tests/test_article_images.R
source("scripts/article_images.R")
source("scripts/link_cache.R")

failures <- 0
check <- function(desc, actual, expected) {
  if (!identical(actual, expected)) {
    failures <<- failures + 1
    cat("FAIL:", desc, "\n  expected:", format(expected), "\n  actual:  ", format(actual), "\n")
  } else cat("ok:", desc, "\n")
}

check("og:image", extract_share_image('<head><meta property="og:image" content="https://cdn.x.com/a.jpg?w=1&amp;h=2"></head>', "https://x.com/s"), "https://cdn.x.com/a.jpg?w=1&h=2")
check("twitter:image, relative, name first", extract_share_image('<meta name="twitter:image" content="/img/b.png">', "https://site.com/news/1"), "https://site.com/img/b.png")
check("content before property", extract_share_image('<meta content="//cdn.y.com/c.jpg" property="og:image" />', "https://y.com"), "https://cdn.y.com/c.jpg")
check("http upgraded", extract_share_image('<meta property="og:image" content="http://z.com/d.jpg">', "https://z.com"), "https://z.com/d.jpg")
check("no image", extract_share_image("<p>nothing</p>", "https://z.com"), NA_character_)

link <- "https://news.google.com/rss/articles/CBMiABC123?oc=5"
check("google link detected", is_google_news_link(link), TRUE)
check("publisher link not google", is_google_news_link("https://www.espn.com/nfl/story/_/id/1"), FALSE)
check("article id", google_article_id(link), "CBMiABC123")
check("decoding params", extract_decoding_params('<c-wiz><div jscontroller="x" data-n-a-sg="SIG" data-n-a-ts="1730000000"></div>'), list(signature = "SIG", timestamp = "1730000000"))
body <- URLdecode(sub("^f\\.req=", "", build_batch_body("ID", "123", "SIG")))
check("batch body has request name", grepl("Fbv4je", body, fixed = TRUE), TRUE)
check("batch body has id/ts/sig", grepl('\\"ID\\",123,\\"SIG\\"', body, fixed = TRUE), TRUE)
reply <- paste0(")]}'\n\n", toJSON(list(list("wrb.fr", "Fbv4je", toJSON(list("garturlres", "https://www.nbcsports.com/story"), auto_unbox = TRUE))), auto_unbox = TRUE))
check("batch reply parsed", parse_batch_response(reply), "https://www.nbcsports.com/story")
check("garbage reply", parse_batch_response("garbage"), NA_character_)

# Reuse what the link cache knows; no lookups when the budget is 0.
NOW <- as.POSIXct("2026-11-18 12:00:00", tz = "UTC")
cache0 <- list("https://a" = list(url = "https://real/a", image = "https://img/a.jpg", seen = "2026-11-18"))
res <- attach_images(list(list(link = "https://a"), list(link = "https://b")), cache0, new_image_budget(0), lookups = character(0), now = NOW, delay = 0)
check("reused url", res$articles[[1]]$url, "https://real/a")
check("reused image", res$articles[[1]]$image, "https://img/a.jpg")
check("unknown left alone when no budget", is.null(res$articles[[2]][["image"]]), TRUE)

# Fallback image tags when there's no og:image
check("link rel image_src", extract_share_image('<link rel="image_src" href="/i/a.jpg">', "https://www.usatoday.com/story/x"), "https://www.usatoday.com/i/a.jpg")
check("itemprop image", extract_share_image('<meta itemprop="image" content="https://cdn.x.com/b.jpg">', "https://x.com"), "https://cdn.x.com/b.jpg")
check("JSON-LD image object", extract_share_image('<script type="application/ld+json">{"@type":"NewsArticle","image":{"@type":"ImageObject","url":"https:\\/\\/a.espncdn.com\\/photo\\/c.jpg"}}</script>', "https://www.espn.com/nfl/story"), "https://a.espncdn.com/photo/c.jpg")
check("JSON-LD image list", extract_share_image('{"image":["https://img.x.com/d.jpg"]}', "https://x.com"), "https://img.x.com/d.jpg")
check("YouTube thumbnail", youtube_thumbnail("https://youtu.be/daWf3JrO7Lw?is=3r7"), "https://i.ytimg.com/vi/daWf3JrO7Lw/hqdefault.jpg")
check("YouTube watch link", youtube_thumbnail("https://www.youtube.com/watch?v=daWf3JrO7Lw"), "https://i.ytimg.com/vi/daWf3JrO7Lw/hqdefault.jpg")
check("not YouTube", youtube_thumbnail("https://www.espn.com/x"), NA_character_)

# Lookup plan: newest first, from the cache's retry rules; same article once
arts <- list(qb = list(list(link = "old", published = "2026-10-01"), list(link = "new", published = "2026-11-03")),
             rb = list(list(link = "mid", published = "2026-10-20"), list(link = "done", published = "2026-11-04"),
                       list(link = "missed", published = "2026-11-02"), list(link = "gaveup", published = "2026-11-02"),
                       list(link = "new", published = "2026-11-03")))
cache <- list(done = list(image = "x"), missed = list(url = "https://real", tries = 1, last_try = "2026-11-18T08:00:00Z"),
              gaveup = list(url = "https://r2", tries = 4, last_try = "2026-11-01T00:00:00Z"))
check("plan order (no duplicates)", plan_image_lookups(arts, cache, 10, NOW), c("new", "missed", "mid", "old"))
check("plan respects budget", plan_image_lookups(arts, cache, 2, NOW), c("new", "missed"))
check("plan waits for retry time", "missed" %in% plan_image_lookups(arts, cache, 10, NOW - 3 * 3600), FALSE)

# The same article in two position files is looked up once and both copies get the picture.
calls <- 0
fake_fetch <- function(link) { calls <<- calls + 1; list(url = paste0("https://real/", link), image = "https://img/new.jpg", rate_limited = FALSE) }
b <- new_image_budget(10)
r1 <- attach_images(arts$qb, list(), b, lookups = c("new"), now = NOW, fetch = fake_fetch, delay = 0)
r2 <- attach_images(arts$rb, r1$cache, b, lookups = c("new"), now = NOW, fetch = fake_fetch, delay = 0)
check("looked up once", calls, 1)
check("second copy gets the picture", r2$articles[[5]]$image, "https://img/new.jpg")

# Google says "slow down": stop Google lookups, still read pages whose real address is known.
g1 <- "https://news.google.com/rss/articles/G1"; g2 <- "https://news.google.com/rss/articles/G2"
rl_fetch <- function(link) list(url = NA_character_, image = NA_character_, rate_limited = TRUE)
pages <- 0
fake_page <- function(url) { pages <<- pages + 1; "https://img/known.jpg" }
known <- list(K = list(url = "https://real/k", tries = 1, last_try = "2026-11-01T00:00:00Z"))
b <- new_image_budget(10)
rl <- attach_images(list(list(link = g1), list(link = g2), list(link = "https://news.google.com/rss/articles/K")),
                    known, b, lookups = c("G1", "G2", "K"), now = NOW, fetch = rl_fetch, page = fake_page, delay = 0)
check("rate limit noticed", c(b$google_stopped, b$rate_limited), c(TRUE, 1))
check("rate limit doesn't count as a failed try", is.null(rl$cache$G1), TRUE)
check("known address still read", c(pages, rl$articles[[3]]$image), c("1", "https://img/known.jpg"))

# Out of time: no lookups.
late <- new_image_budget(10, minutes = 0)
lt <- attach_images(list(list(link = g1)), list(), late, lookups = "G1", now = NOW, fetch = fake_fetch, delay = 0)
check("time cap stops lookups", is.null(lt$cache$G1), TRUE)

# Summary line for the log
sb <- new_image_budget(5); sb$looked_up <- 2; sb$found <- 1; sb$no_image <- 1
check("summary line", image_summary(sb, list(qb = list(list(image = "a"), list(image = "b")), rb = list(list(image = "c"), list()))),
      "Images: 2 looked up (1 found, 1 no image, 0 rate-limited) - 3 of 4 articles have a picture (75%)")

# Video pages: the cover frame stands in for a share image (round 12)
ld_video <- '<html><head><script type="application/ld+json">{"@type":"VideoObject","name":"Highlights","thumbnailUrl":["https:\\/\\/cdn.example.com\\/v\\/cover.jpg"]}</script></head></html>'
check("JSON-LD video thumbnail", extract_share_image(ld_video, "https://www.example.com/video/1"), "https://cdn.example.com/v/cover.jpg")
poster_page <- '<html><body><video class="player" poster="/img/poster.jpg" src="/v.mp4"></video></body></html>'
check("<video poster> frame", extract_share_image(poster_page, "https://www.example.com/watch/2"), "https://www.example.com/img/poster.jpg")
og_and_video <- '<meta property="og:image" content="https://cdn.example.com/og.jpg"><video poster="/p.jpg"></video>'
check("share image still wins", extract_share_image(og_and_video, "https://www.example.com/x"), "https://cdn.example.com/og.jpg")
check("video links", c(is_video_link("https://www.youtube.com/watch?v=abc"), is_video_link("https://www.nfl.com/videos/highlights-1"), is_video_link("https://www.espn.com/video/clip?id=1"), is_video_link("https://www.espn.com/nfl/story/_/id/1")), c(TRUE, TRUE, TRUE, FALSE))
vid <- attach_images(list(list(link = "https://youtu.be/abcdefghijk")), list(), new_image_budget(1), lookups = "https://youtu.be/abcdefghijk", now = NOW, delay = 0)$articles
check("video flagged and given its thumbnail", c(isTRUE(vid[[1]]$video), vid[[1]]$image), c("TRUE", "https://i.ytimg.com/vi/abcdefghijk/hqdefault.jpg"))

# Deeper extraction: a site logo doesn't count; fall back to the article's biggest picture
logo_page <- '<meta property="og:image" content="https://site.com/logo.png"><article><img src="https://site.com/photo-1200.jpg" width="1200"></article>'
check("og logo skipped for the article photo", extract_share_image(logo_page, "https://site.com/s"), "https://site.com/photo-1200.jpg")
check("lazy data-src with width", extract_share_image('<img data-src="/a/p.jpg" width="800">', "https://host.com/x"), "https://host.com/a/p.jpg")
check("srcset largest", extract_share_image('<img srcset="/s.jpg 320w, /l.jpg 1280w, /m.jpg 640w">', "https://host.com/x"), "https://host.com/l.jpg")
check("tiny icons ignored", extract_share_image('<img src="/i/icon.png" width="24">', "https://host.com/x"), NA_character_)
check("default image detector", c(is_default_image("https://x.com/favicon.ico"), is_default_image("https://x.com/img/Default-Share.png"), is_default_image("https://x.com/p/gibbs.jpg")), c(TRUE, TRUE, FALSE))

# Review fixes
check("bad minutes value falls back to 15", is.na(new_image_budget(5, minutes = NA)$deadline), FALSE)
nav_page <- '<header><img src="/img/author-avatar.jpg"></header><article><p>x</p><img src="/photos/game.jpg"></article>'
check("article photo beats header image", extract_share_image(nav_page, "https://site.com/s"), "https://site.com/photos/game.jpg")
sleeps <- 0
counted_sleep <- function(s) sleeps <<- sleeps + 1
known2 <- list(K2 = list(url = "https://real/k2", tries = 1, last_try = "2026-11-01T00:00:00Z"))
attach_images(list(list(link = "https://news.google.com/rss/articles/K2")), known2, new_image_budget(5), lookups = "K2", now = NOW,
              page = function(u) "https://img/k2.jpg", delay = 1, sleep = counted_sleep)
check("publisher page reads aren't slowed by Google pacing", sleeps, 0)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll article image tests passed\n")
