# Run from the repo root: Rscript scripts/tests/test_article_images.R
source("scripts/article_images.R")

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

# Reuse from the previous run; no lookups when the budget is 0.
prev <- list("https://a" = list(url = "https://real/a", image = "https://img/a.jpg"))
budget <- new_image_budget(0)
out <- attach_images(list(list(link = "https://a"), list(link = "https://b")), prev, budget, delay = 0)
check("reused url", out[[1]]$url, "https://real/a")
check("reused image", out[[1]]$image, "https://img/a.jpg")
check("unknown left alone when no budget", is.null(out[[2]][["image"]]), TRUE)

# Fallback image tags when there's no og:image
check("link rel image_src", extract_share_image('<link rel="image_src" href="/i/a.jpg">', "https://www.usatoday.com/story/x"), "https://www.usatoday.com/i/a.jpg")
check("itemprop image", extract_share_image('<meta itemprop="image" content="https://cdn.x.com/b.jpg">', "https://x.com"), "https://cdn.x.com/b.jpg")
check("JSON-LD image object", extract_share_image('<script type="application/ld+json">{"@type":"NewsArticle","image":{"@type":"ImageObject","url":"https:\\/\\/a.espncdn.com\\/photo\\/c.jpg"}}</script>', "https://www.espn.com/nfl/story"), "https://a.espncdn.com/photo/c.jpg")
check("JSON-LD image list", extract_share_image('{"image":["https://img.x.com/d.jpg"]}', "https://x.com"), "https://img.x.com/d.jpg")
check("YouTube thumbnail", youtube_thumbnail("https://youtu.be/daWf3JrO7Lw?is=3r7"), "https://i.ytimg.com/vi/daWf3JrO7Lw/hqdefault.jpg")
check("YouTube watch link", youtube_thumbnail("https://www.youtube.com/watch?v=daWf3JrO7Lw"), "https://i.ytimg.com/vi/daWf3JrO7Lw/hqdefault.jpg")
check("not YouTube", youtube_thumbnail("https://www.espn.com/x"), NA_character_)

# Lookup plan: newest first, retry misses up to 3 tries, skip ones that already have images
arts <- list(qb = list(list(link = "old", published = "2026-10-01"), list(link = "new", published = "2026-11-03")),
             rb = list(list(link = "mid", published = "2026-10-20"), list(link = "done", published = "2026-11-04"),
                       list(link = "missed", published = "2026-11-02"), list(link = "gaveup", published = "2026-11-02")))
prev <- list(done = list(image = "x"), missed = list(url = "https://real", image_tries = 1), gaveup = list(url = "https://r2", image_tries = 3))
check("plan order", plan_image_lookups(arts, prev, 10), c("new", "missed", "mid", "old"))
check("plan respects budget", plan_image_lookups(arts, prev, 2), c("new", "missed"))
b0 <- new_image_budget(0)
kept <- attach_images(list(list(link = "missed")), prev, b0, delay = 0, lookups = character(0))
check("tries carried over", kept[[1]]$image_tries, 1)

# Video pages: the cover frame stands in for a share image (round 12)
ld_video <- '<html><head><script type="application/ld+json">{"@type":"VideoObject","name":"Highlights","thumbnailUrl":["https:\\/\\/cdn.example.com\\/v\\/cover.jpg"]}</script></head></html>'
check("JSON-LD video thumbnail", extract_share_image(ld_video, "https://www.example.com/video/1"), "https://cdn.example.com/v/cover.jpg")
poster_page <- '<html><body><video class="player" poster="/img/poster.jpg" src="/v.mp4"></video></body></html>'
check("<video poster> frame", extract_share_image(poster_page, "https://www.example.com/watch/2"), "https://www.example.com/img/poster.jpg")
og_and_video <- '<meta property="og:image" content="https://cdn.example.com/og.jpg"><video poster="/p.jpg"></video>'
check("share image still wins", extract_share_image(og_and_video, "https://www.example.com/x"), "https://cdn.example.com/og.jpg")
check("video links", c(is_video_link("https://www.youtube.com/watch?v=abc"), is_video_link("https://www.nfl.com/videos/highlights-1"), is_video_link("https://www.espn.com/video/clip?id=1"), is_video_link("https://www.espn.com/nfl/story/_/id/1")), c(TRUE, TRUE, TRUE, FALSE))
vid <- attach_images(list(list(link = "https://youtu.be/abcdefghijk")), list(), new_image_budget(1), delay = 0, lookups = "https://youtu.be/abcdefghijk")
check("video flagged and given its thumbnail", c(isTRUE(vid[[1]]$video), vid[[1]]$image), c("TRUE", "https://i.ytimg.com/vi/abcdefghijk/hqdefault.jpg"))

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll article image tests passed\n")
