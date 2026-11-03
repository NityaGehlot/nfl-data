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
check("unknown left alone when no budget", is.null(out[[2]]$image), TRUE)

if (failures > 0) {
  cat("\n", failures, "test(s) failed\n")
  quit(status = 1)
}
cat("\nAll article image tests passed\n")
