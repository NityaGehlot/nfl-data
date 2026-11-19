# Run from the repo root: Rscript scripts/tests/test_link_cache.R
source("scripts/link_cache.R")

failures <- 0
check <- function(desc, actual, expected) {
  if (!identical(actual, expected)) {
    failures <<- failures + 1
    cat("FAIL:", desc, "\n  expected:", format(expected), "\n  actual:  ", format(actual), "\n")
  } else cat("ok:", desc, "\n")
}

now <- as.POSIXct("2026-11-18 12:00:00", tz = "UTC")
hrs <- function(h) format(now - h * 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

check("google key is the article id", cache_key("https://news.google.com/rss/articles/CBMiABC?oc=5"), "CBMiABC")
check("other links key on themselves", cache_key("https://x.com/a"), "https://x.com/a")

tmp <- tempfile(fileext = ".json"); writeLines("{not json", tmp)
check("corrupt cache file -> empty", load_link_cache(tmp), list())
check("missing cache file -> empty", load_link_cache(file.path(tempdir(), "nope.json")), list())

check("never seen -> look up", needs_lookup(NULL, now), TRUE)
check("found image -> never again", needs_lookup(list(image = "https://i/a.jpg", tries = 1, last_try = hrs(100)), now), FALSE)
check("1 try, 1h ago -> wait", needs_lookup(list(tries = 1, last_try = hrs(1)), now), FALSE)
check("1 try, 2h ago -> retry", needs_lookup(list(tries = 1, last_try = hrs(2)), now), TRUE)
check("2 tries, 3h ago -> wait (needs 4h)", needs_lookup(list(tries = 2, last_try = hrs(3)), now), FALSE)
check("4 tries -> give up", needs_lookup(list(tries = 4, last_try = hrs(500)), now), FALSE)

c1 <- record_result(list(), "k", NA_character_, NA_character_, now)
check("failure counts a try", c1$k$tries, 1)
check("failure time saved", c1$k$last_try, hrs(0))
c2 <- record_result(c1, "k", "https://real/a", NA_character_, now)
check("url kept on second failure", c2$k$url, "https://real/a")
check("second try counted", c2$k$tries, 2)
c3 <- record_result(c2, "k", "https://real/a", "https://img/a.jpg", now)
check("image saved", c3$k$image, "https://img/a.jpg")

seeded <- seed_cache_from_articles(list(), list(
  list(link = "https://news.google.com/rss/articles/AAA", url = "https://real/x", image = "https://img/x.jpg"),
  list(link = "https://news.google.com/rss/articles/BBB", image_tries = 2),
  list(link = "https://news.google.com/rss/articles/CCC")
), now)
check("seed copies url", seeded$AAA$url, "https://real/x")
check("seed copies image", seeded$AAA$image, "https://img/x.jpg")
check("seed maps image_tries", seeded$BBB$tries, 2)
check("seed skips never-looked-up", is.null(seeded$CCC), TRUE)

old <- list(a = list(seen = "2026-10-03"), b = list(seen = "2026-10-05"))
check("prune drops 46-day-old, keeps 44", names(prune_cache(old, as.Date("2026-11-18"))), "b")

f <- tempfile(fileext = ".json"); save_link_cache(c3, f)
check("save/load round trip", load_link_cache(f)$k$image, "https://img/a.jpg")

cat(if (failures == 0) "All link cache tests passed\n" else sprintf("%d failure(s)\n", failures))
quit(status = if (failures == 0) 0 else 1)
