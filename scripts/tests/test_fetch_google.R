# Run from the repo root: Rscript scripts/tests/test_fetch_google.R
# Exercises fetch_google() from generate_nfl_news.R against a saved RSS sample (no network).
suppressPackageStartupMessages({ library(xml2); library(jsonlite); library(lubridate); library(stringr) })
source("scripts/news_helpers.R")

SEASON_START   <- as.Date("2026-09-01")
MAX_PER_PLAYER <- 2

rss <- '<?xml version="1.0"?><rss><channel>
<item><title>CeeDee Lamb says the Cardinals punched us in the mouth every time - CBS Sports</title><link>https://example.com/1</link><pubDate>Thu, 30 Oct 2026 12:00:00 GMT</pubDate></item>
<item><title>Week 8 rankings: Puka Nacua tops the list - FantasyPros</title><link>https://example.com/2</link><pubDate>Fri, 31 Oct 2026 09:00:00 GMT</pubDate></item>
<item><title>CeeDee Lamb ruled out vs. Cardinals with ankle injury - ESPN</title><link>https://example.com/3</link><pubDate>Fri, 31 Oct 2026 15:00:00 GMT</pubDate></item>
<item><title>CeeDee Lamb old preseason note - NFL.com</title><link>https://example.com/4</link><pubDate>Mon, 10 Aug 2026 15:00:00 GMT</pubDate></item>
</channel></rss>'

read_xml <- function(url) xml2::read_xml(rss)  # stand-in for the network call

# Pull just the fetch_google() definition out of the generator script
exprs <- parse("scripts/generate_nfl_news.R")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("fetch_google"))) eval(e)
}

out <- fetch_google("CeeDee Lamb")
json <- toJSON(out, auto_unbox = TRUE)
stopifnot(
  length(out) == 2,                                   # off-topic + preseason articles dropped
  out[[1]]$published == "2026-10-31",                 # real dates, newest first
  out[[1]]$impact == "negative",
  out[[1]]$category == "injury",
  grepl("ankle", out[[1]]$summary),
  out[[1]]$source == "ESPN",
  out[[1]]$title == "CeeDee Lamb ruled out vs. Cardinals with ankle injury",
  out[[2]]$published == "2026-10-30",
  out[[2]]$impact == "neutral",                       # "mouth" is not an injury anymore
  !grepl("\\{\\}", json)
)
cat(prettify(json), "\nfetch_google test passed\n")
