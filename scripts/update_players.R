library(httr)
library(jsonlite)

OUTPUT_FILE <- "data/sleeper_players.json"

message("Fetching Sleeper players...")

res <- GET("https://api.sleeper.app/v1/players/nfl")

if (status_code(res) != 200) {
  stop("❌ Failed to fetch Sleeper players")
}

players <- content(res, as = "text", encoding = "UTF-8")

if (!dir.exists("data")) dir.create("data")

writeLines(players, OUTPUT_FILE)

message("✅ Sleeper players saved to data/sleeper_players.json")
