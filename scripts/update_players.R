# =====================
# scripts/update_players.R (SAFE DAILY CACHE VERSION)
# =====================

library(httr)
library(jsonlite)

OUTPUT_FILE <- "data/sleeperAPI/sleeper_players.json"

# =====================
# ⏱️ SKIP IF UPDATED < 24 HOURS AGO
# =====================
if (file.exists(OUTPUT_FILE)) {

  file_info <- file.info(OUTPUT_FILE)
  last_modified <- file_info$mtime

  # Time difference in hours
  hours_since_update <- as.numeric(difftime(Sys.time(), last_modified, units = "hours"))

  if (!is.na(hours_since_update) && hours_since_update < 24) {
    message("⏭️ Skipping Sleeper update (last updated ", round(hours_since_update, 2), " hours ago)")
    quit(save = "no", status = 0)  # ✅ exits WITHOUT failing GitHub Action
  }
}

# =====================
# 📡 FETCH FROM SLEEPER
# =====================
message("Fetching Sleeper players...")

res <- tryCatch(
  GET("https://api.sleeper.app/v1/players/nfl"),
  error = function(e) NULL
)

if (is.null(res) || status_code(res) != 200) {
  stop("❌ Failed to fetch Sleeper players")
}

players <- content(res, as = "text", encoding = "UTF-8")

# =====================
# 💾 SAVE FILE
# =====================
if (!dir.exists("data")) dir.create("data")

writeLines(players, OUTPUT_FILE)

message("✅ Sleeper players saved to data/sleeperAPI/sleeper_players.json")
