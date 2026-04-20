# scripts/install_packages.R
# Install all R packages required by this project.
# Usage: source("scripts/install_packages.R")  OR  Rscript scripts/install_packages.R

options(repos = "https://cloud.r-project.org")

# =====================
# SYSTEM DEPENDENCIES
# =====================
# Before running this script on Linux, install system libraries:
#   sudo apt-get install -y libcurl4-openssl-dev libssl-dev libxml2-dev

# =====================
# CRAN PACKAGES
# =====================
cran_packages <- c(
  "dplyr",      # Data manipulation (tidyverse core)
  "jsonlite",   # JSON read/write
  "readr",      # CSV / text file reading
  "lubridate",  # Date/time parsing
  "data.table", # Fast data operations
  "httr",       # HTTP requests (news fetcher)
  "stringr",    # String manipulation
  "xml2",       # XML / RSS parsing (Google News)
  "tibble"      # Tibble data frames
)

install.packages(cran_packages)

# =====================
# NFLVERSE PACKAGES
# =====================
# Try CRAN first, fall back to the nflverse r-universe mirror.
if (!requireNamespace("nflreadr", quietly = TRUE)) {
  install.packages(
    "nflreadr",
    repos = c("https://nflverse.r-universe.dev", "https://cloud.r-project.org")
  )
} else {
  message("nflreadr already installed: ", as.character(packageVersion("nflreadr")))
}

# =====================
# VERIFY
# =====================
all_pkgs <- c(cran_packages, "nflreadr")
missing  <- all_pkgs[!sapply(all_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing) == 0) {
  message("✅ All packages installed successfully.")
} else {
  stop("The following packages could not be installed: ", paste(missing, collapse = ", "))
}
