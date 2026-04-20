# Contributing to nfl-data

Thank you for your interest in contributing! This document explains how the pipeline works and how to run it locally.

---

## Prerequisites

- **R ≥ 4.2.0** — [Download R](https://cran.r-project.org/)
- Git

## Setting Up

Clone the repository:

```bash
git clone https://github.com/NityaGehlot/nfl-data.git
cd nfl-data
```

Install all R dependencies:

```r
source("scripts/install_packages.R")
```

---

## Generating Data

### Weekly Player Stats

```bash
Rscript scripts/generate_weekly_stats.R
```

- Pulls from `nflreadr` (official nflverse package).
- Writes one JSON file per week to `data/player_stats_{year}_week{NN}.json`.
- Runs automatically every Tuesday at 06:00 UTC via `.github/workflows/update-stats.yml`.

### NFL News Feed

```bash
Rscript scripts/generate_nfl_news.R
```

- Queries Google News RSS for each active player.
- Writes `data/nfl_news.json` (up to 220 articles).
- Runs automatically every 6 hours via `.github/workflows/update-news.yml`.

### Validating Output

```bash
Rscript scripts/validate_data.R
```

- Checks that all generated JSON files conform to the expected schema.
- Prints a per-file pass/fail summary and exits non-zero if any file fails.

---

## Workflows

All automation lives in `.github/workflows/`:

| File | Purpose |
|------|---------|
| `update-stats.yml` | Runs `generate_weekly_stats.R` weekly and commits changes |
| `update-news.yml` | Runs `generate_nfl_news.R` every 6 hours and commits changes |
| `pages.yml` | Deploys `data/` to GitHub Pages on every push to `main` |

You can trigger any workflow manually from the **Actions** tab in GitHub using `workflow_dispatch`.

---

## Making Changes

1. Create a feature branch: `git checkout -b feature/your-change`
2. Make your changes and test locally (see above).
3. Run `Rscript scripts/validate_data.R` to confirm output is valid.
4. Open a pull request against `main`.

---

## Coding Conventions

- R scripts follow base-R + tidyverse conventions.
- JSON output is a flat top-level array of objects (no numeric-keyed wrapper objects).
- Week numbers in filenames are zero-padded to two digits: `week01`, `week10`.
- All numeric fields (season, week, stats) are written as numbers, not strings.
