# nfl-data

A fully automated NFL data pipeline that feeds a fantasy football application. It collects per-player weekly stats, defensive scoring, injury reports, and live news — then exposes them as clean JSON files via GitHub Pages so any front-end can consume them without a backend.

---

## Table of Contents
1. [What This Repo Does](#what-this-repo-does)
2. [Outputs](#outputs)
3. [Data Sources](#data-sources)
4. [Architecture](#architecture)
5. [Update Cadence](#update-cadence)
6. [Repo Structure](#repo-structure)
7. [Running Locally](#running-locally)
8. [Data Schema](#data-schema)
9. [Known Limitations](#known-limitations)
10. [Roadmap](#roadmap)

---

## What This Repo Does

Fantasy platforms show you stats, but they lock them behind paywalls or proprietary APIs. This pipeline pulls official NFL play-by-play data, computes fantasy scores (PPR, kicker, DST), overlays injury status, and streams relevant player news — all committed straight to this repo and served for free over GitHub Pages. The data updates automatically throughout the season with no manual intervention.

---

## Outputs

| File | Updated | Description |
|------|---------|-------------|
| `data/player_stats_{season}_week{NN}.json` | Every Tuesday UTC | Per-player stats for a single week (QB/RB/WR/TE/K/DEF). One file per week. |
| `data/news/news_{position}.json` | Every 6 hours | Recent articles per position group (`qb`, `rb`, `wr`, `te`, `k`, defense and OL groups). Only articles whose headline names the player; each has a category, impact and short summary. |

These files are deployed to **GitHub Pages** on every push to `main`, making them accessible via a public URL for any front-end.

---

## Data Sources

| Source | What it provides | Link |
|--------|-----------------|------|
| **nflreadr** (nflverse) | Weekly player stats, player metadata, injury reports, schedules, team stats | [nflverse.github.io/nflreadr](https://nflverse.github.io/nflreadr/) |
| **Sleeper API** | Active player roster for filtering news queries | [api.sleeper.app](https://api.sleeper.app/v1/players/nfl) |
| **Google News RSS** | Recent articles for each player/team | [news.google.com](https://news.google.com/) |

---

## Architecture

```
GitHub Actions (schedule)
        │
        ├── update-stats.yml  (every Tuesday 06:00 UTC)
        │       └─ scripts/generate_weekly_stats.R
        │               ├── nflreadr::load_player_stats()
        │               ├── nflreadr::load_players()
        │               ├── nflreadr::load_injuries()
        │               ├── nflreadr::load_schedules()
        │               └── nflreadr::load_team_stats()
        │               └─▶ data/player_stats_{year}_week{NN}.json
        │
        ├── update-news.yml   (every 6 hours)
        │       └─ scripts/generate_nfl_news.R
        │               ├── Sleeper API  (active players)
        │               └── Google News RSS (per-player queries)
        │               └─▶ data/news/news_{position}.json
        │
        └── pages.yml         (on push to main)
                └─▶ GitHub Pages (serves data/ as public JSON)
```

---

## Update Cadence

| Workflow | Schedule | Trigger |
|----------|----------|---------|
| Update NFL Weekly Stats | Every Tuesday at 06:00 UTC | `schedule` + `workflow_dispatch` |
| Update NFL News | Every 6 hours | `schedule` + `workflow_dispatch` |
| Deploy Pages | On every push to `main` | `push` |

Stats are committed back to the repo automatically. If there are no changes (bye weeks, pre-season), the commit step skips silently.

---

## Repo Structure

```
nfl-data/
├── scripts/
│   ├── generate_weekly_stats.R   # Pulls stats from nflreadr, writes per-week JSON
│   ├── generate_nfl_news.R       # Fetches player news via Google News RSS
│   ├── news_helpers.R            # Date parsing, relevance + headline classification for news
│   ├── tests/                    # Offline tests for the news helpers
│   ├── validate_data.R           # Schema checks for generated JSON files
│   └── install_packages.R        # Documents and installs all R dependencies
│
├── data/
│   ├── player_stats_2025_week01.json  # Week 1 player stats (example)
│   ├── ...
│   ├── player_stats_2025_week22.json  # Week 22 player stats
│   └── news/news_{position}.json      # Latest NFL news per position group
│
├── .github/
│   └── workflows/
│       ├── update-stats.yml   # Weekly stats automation
│       ├── update-news.yml    # News automation
│       └── pages.yml          # GitHub Pages deployment
│
├── CONTRIBUTING.md
├── CHANGELOG.md
├── LICENSE
└── README.md
```

---

## Running Locally

### Prerequisites

- **R ≥ 4.2.0** — [Download R](https://cran.r-project.org/)
- Internet access (nflreadr pulls from GitHub releases; news uses Google RSS)

### Install Dependencies

```r
source("scripts/install_packages.R")
```

This installs all required packages in one step. See `scripts/install_packages.R` for the full list with version notes.

### Generate Weekly Stats

```bash
Rscript scripts/generate_weekly_stats.R
```

Writes `data/player_stats_{year}_week{NN}.json` for every week available in the current season.

### Generate News Feed

```bash
Rscript scripts/generate_nfl_news.R
```

Writes `data/news/news_{position}.json` (up to 2 articles per player).

Test the news parsing/classification logic offline:

```bash
Rscript scripts/tests/test_news_helpers.R
Rscript scripts/tests/test_fetch_google.R
```

### Validate Output

```bash
Rscript scripts/validate_data.R               # latest season's stats + all news files
Rscript scripts/validate_data.R --season 2025 # one season
Rscript scripts/validate_data.R --all         # every season (~1 minute)
```

Checks the stats files under `data/Stats/` and the news files under `data/news/` against the expected schema and prints a pass/fail summary. It also warns when every article shares one date (a sign RSS dates failed to parse).

---

## Data Schema

### Player Stats (`player_stats_{year}_week{NN}.json`)

Top-level structure: a **JSON array** of player objects. One object per player per week.

#### Common fields (all positions)

| Field | Type | Example |
|-------|------|---------|
| `season` | integer | `2025` |
| `week` | integer | `10` |
| `player_id` | string | `"00-0036442"` |
| `player_name` | string | `"Patrick Mahomes"` |
| `position` | string | `"QB"` / `"RB"` / `"WR"` / `"TE"` / `"K"` / `"DEF"` |
| `team` | string \| null | `"KC"` |
| `opponent_team` | string | `"LV"` |
| `headshot_url` | string | `"https://static.www.nfl.com/..."` |
| `fantasy_points_ppr` | number | `34.5` |
| `injury_status` | string | `"ACTIVE"` / `"OUT"` / `"Q"` |
| `practice_status` | string | `"Full"` |
| `primary_injury` | string | `"Ankle"` |

#### Position-specific fields

<details>
<summary>QB</summary>

`completions`, `attempts`, `passing_yards`, `passing_tds`, `passing_interceptions`, `carries`, `rushing_yards`, `rushing_tds`, `fumbles`
</details>

<details>
<summary>RB / WR / TE</summary>

`carries`, `rushing_yards`, `rushing_tds`, `receptions`, `targets`, `receiving_yards`, `receiving_tds`, `fumbles`
</details>

<details>
<summary>K (Kicker)</summary>

`fg_made`, `fg_att`, `fg_missed`, `fg_pct`, `fg_made_0_19`, `fg_made_20_29`, `fg_made_30_39`, `fg_made_40_49`, `fg_made_50_59`, `fg_made_60_`, `pat_made`, `pat_att`, `pat_missed`, `pat_pct`
</details>

<details>
<summary>DEF (Team Defense)</summary>

`def_sacks`, `def_interceptions`, `def_fumbles_forced`, `fumble_recovery_opp`, `def_tds`, `def_safeties`, `passing_yards_allowed`, `passing_tds_allowed`, `rushing_yards_allowed`, `rushing_tds_allowed`
</details>

#### Fantasy Scoring Rules

| Position | Scoring |
|----------|---------|
| QB/RB/WR/TE | Standard PPR from nflreadr |
| K | FG 0–39 yds = 3 pts · FG 40–49 = 4 pts · FG 50+ = 5 pts · PAT = 1 pt · Miss = −1 pt |
| DEF | Sack = 1 · INT = 2 · Fumble forced = 1 · Fumble rec = 2 · TD = 6 · Safety = 2 |

---

### News Feed (`data/news/news_{position}.json`)

Top-level structure: a **JSON array** of article objects, newest first for each player.

| Field | Type | Values |
|-------|------|--------|
| `title` | string | Article headline (source name removed) |
| `source` | string | Publisher, e.g. `"ESPN"` (omitted if unknown) |
| `link` | string | Google News redirect URL |
| `published` | string | `YYYY-MM-DD` (omitted if the feed date can't be parsed) |
| `player` | string | Player the article is about (the headline names him) |
| `category` | string | `"injury"` / `"injury_return"` / `"transaction"` / `"suspension"` / `"performance"` / `"fantasy_advice"` / `"general"` |
| `impact` | string | `"negative"` / `"slightly_negative"` / `"roster_move"` / `"positive"` / `"neutral"` |
| `summary` | string | One-sentence overview generated from the headline, e.g. `"Saquon Barkley is dealing with an injury (ankle) that could keep him out. Check his status before lineups lock."` |

Impact and category come from whole-word keyword rules in `scripts/news_helpers.R`, applied only to the part of the headline that names the player.

---

## Known Limitations

- **News season window**: `generate_nfl_news.R` keeps articles from September 1 of the current NFL season onward (computed automatically).
- **News summaries**: Summaries are generated from the headline only (Google News links don't expose article text), so they describe what the headline says, not the full article.
- **Google News rate limiting**: The news script inserts a 1-second delay between queries to avoid throttling; large batches may still time out.
- **Sleeper API**: The player list is unversioned and may include stale entries; the script filters for `status == "Active"` to mitigate this.
- **Kicker stats**: The `fg_made_20_29` column in the source data is inconsistently named (`fgm_made_20_29` vs `fg_made_20_29`); the script handles this with a safe-column fallback.
- **Type coercion**: Fields in per-week JSON files are numbers (int/float), not strings.
- **Bye weeks**: Players on bye have all stats set to `0` and `injury_status` set to `"ACTIVE"`.

---

## Roadmap

- [ ] Add historical seasons (2011–2024) to the data archive
- [ ] Compute season-to-date aggregate stats per player
- [ ] Add a `POST /draft` endpoint or draft rankings export
- [ ] Replace Google News RSS with a dedicated NFL news API for richer metadata
- [ ] Add GitHub Actions status badges to this README
- [ ] Publish a simple HTML dashboard on GitHub Pages to visualize weekly leaders
