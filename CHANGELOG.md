# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

---

## [Unreleased]

### Added
- Comprehensive `README.md` with architecture diagram, data schema, local run instructions, and roadmap
- `LICENSE` (MIT)
- `CONTRIBUTING.md` with setup and workflow documentation
- `scripts/validate_data.R` — schema validation for all generated JSON files
- `scripts/install_packages.R` — single-command R dependency installer

### Changed
- `scripts/generate_weekly_stats.R`: output is now a flat JSON array (previously a numeric-keyed object); week numbers in filenames are zero-padded to two digits
- `.github/workflows/pages.yml`: removed stale placeholder comment

---

## [1.0.0] — 2025-09-04

### Added
- `scripts/generate_weekly_stats.R` — weekly player stats pipeline using `nflreadr`
- `scripts/generate_nfl_news.R` — automated NFL news feed via Google News RSS and Sleeper API
- `.github/workflows/update-stats.yml` — weekly automation (every Tuesday 06:00 UTC)
- `.github/workflows/update-news.yml` — news automation (every 6 hours)
- `.github/workflows/pages.yml` — GitHub Pages deployment on push to `main`
- Initial `data/` directory with 2025 season stats and news feed
