# U.S. county-level election results (President, House, Senate), 1990+

An open, documented county-level dataset of U.S. election results, with a Shiny map app. The main contribution is
county-level **U.S. House** results, which are otherwise available only in paid repositories or with known errors in the
open datasets (MEDSL, OpenElections). It also supports a replication of Mayda et al. (2022), *The Political Impact of
Immigration*.

**Start here:** [`docs/DECISIONS.md`](docs/DECISIONS.md) records scope, data model, map design, gap handling, data-quality
rules, sources and licensing, and what is still open.

- Code: `R/data_creation/` (one script per source), `R/build_provenance.R`, `R/house_coverage_tracker.R`.
- Data-quality records (tracked in this repo): `R/output/data_corrections_log.csv`, `R/output/elect_cty_final_provenance_summary.csv`,
  `house_results_coverage.csv`, plus per-source QA reports in `R/output/`.
- Release files (President, House, Senate long/summary/gaps CSVs, `SOURCES.csv`, `DATA_DICTIONARY.md`): built by
  `R/data_creation/03a_house_release.R` and `03d_release_all_offices.R` into `release/`, but the CSVs themselves are
  large and are **not** committed to this repo (see `.gitignore`) — they are published as GitHub Releases instead.
- Large intermediate `.rds` files (`R/output/**/*.rds`, including the working panel `elect_cty_final.rds`) and all raw
  source downloads (`R/data/`) are likewise excluded from git; they are reproducible by re-running the pipeline.
- Status: in progress. Current release is **v0.2.0** (President, House, Senate, 1990-2024); see section 11 of
  `docs/DECISIONS.md` and the coverage snapshot. The Shiny map app has not been built yet.
