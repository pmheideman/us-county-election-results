# U.S. county-level election results (President, House, Senate), 1990+

An open, documented county-level dataset of U.S. election results, with a Shiny map app. The main contribution is
county-level **U.S. House** results, which are otherwise available only in paid repositories or with known errors in the
open datasets (MEDSL, OpenElections). It also supports a replication of Mayda et al. (2022), *The Political Impact of
Immigration*.

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.22976553.svg)](https://doi.org/10.5281/zenodo.22976553)

**Interactive map:** <https://pmheideman.shinyapps.io/us-county-election-results/>

## Download

The data are published as [GitHub Releases](https://github.com/pmheideman/us-county-election-results/releases). The current
release is **v1.0.0**: one zip with every file, or the files one by one.

| File | What it holds |
|---|---|
| `us_county_results_long.csv` / `.parquet` | One row per year x office x county x district x candidate x party line, with votes, party group, source and quality flags |
| `us_county_results_summary.csv` | One row per year x office x county: Democratic, Republican, other and total votes, and shares |
| `us_county_results_gaps.csv` | Every state-year-office that is not fully covered, and why |
| `us_county_results_no_ballot.csv` | House seats whose unopposed winner was not on the ballot (FL, LA, OK, AR), by county |
| `SOURCES.csv` | Every source: publisher, document, where to find it, how it was transcribed, its license terms |
| `data_corrections_log.csv` | Every data error found in the sources and how it was handled |
| `DATA_DICTIONARY.md` | Column definitions, codes and known limits |

Scope: U.S. House 1990-2024, President 1992-2024, U.S. Senate 1990-2024; regular general elections; 48 contiguous states
(no Alaska, Hawaii or DC). Read FIPS codes and districts as text to keep leading zeros. In R:

```r
library(readr)
long <- read_csv("us_county_results_long.csv", col_types = cols(.default = col_character(), year = col_integer(), votes = col_integer()))
```

## Citation and license

Please cite the dataset:

> Heideman, Paul (2026). *U.S. county-level election results: President, U.S. House and U.S. Senate, 1990-2024* (v1.0.0)
> [Data set]. Zenodo. https://doi.org/10.5281/zenodo.22976553

The files are archived on [Zenodo](https://doi.org/10.5281/zenodo.22976553) as well as attached to the GitHub Release. Cite
the dataset (GitHub's "Cite this repository" button uses [`CITATION.cff`](CITATION.cff)) and, where relevant, the upstream
sources listed in `SOURCES.csv`. Data: [CC BY 4.0](LICENSE-DATA.md); the state records it is built from keep their own terms (see
`LICENSE-DATA.md`). Code: [MIT](LICENSE). Changes between releases: [`CHANGELOG.md`](CHANGELOG.md).

## Project documentation

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
- Status: current release is **v1.0.0** (President, House, Senate, 1990-2024); see section 11 of `docs/DECISIONS.md` and
  the coverage snapshot. The Shiny map app (`shiny_app/`) is live at
  <https://pmheideman.shinyapps.io/us-county-election-results/>.
