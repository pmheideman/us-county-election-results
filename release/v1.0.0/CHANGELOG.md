# Changelog

Release numbers follow the rule in `docs/DECISIONS.md` (section 8): a change in the columns or their meaning bumps the
middle number; new years, states or corrections bump the last. File names stay the same across releases.

## v1.0.0 (2026-09-26)

First public release.

**Coverage.** Candidate-level county results for regular general elections in the 48 contiguous states:

| Office | Years | County-years | Counties |
|---|---|---|---|
| U.S. House | 1990-2024 | 54,939 | 3,110 |
| President | 1992-2024 | 27,969 | 3,111 |
| U.S. Senate | 1990-2024 | 37,283 | 3,110 |

420,503 candidate rows from 1,523 documented sources: state election offices' reports, files and websites, MEDSL,
OpenElections and Algara & Amlani. 41 state-years are not fully covered (`us_county_results_gaps.csv`). Of these,
20 are House seats whose unopposed winner was never on the ballot, 8 are Senate years whose only race was a special
election, 7 are partial and 6 are House state-years with no county-level source found.

**Files.** `us_county_results_long.csv` (plus a new `.parquet` copy), `us_county_results_summary.csv`,
`us_county_results_gaps.csv`, `us_county_results_no_ballot.csv`, `SOURCES.csv`, `data_corrections_log.csv` (135 entries),
`DATA_DICTIONARY.md`, `CHANGELOG.md`, `VERSION`, `SHA256SUMS.txt`.

**Changes since the unpublished v0.2.0 build (2026-09-25)**
- Fixed: Yates County, NY was missing from the 2016 Senate results. MEDSL's 2016 files code it as FIPS 36122,
  which the release scripts had dropped as a non-county bucket. It is now recoded to 36123. The House was not affected.
- New: `us_county_results_long.parquet`, with the same rows and columns as the long CSV and `year`/`votes` stored as integers.
- License: the data license (CC BY 4.0) is final. `LICENSE-DATA.md` now says that the underlying state records keep
  their own terms and names the states whose site terms restrict reuse.
- New: `CITATION.cff`, this changelog, and SHA-256 checksums for the release files.

## v0.2.0 (2026-09-21 to 2026-09-25, not published)

Added President and Senate to the House files, plus `SOURCES.csv` and `us_county_results_no_ballot.csv`. Most House gaps
were filled from state election books, canvasses and archived results pages. Internal preview only.

## v0.1.0-house (2026-09-20, not published)

First House-only build: long, summary and gaps files. Internal preview only.
