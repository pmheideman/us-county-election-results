# Data dictionary: v0.2.0 (U.S. House, President and U.S. Senate)

Scope: U.S. House (1990-2024), U.S. President (1992-2024) and U.S. Senate (1990-2024), by county, 48 states (no Alaska, Hawaii or DC), regular general elections, final results. Special elections are excluded.
All offices share the same three tables; the `office` column is `house`, `president` or `senate`.

## us_county_results_long.csv (one row per year x county x district x candidate x party line)
| Column | Meaning |
|---|---|
| year | Election year |
| office | `house`, `president` or `senate` |
| state_fips, state, state_po | 2-digit state FIPS, state name, postal code |
| county_fips, county_name | 5-character county FIPS (leading zero kept) and name. Parishes in Louisiana; independent cities in Virginia are county-equivalents |
| district | House only: congressional district, 2 digits; `00` = at-large seat; blank if the source does not give one (a few years in SC 2008, IA 2010/2012, NY). Blank for President and Senate (statewide offices) |
| stage | `general`, or `runoff` for Louisiana and Texas district-years decided in a later round |
| candidate | Candidate name, title-cased, one spelling per district-year |
| party | Party line label as reported (e.g. Democratic, Republican, Working Families). A candidate on a fusion ticket has one row per line |
| party_group | `DEM`, `REP` or `OTHER` (the grouping used for all shares) |
| votes | Votes for that candidate on that line in the county |
| source | Where the row's numbers come from, as `<state>_<origin>_<year>` (e.g. `in_sos_report_1998`, `va_sos_web_2004`, `tx_openelections_2004`, `nc_medsl_2018`). `SOURCES.csv` describes every token: publisher, document, where to find it, how it was obtained and transcribed, license status and the related corrections-log entries |
| quality_flag | Semicolon-separated: `placeholder_name` (generic label because the source printed no name), `surname_only`, `initials_only` (first names given as initials only), `totals_possibly_inflated`, `other_candidates_aggregated` (President and Senate only: the source itemizes only the two major-party nominees, or MEDSL's own 'OTHER' row; all other candidates are one row named 'All other candidates (not itemized in the source)' or 'Other candidates') |

## us_county_results_summary.csv (one row per year x county)
County totals across all districts in the county: `n_districts`, `dem_votes`, `rep_votes`, `other_votes`, `total_votes`,
`dem_two_party_share` = D / (D + R), `rep_share_of_total` = R / all votes (the outcome used by Mayda et al.), `status` (`covered`),
`quality_flag` (adds `one_party_race` when no Democratic or no Republican votes, and `other_candidates_aggregated`). `n_districts` is blank for President and Senate. Counties split across House districts appear once per office here; their
per-district detail is in the long file.

## us_county_results_gaps.csv (one row per state-year that is not fully covered)
`counties_covered`, `counties_expected`, `coverage_pct`, `status` (`partial` or `none`), `gap_reason`: `unopposed_no_ballot` (known: Louisiana,
Oklahoma), `special_election_only` (Senate: the only race that year was a special election, excluded), `partial`, or `source_not_found` (no county-level source found yet; may include some unopposed seats), and a note. President and Senate rows are listed by `office`; for the Senate a state-year is expected when a race appears in any source, so a race missing from every source cannot be listed.

## SOURCES.csv (one row per `source` token)
Columns: `source`, `state_po`, `year`, `origin`, `publisher`, `document` (title of the publication or dataset), `locator` (URL, or the local file name in the project's data folder when the original URL was not recorded), `format`, `obtained` (downloaded / user-supplied), `transcription` (parsed text, OCR, or read by hand from page images, plus the build script), `license`, `license_status`, `n_rows`, `n_counties`, `n_districts`, `corrections_log_entries` (row numbers in `data_corrections_log.csv` that concern this state and year) and `notes`.

`origin` values:
| origin | Meaning |
|---|---|
| `medsl` | MIT Election Data and Science Lab precinct-level House returns (Harvard Dataverse), aggregated to county, with this project's corrections. Used for 2016-2024 where no better source was found |
| `openelections` | OpenElections (openelections.org) volunteer-compiled county files for a state, mostly 2000-2014 |
| `sos_report` | A report, canvass or statement of vote published by the state's election office (PDF, book or scan), read by text extraction, OCR or by hand |
| `sos_file` | A downloadable data file published by the state (spreadsheet, CSV, XML or text) |
| `sos_web` | A state election website or database (HTML tables, archive pages, a results app) |
| `secondary_web` | A public web compilation of official results (Wikipedia's by-county tables: Maine and Mississippi President 2004), used where no primary county source was found; the statewide sums are checked against the FEC certified totals |

`license_status`: `not_stated` = official public record with no terms on the pages used; `cc_by_sa_secondary` = a Wikipedia-compiled table (text CC BY-SA 4.0; the underlying vote counts are public facts); `unverified` = a license is believed to exist (MEDSL: CC0 1.0; OpenElections: check the repository) but it has not been checked. **No license has been verified yet**; do not assume the data may be redistributed under the release license until the `license` column is confirmed.

Candidate names for some rows were completed from Wikipedia district-results pages (Wikipedia text is CC BY-SA); the individual renames are listed in `candidate_name_overrides.csv` in the repository.

## Definitions and known limits
- `votes` are votes for candidates. Undervotes, overvotes, blanks and contest-total rows are excluded; special elections are excluded.
- Shares in the summary reproduce the project's shares panel exactly for every office (checked county-year by county-year). The President and Senate `total_votes` are sums of candidate votes; the earlier MEDSL totals that included over/undervotes (Arizona, Iowa, Vermont 2024) were corrected.
- President 1992-1996 and Senate 1990-2014 come from the Algara-Amlani historical file (CC0): only the nominees of the two major parties are named. From 2000 (President) and 2016 (Senate) every candidate is itemized (MEDSL, with its own 'Other' row for President).
- Miami-Dade County is FIPS 12086 in every year (the historical source's old code 12025 is recoded).
- Shares in the summary reproduce the project's shares panel exactly (checked county-year by county-year).
- Known problems are listed in `data_corrections_log.csv` (71 entries), including Kansas City MO 2016 (excluded non-county bucket, so Jackson/Clay/Platte are undercounted) and NJ Bergen 2024 (totals possibly inflated). Some rows still carry placeholder, surname-only or initials-only candidate names (see `quality_flag`). Candidate names were completed from Wikipedia district results; the method for each rename is in `candidate_name_overrides.csv` in the repository.
