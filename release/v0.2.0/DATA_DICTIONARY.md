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
`quality_flag` (adds `one_party_race` when no Democratic or no Republican votes, `other_candidates_aggregated`, and `excludes_unopposed_seat`: House only, part of the county lies in a district whose unopposed winner was not on the ballot or not tabulated, so the county's totals cover only its contested districts; see `us_county_results_no_ballot.csv`). `n_districts` is blank for President and Senate. Counties split across House districts appear once per office here; their
per-district detail is in the long file.

## us_county_results_gaps.csv (one row per state-year that is not fully covered)
`counties_covered`, `counties_expected`, `coverage_pct`, `status` (`partial` or `none`), `gap_reason`: `unopposed_no_ballot` (every county without returns lies in House seats whose unopposed winner was not on the ballot; the note names each district and winner), `special_election_only` (Senate: the only race that year was a special election, excluded), `partial`, or `source_not_found` (no county-level source found yet; may include some unopposed seats), and a note. A `partial` row can mix both: the note then counts the no-ballot counties and the counties genuinely missing data. President and Senate rows are listed by `office`; for the Senate a state-year is expected when a race appears in any source, so a race missing from every source cannot be listed.

## us_county_results_no_ballot.csv (one row per year x House district x county with no ballot)
House seats whose winner was unopposed and therefore has no county returns: Florida, Louisiana and Oklahoma do not print unopposed candidates on the ballot (Louisiana declares them elected, R.S. 18:511); Arkansas prints them but does not tabulate their votes. 84 seats, 1990-2024; no other state leaves unopposed House candidates off the ballot in the FEC or House Clerk records.
| Column | Meaning |
|---|---|
| year, office, state_fips, state, state_po, county_fips, county_name | As in the long file (`office` is always `house`) |
| district | The unopposed seat (2 digits) |
| candidate, party_group | The unopposed winner and `DEM`/`REP` |
| whole_county | `yes`: the county lies only in no-ballot seats that year, so it has no House row in the other files. `no`: the county is split and its other districts were contested; its summary row carries `excludes_unopposed_seat` |
| reason | `unopposed_no_ballot` |
| state_rule | The state law as the House Clerk's statistics state it |
| source | The record showing the seat was unopposed: FEC *Federal Elections* (1990, 2004-2022) or House Clerk *Statistics of the Congressional Election* (1992-2002, 2024) |
| county_method | How the district's counties were found: `same_map_year:<year>` = the counties reporting that district in the nearest election on the same district map; `boundary_overlay:...` = district boundaries overlaid on county boundaries (1990: UCLA historical district shapefiles, 102nd Congress; Louisiana 2022: Census TIGER 118th Congress), counting a county when at least 1% of its area is in the district |

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

`license_status` (checked 2026-09-23; see the `license` column for what each publisher's page says): `cc0_verified` = CC0 1.0 on the dataset's Dataverse record (MEDSL; Algara & Amlani); `mit_repo` = an OpenElections state repository with an MIT license (covers OpenElections' own work, not the underlying state data); `no_license_file` = an OpenElections repository with no license file (OpenElections says its data is free to anyone; the counts are public records); `not_stated` = official public record whose site or files state no reuse terms; `state_permissive` = the state's policy says content may be copied or distributed (NJ, RI, CA; RI requests credit); `state_conditional` = copying permitted only for non-commercial or personal use, unmodified, with citation (NC, UT); `state_restrictive` = site terms restrict copying, republication or derivative works (FL, IN, CO, WI); whether they reach factual vote counts, or the specific files used, is unresolved; `terms_unreachable` = the terms page could not be read (GA, MD, NH, AZ); `cc_by_sa_secondary` = a Wikipedia-compiled table (text CC BY-SA 4.0; the underlying vote counts are public facts). **Only MEDSL and Algara & Amlani carry an explicit open license.** State sources are not licensed under CC0 or CC BY by their publishers; do not assume the release license can override a state's terms.

Candidate names for some rows were completed from Wikipedia district-results pages (Wikipedia text is CC BY-SA); the individual renames are listed in `candidate_name_overrides.csv` in the repository.

## Definitions and known limits
- `votes` are votes for candidates. Undervotes, overvotes, blanks and contest-total rows are excluded; special elections are excluded.
- Shares in the summary reproduce the project's shares panel exactly for every office (checked county-year by county-year). The President and Senate `total_votes` are sums of candidate votes; the earlier MEDSL totals that included over/undervotes (Arizona, Iowa, Vermont 2024) were corrected.
- President 1992-1996 and Senate 1990-2014 come from the Algara-Amlani historical file (CC0): only the nominees of the two major parties are named. From 2000 (President) and 2016 (Senate) every candidate is itemized (MEDSL, with its own 'Other' row for President).
- Miami-Dade County is FIPS 12086 in every year (the historical source's old code 12025 is recoded).
- Shares in the summary reproduce the project's shares panel exactly (checked county-year by county-year).
- Known problems are listed in `data_corrections_log.csv` (71 entries), including Kansas City MO 2016 (excluded non-county bucket, so Jackson/Clay/Platte are undercounted) and NJ Bergen 2024 (totals possibly inflated). Some rows still carry placeholder, surname-only or initials-only candidate names (see `quality_flag`). Candidate names were completed from Wikipedia district results; the method for each rename is in `candidate_name_overrides.csv` in the repository.
