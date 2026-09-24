# Project decisions: U.S. county-level election results

Decision record for the county-level election data project and its map app. Each decision says **what was decided,
why, and what it implies**. Dates are when the decision was made; the owner of every decision is the project lead
(the user) unless marked *proposed*. Items marked **OPEN** are not decided yet.

Last updated: 2026-09-20.

---

## 0. Purpose and where the value is

- The project began as a replication of Mayda et al. (2022), *The Political Impact of Immigration: Evidence from the
  United States* (AEJ: Applied). Building that paper's dependent variable (county vote shares for President, House and
  Senate, 1990-2016, extended to 2024) turned out to be a very large data task.
- **Decision (2026-09-20): treat the election data as an independent project.** The Mayda replication is still wanted
  but becomes a downstream consumer of this dataset.
- **The real contribution is county-level U.S. House results.** County-level President and Senate results are already
  easy to obtain from open sources. County-level House results are hard to get: they are sold in paid repositories, or
  are incomplete or contain errors in the two open-access datasets (MEDSL and OpenElections), and we have found and
  documented a number of those errors (`R/output/data_corrections_log.csv`).
- Consequence for effort: House gets the collection and validation work. President and Senate are included so the app
  is complete, and are built from existing open sources with lighter checking (they are still validated, see section 7).

## 1. Deliverables

1. **A Shiny app.** The user chooses the office (President, House, Senate); the app shows a complete county map of
   results; hovering or clicking a county shows the results with candidate names and parties. UI details are flexible.
2. **The complete results as a CSV** (plus a data dictionary, provenance and the corrections log), so other researchers
   can use them. We already hold more county-level House data than either MEDSL or OpenElections.

## 2. Scope

| Item | Decision |
|---|---|
| Offices | President, House, Senate |
| Years | **1990 and later** for all three offices (v1) |
| Geography | 50 states minus **Alaska and Hawaii** (excluded for now). **DC is ignored** for now |
| Results shown | **Final results.** A `stage` field is kept only where we have the data (e.g. Louisiana runoffs), but the app and the default CSV view show the final result |
| Elections | **Regular general elections only. Special elections are ignored** (decided 2026-09-20). Implemented: MEDSL's `special` flag is excluded. Where a special race was the *only* race of the year (Arizona Senate 2020) the state is shown as a gap (`special_only`) |

Implications:

- 1990 is earlier than most open sources reach for House data, so v1 will ship with **documented gaps** (section 6).
- "Final result" means the deciding round. The rule already used for Louisiana is: drop closed party primaries, take the
  latest-dated general/runoff race per district. Maine's ranked-choice race is reported as first-choice votes because
  ranked-choice transfers have no county breakdown. The Louisiana 2016 and 2020 rows currently taken from MEDSL are
  first-round and need to be harmonized to the final round under this rule.

## 3. Data model

- **Source of truth: a candidate-level long table** (proposed fields, to be finalized with the data dictionary): year,
  office, state FIPS, county FIPS, district (House), stage, candidate, party (as reported), party group (D / R / other),
  votes, source, and a provenance/quality flag. Shares are *derived* from it.
- The existing shares panel (`R/output/elect_cty_final.rds`: `demovote`, `repuvote`, `totalvote`) becomes a derived view,
  which is what the Mayda replication reads.
- Why: the app needs candidate names, parties and votes, and today the panel stores only shares. Only four builds
  (Louisiana, Oklahoma x2, South Dakota) kept candidate-level tables; the other 74 per-state House builds saved shares
  only and will need small edits to save their long tables. Candidate-level data does exist for President 2000-2024
  (`countypres_2000-2024.tab`) and for MEDSL House/Senate 2016-2024. Historical President (1868+) and Senate (1908+) rows
  are shares only.
- Where only shares exist (mostly pre-2000 President and pre-2016 Senate), the app will show the major-party nominees'
  names from a small lookup and mark the row as "shares only", not as full candidate detail.
- `totalvote` = votes for candidates. Non-candidate rows (undervotes, overvotes, blanks, "contest total") are not votes and
  are excluded. **Done 2026-09-20** for all MEDSL House/Senate rows (3,081 rows changed in 99 state-years; the same step
  removed special elections). MEDSL House/Senate totals are now 0.97-0.99 of the presidential total in the median.

## 4. Map and display

- **Color: two-party share.** Diverging scale (Democratic to Republican) on D / (D + R). Note that Mayda et al.'s
  outcome is different (Republican votes divided by *all* votes); the long table supports both definitions.
- **Hover / click:** county name, winner, two-party share, and the candidate results with names, parties and votes.
- **Counties with more than one House district in the year in question** (split counties): show **every district's
  candidates, parties and vote totals**, plus a **cross-district county total**. Candidates differ across districts, so
  the cross-district total is by party group (Democratic, Republican, other) and total votes, not by candidate. The county
  is colored by this cross-district two-party share.
- **One-party races** (no Democrat or no Republican on the ballot) are colored normally, with the two-party share at the
  extreme, and labeled (e.g. "no Democratic candidate"). They are not gaps.

## 5. Geography

- One county boundary set plus a crosswalk for known changes, with exceptions documented: Shannon County to Oglala Lakota
  (SD, FIPS 46113 to 46102, 2015); Connecticut (towns / planning regions); Virginia independent cities; Kansas City, MO;
  Miami-Dade (12025 to 12086); New England towns. Alaska and Hawaii are out of scope for now.

## 6. Gaps

- **Plan around documented gaps.** We will not chase 100% coverage; we will make every gap visible and explained.
- **Gray fill with a reason**, not a blank:

| Reason code | Meaning |
|---|---|
| `unopposed_no_ballot` | The seat had no election because the candidate was unopposed (e.g. Louisiana, Oklahoma); no votes exist anywhere |
| `no_election` | The office was not on the ballot (e.g. no Senate race in that state that cycle) |
| `special_only` | The only race that year was a special election, which v1 ignores (e.g. Arizona Senate 2020) |
| `source_not_found` | An election was held but we have no county-level source yet |
| `partial` | Some districts or counties are missing for that year |
| `excluded` | Outside v1 scope (Alaska, Hawaii) |

- The coverage tracker and the provenance completeness check (`R/build_provenance.R`) become the source for these labels.

**Snapshot of House coverage at time of writing** (scope 1990+, 48 states, 864 state-years; from
`house_results_coverage.csv`): 553 full, 47 partial, 264 with no data yet. Coverage by era: only 15-21% of states are
fully covered in 1990-1998, 56-69% in 2000-2006, 75-83% in 2008-2012, and 92-98% in 2014-2024. Fully covered in every
year: Arizona, California, Kentucky, Maine, Maryland. Largest remaining gaps (non-full years out of 18): Indiana 14;
Kansas, Minnesota, Vermont, South Carolina and Utah 11 each; Rhode Island, Arkansas and New York 10 each. Some "none"
years will turn out to be `unopposed_no_ballot` rather than missing sources. (The tracker counts a state-year as full at
98% of counties.)

## 7. Data quality

- Every source is validated before use: county sums against printed totals where a source prints them; candidate shares
  against Wikipedia or certified results; House-to-presidential turnout ratios; and the provenance and completeness
  checks. Corrections are recorded in `R/output/data_corrections_log.csv`
  (`R/write_corrections_log.R`), and every panel row's origin in `R/output/elect_cty_final_provenance.rds`
  (`R/build_provenance.R`).
- Corrections should move from hand patches on the panel to rules applied when the long table is built, so results are
  reproducible from raw sources.

## 8. Release format, hosting and license (decided 2026-09-20)

**Hosting: GitHub.** Code, documentation and small quality files (corrections log, provenance summary, coverage tracker) live in
the repository. The large data files are published as **GitHub Releases** (up to 2 GB per file), not committed. Raw downloads
(`R/data/`, about 3.6 GB) and the Mayda replication package are not in the repository (see `.gitignore`).

**License (provisional):** data **CC BY 4.0**, code **MIT** (`LICENSE`, `LICENSE-DATA.md`). Why: our main upstream sources are
CC0 (MEDSL, Algara & Amlani) or public records, so we are free to choose. CC BY 4.0 allows any reuse, including commercial and
redistribution, and only requires credit, which gives the project citations and covers attribution duties from any upstream that
needs it. CC0 would be equally lawful and is the maximally open alternative. **Provisional** until the source terms marked
"confirm" in section 9 are checked; the Alabama 1990-2012 rows come from a file the Alabama Secretary of State hosts (see section 9), so no separate
compiler permission is needed beyond confirming the SOS site's terms. The copyright holder line in the license files is a placeholder.

**File formats and versioning (defaults, change on request).** The question is: what are the columns of the released CSV, what are
the files called, and how do we number releases so other people's code does not break?

- `us_county_results_long.csv` (and `.parquet`): one row per year x office x county x district x candidate. Columns:
  `year`, `office` (president / house / senate), `state_fips`, `county_fips` (5 characters, leading zero kept), `county_name`,
  `district` (House; blank otherwise), `stage` (default `general`; runoff or first-round only where we have it),
  `candidate`, `party` (as reported), `party_group` (DEM / REP / OTHER), `votes`, `detail_level` (`candidate` or
  `party_shares_only`), `source`, `quality_flag`.
- `us_county_results_summary.csv`: one row per year x office x county with `dem_votes`, `rep_votes`, `other_votes`,
  `total_votes`, `dem_two_party_share`, `rep_share_of_total`, `n_districts`, `status` and `gap_reason`.
- `us_county_results_gaps.csv`: every county-year-office we do not cover, with its reason code.
- `DATA_DICTIONARY.md`, `data_corrections_log.csv`, `SOURCES.csv`, `CHANGELOG.md` ship with every release.
- File names stay the same across releases; the release number goes in a `VERSION` file and the GitHub tag: `v0.1.0`, `v0.2.0`, ...
  (a change in columns or meaning bumps the middle number; new years or states or fixes bump the last).

## 8b. Still open

1. Order of work (decided 2026-09-20): **House first**, then Senate and President. Status: the House long table is complete (see section 11).
2. Confirm the source terms flagged in section 9, and fill in the copyright holder.
3. Data-quality issues still flagged in `data_corrections_log.csv`: MEDSL 2016 totals inflated in AL, SC, AR and IN; Maine 2024
   totals doubled; New Jersey Bergen County 2024 total; Indiana 2018-2022 coverage; Indiana and Alabama pre-2016 gaps.

## 9. Sources and licensing

The project lead's understanding is that everything we hold is open, because Mayda et al. did not share their
proprietary data and we did not use it. The check below agrees for most sources, with a few to confirm before release.
**We do not use CQ Press or Leip's Atlas data directly.**

| Source | Used for | License / status |
|---|---|---|
| MEDSL (MIT Election Data and Science Lab) | President 2000-2024 (`countypres`), House/Senate precinct files 2016-2024 | Open (MEDSL publishes on Harvard Dataverse, CC0). Confirm per file before release |
| Algara & Amlani, Harvard Dataverse doi:10.7910/DVN/DGUMFI | Historical President 1868-2020, Senate 1908-2020 | CC0 1.0 as deposited. It is compiled from CQ Press / ICPSR historical returns, so the upstream provenance should be cited |
| OpenElections | Many pre-2016 state House builds | Volunteer project, repos per state. **Confirm each repo's license** and attribution |
| State election boards / secretaries of state (e.g. NC, VA, FL, KY, LA, OK, SD, ME, NH, AZ, CA, MD, UT) | Direct county results | Public records. **Confirm each site's terms of use** |
| Alabama Secretary of State, Election Data page (`eaushouse1980-2012_0.xls`, Auburn-compiled archive file hosted at sos.alabama.gov/alabama-votes/voter/election-data) | Alabama House 1980-2012 | Public record hosted by the state; no terms on the page. **Confirm the site's terms of use** |
| Wikipedia | Party lookups (KY, ME, NJ, OR) and cross-checks | Facts only, used for verification; attribute (CC BY-SA) and keep out of the redistributed values where possible |

**License check (2026-09-23).** Every registry row was checked against the publisher's terms (details in `R/data/source_registry.csv`, `license` column; carried into `SOURCES.csv`). Verified CC0 1.0 on Dataverse: MEDSL (House and Senate precinct files 2016-2024, county President 2000-2024) and Algara & Amlani. OpenElections: MIT in the CA, IN, NV and OR repos; no license file in the other 37 (org page says only 'The data is and will remain free to anyone'). State sites: none grants CC0/CC BY. Permissive statements: NJ, RI (credit requested), CA (state-wide policy; not confirmed for the SOS site). Conditional (non-commercial/unmodified): NC, UT. Restrictive site terms: FL, IN, CO (also bans robots/data mining; our pull used its public API), WI (Blue Book compilation copyright). Terms pages unreadable (HTTP 403): GA, MD, NH, AZ. Others state no reuse terms. The only commercial-publisher source (CQ Press America Votes 33, 9 New York 2018 Senate rows) was replaced on 2026-09-23 by the New York State Board of Elections' Elections Database (results.elections.ny.gov, an Elstats platform that returns HTTP 403 to scripted clients, so the county CSV was fetched from a browser session); the official numbers are identical for all 62 counties. The database's About page states no terms. Open question for the release license: vote counts are factual public records, but the restrictive/conditional/unreachable states may warrant a written permission request or a reliance-on-facts note before publishing under CC BY 4.0.

## 11. Status: House (2026-09-20)

- **The candidate-level House long table is built** for all 70 sources in the shares panel: 143,974 rows, 37,791 county-years (1990+, 48 states), from
  `R/output/long/he_<x>.rds` (one per source) assembled by `R/data_creation/02z_house_long_assemble.R`. **Master check: shares derived from the long
  table reproduce the panel exactly for all 37,792 county-years.** Every source passed the same exact-match test against its own shares file first.
- The test found real errors in the old data, now fixed: New York Kings County 2004 (a candidate on both major-party lines counted in both numerators)
  and California 2000 District 16 (Santa Clara, a mis-parsed block). Both are in `data_corrections_log.csv`.
- **First release files (v0.1.0-house)** in `release/v0.1.0-house/`: `us_county_results_long.csv` (143,952 rows), `us_county_results_summary.csv`
  (37,787 county-years, cross-district totals), `us_county_results_gaps.csv` (311 state-years: 264 `source_not_found`, 36 `partial`, 11 `unopposed_no_ballot`),
  `DATA_DICTIONARY.md`, `VERSION`, and a copy of the corrections log.
- Known limits carried into the release as `quality_flag`: a few remaining placeholder or surname-only names; totals possibly inflated (NJ Bergen 2024 only); Kansas City, MO 2016 votes sit in a non-county bucket that
  is excluded (Jackson, Clay and Platte counties undercounted).
- **Candidate names (done 2026-09-20):** full names filled from Wikipedia district results and then cleaned with extra rules (accents, misspellings, party-word rows, unopposed seats);
  every rename is recorded in `R/data/raw_election/candidate_name_overrides.csv` (1,292). 99.2% of candidates with at least 1% of their district vote are named; only 228 of
  143,477 release rows (0.16%) still carry a name flag.
- **Arkansas (2026-09-20):** 1990-2002 and 2006 built from the official Secretary of State books and folded in (558 county-years); every district ties to the printed totals. Unopposed seats
  (1998 AR-1, 2000 AR-3) are gaps with reason `unopposed_no_ballot`. Still missing: 2004 (no source), and partial 2008/2012/2014/2022.
- **Texas (2026-09-20):** the SOS archive (`elections.sos.state.tx.us`, 1992-current) has one static county table per district. 1992-1998 and 2006 added, 2014 (El Paso/Ellis swap), 2016 and 2018 (MEDSL totals) replaced by the official
  numbers; districts redrawn by court order (1996, 2006) voted on a November special-election ballot with a December runoff and are included under the decisive-round rule. Texas gap left: 1990 (site starts 1992).
- **Virginia (2026-09-20):** the Department of Elections' Historical Elections Database (`historical.elections.virginia.gov`) serves a Results CSV per contest, with a row per county/independent city. It is scripted through the site's own public GraphQL search
  (contest ids) and CSV download endpoint. All 18 elections 1990-2024 are built (133-136 localities each) and replace the earlier Virginia rows; this fixed county/city name collisions in the panel (Bedford City, Richmond, Franklin) and 2014 double counts. No Virginia gaps remain. Candidate names in the 1990s-2000s are initials only (flag `initials_only`).
- **Kansas (2026-09-21):** all 11 elections 1990-2010 built from the Secretary of State's scanned election-statistics books (OCR text checked against the printed district totals and, where the tie broke, against the page images); no Kansas gaps remain.
- **Minnesota (2026-09-21):** all 11 elections 1990-2010 from the Secretary of State books ('Vote for U.S. Representative by county'); DFL = Democratic, IR/R = Republican. 1990-1996 books omit write-ins (shares overstated by about 0.1-0.5 pt); one inferred value (Sherburne 2006).
- **South Carolina (2026-09-21):** 2008 completed (46 of 46 counties) from the State Election Commission's ENR archive. That site has no public list of elections, only this one snapshot was reachable, and it later put up an automated-access (CAPTCHA) check, so it was not scanned further. 1990-2006 and 2010 are covered by the SC Election Report PDFs still to be processed.
- **South Carolina (2026-09-21, second part):** 1990-2006 built from the SC Election Commission reports (scanned printouts, transcribed from images or cleaned OCR, all tied to the printed STATE TOTAL rows); only 2010 remains a gap (no report in hand). 2002 district 1 differs from Wikipedia (flagged).
- **Indiana (2026-09-21):** 1990-2000 (image-only Secretary of State reports), 2002 (native-text PDF) and 2010 (scanned booklet) built and verified against the printed totals; they replace the OpenElections rows for 2002 and 2010 (which had 26 zero-vote counties and partial split counties). 2004-2008 still rest on the flawed OpenElections rows; 2018-2022 are partial (MEDSL).
- **Source tokens and SOURCES.csv (2026-09-21):** the long file's `source` column is now `<state>_<origin>_<year>` (e.g. `in_sos_report_1998`, `tx_sos_web_1996`, `nc_medsl_2018`), with origins medsl / openelections / sos_report / sos_file / sos_web (an earlier `academic_xls` origin, used for the Alabama Auburn workbook, was retired on 2026-09-23 when that file was confirmed as hosted on the Alabama Secretary of State site and reclassified `sos_file`). `release/.../SOURCES.csv` has one row per token (publisher, document, locator, format, how obtained and transcribed, license, license status, related corrections-log entries). Built from `R/data/source_registry.csv` (hand-maintained; the release script stops if a source has no registry entry) by `R/source_tokens.R`. **No license has been verified**: MEDSL is believed CC0, OpenElections and the Auburn Alabama workbook need checking, state sources state no terms. Also: when several sources hold an identical row, the per-year official build now wins over the state-wide OpenElections file in `build_provenance.R`.
- **Indiana 2016-2024 (2026-09-21):** the Election Division's ENR archive (`enr.indianavoters.in.gov/archive/<year>General/`, static JSON) replaces MEDSL for Indiana House (MEDSL had doubled counties, missing districts and partial coverage). Archives for 2008-2014 do not exist there, so Indiana 2004-2008 remain the only Indiana gaps.
- **Illinois (2026-09-21):** 1998-2024 from the State Board of Elections' downloadable county vote totals (scripted through the page's own year dropdown); replaces MEDSL for 2016-2024 (up to 4% differences; 2022 Cook and DuPage badly short). 1990-1996 come from the scanned official-vote books (forks). **Indiana 2004-2008** built from the Election Division report scans; Indiana now has no gaps 1990-2024.
- **Nebraska (2026-09-21):** 1990-2006 from the Secretary of State canvass books (scans read from images; 2004 native text). Lesson recorded in the corrections log: county-sum ties cannot catch two offsetting misreads; the county-level ratio to the presidential/Senate total did.
- **Georgia (2026-09-21):** 1990-1998 from the Secretary of State's archived results pages (Wayback Machine, July 2008 captures; one page per district with county votes). Georgia now has no gaps 1990-2024 except what MEDSL/OpenElections leave in 2012-2016.
- **New York House 1996-2018 (2026-09-23):** all 12 even years from the State Board of Elections' Elections Database (`results.elections.ny.gov`, an Elstats platform; 403 to scripted clients, so the 346 district tables were fetched from a browser session; `01ep_house_county_new_york_boe.R`, `01eq_new_york_boe_apply.R`). Fills 1996, 1998, 2002, 2008, 2010 and replaces the OpenElections rows (2000-2014) and MEDSL rows (2016-2018), which had missing counties (2012 58/62, 2016 61/62, 2018 47/62) and errors (2000 Bronx/Queens, 2004 Kings). County slices tie to the printed district row in every column of every contest once a duplicate "Otsego" row (a copy of Oswego) in 2010 CD24 and 2012-2018 CD22/CD24 is dropped. Independent checks: FEC official district results 2004-2018 (Democratic and Republican votes match to <0.05% in every district-year except the known FEC 2006 CD28 total; district totals differ only by scattering, which we exclude) and America Votes district totals 1996-2010 (exact except three America Votes typos). Blank/void/scattering are not counted. Database starts in 1996, so 1990-1994 stay gaps; 1996 has no Oswego rows (61/62). The old `elect_he_cty_ny.rds` is now `SUPERSEDED_elect_he_cty_ny.rds`.
- **Connecticut House 1990-2014 (2026-09-23):** all 13 even years from the Secretary of the State's Election History database (`electionhistory.ct.gov`, an Elstats platform that answers curl; `01er_connecticut_download.R`, `01es_house_county_connecticut_elstats.R`, `01et_connecticut_elstats_apply.R`). Town rows are summed to the 8 counties with the existing town-county crosswalk; party classification is the literal ballot line (no fusion consolidation), as in the earlier build and MEDSL. Fills 1990-1998, 2004 and 2014 and replaces the OpenElections rows of 2000-2012 (2012 had 3 of 8 counties). 1998 has only 5 of 8 counties (CD1 was uncontested, so 19 towns have no House rows). Source defects handled and logged: two wrong or missing party labels (1998 CD3 Cole, 2002 CD2 Courtney; fixed from FEC results), an inconsistent district-row cell (2008 CD3 Itshaky), a 200-vote town-total discrepancy (1994 CD5 Woodbridge), write-ins printed only on district rows. Checks: town rows tie to the district rows in all 80 contests apart from those; Democratic, Republican and total votes match the FEC in every district-year 2004-2014 except 2006 CD4 (Democratic 1.3% above the FEC, same as the old build). 2016-2018 are unchanged MEDSL (identical to the database to 4 decimals). The old `elect_he_cty_ct.rds` is now `SUPERSEDED_elect_he_cty_ct.rds`.
- **Washington House 2008 and 2010 (2026-09-23):** from the Secretary of State's 2008 and 2010 General Election data downloads (`01eu_house_county_washington_sos.R`, `01ew_washington_2010_sos_precinct.R`, `01ev_washington_sos_apply.R`). 2008 was a gap (workbook with county x district x candidate; county sums equal the printed district totals). 2010 had 37 of 39 counties and errors from the OpenElections precinct file (two misspelled counties dropped; King/Skamania/Snohomish candidate swaps; Pierce suppressed cells); rebuilt from the 39 county precinct files (five layouts) and tested against the FEC: Democratic and Republican district totals equal the FEC's in all 9 districts. The archive page named in the request (`washington-state-election-results-archive`) only offers a statewide/district-level workbook (1898-2006), no counties; pre-2007 county results must come from the county elections offices, so WA 1990-1998 remain gaps. The SOS site states it is public domain ("You may use, share or copy such information"; credit requested) so both years are `state_permissive`.
- **Massachusetts House 1990-2014 (2026-09-23):** all 13 even years from the Secretary of the Commonwealth's Elections Statistics site (`electionstats.state.ma.us`, the older PD43+ platform; plain curl works; `01ex_massachusetts_download.R`, `01ey_house_county_massachusetts_electionstats.R`, `01ez_massachusetts_electionstats_apply.R`). Town tables summed to the 14 counties; town rows tie to each table's TOTALS row in all 129 contests after two documented repairs (2000 CD4 missing Travis column, reconstructed from the row totals and confirmed by the FEC; 1996 CD10 Sandwich Teague cell). Fills 1990-1998 (1996: 13 of 14 counties, Sutton has no rows) and replaces the OpenElections rows of 2000-2014. **Terms caution:** the Secretary's Terms and Conditions (sec.state.ma.us/divisions/terms.htm) prohibit "scraping or crawling, whether by automated means or manually" and creating derivative works; the 129 tables were fetched with a script before those terms were read, so the source is registered `state_restrictive`. **Decision (project lead, 2026-09-23): the Massachusetts rows stay in the release** -- the vote counts are public facts -- with the terms language kept visible in `SOURCES.csv`; permission has not been requested (an email or public-records request to the Elections Division would settle it if that changes). The build is easy to reverse: the panel rows come from `elect_he_cty_maelst_<year>.rds`, and the old OpenElections file is kept as `SUPERSEDED_elect_he_cty_ma.rds`.
- **Ohio House 1996 and 2002 (2026-09-23):** from the Ohio Secretary of State's own results pages as archived by the Wayback Machine (October 2004 captures of `www.sos.state.oh.us/sos/results/`; three pages fetched with curl: 1996 `90/1996/gen/UShouse.htm`, 2000 `2000/gen/us_house_of_representatives_00.htm`, 2002 `2002/gen/2USreps-dist.htm`; `01fa_ohio_sos_archive_parse.py`, `01fb_house_county_ohio_sos_archive.R`, `01fc_ohio_sos_archive_apply.R`). The pages give every candidate's votes in every county of every district; county rows tie to the printed district totals. Fills 1996 (88 counties; district totals equal America Votes) and corrects 2002 (Cuyahoga was missing two districts' worth of votes in the OpenElections rows); 2000 is identical to the archive. The archive holds only statewide summaries for 1990, 1992, 1994 and 1998, and the 2004 House data frame is not archived (the SOS site is a different source for 2004 and 2010).
- **Nevada House 1990-1998 (2026-09-23):** from the Secretary of State's official general-election abstracts linked from `nvsos.gov/elections/election-information/previous-elections/election-results` (five PDFs downloaded through Chrome: the site is behind Imperva bot protection; scanned 1990/1992/1996, noisy OCR layer 1994, text 1998; `01fd_nevada_1990_1998_transcribe.py`, `01fe_house_county_nevada_1990_1998.R`, `01ff_nevada_apply.R`). 17 county columns per table; district 1 is Clark only. Transcribed from the page images; every row ties to its printed total and the statewide candidate totals equal the FEC's (1992 needed one FEC-assisted digit correction because the fax-quality scan confuses 5 and 6). The site's privacy policy says its information is public and "may be freely distributed or copied" (credit requested), so `state_permissive`; its business-services terms prohibit automated/systematic collection, so only these five documents were fetched, one at a time. Nevada now has no House gaps.
- **Missouri House 1990-1998 (2026-09-23):** from the Official Manual of the State of Missouri (Blue Book) general-election pages (Missouri Digital Heritage collection, public domain), 14 page images saved by the project lead in `R/data/county_house_files/missouri/` and read by eye into `transcribed/mo_*.txt` (`01fg_missouri_1990_1998_parse.py`, `01fh_house_county_missouri_1990_1998.R`, `01fi_missouri_apply.R`). Each table gives one district's candidates by county (or part of a county); all 45 district tables tie to their printed TOTALS in every column except the Republican column of 1992 District 8 (270 votes short of the printed and FEC total, left as printed). Split counties are summed; Kansas City rows belong to Jackson County; all 115 counties are present every year. 1990 lists only the two major-party candidates. Missouri now has no House gaps.
- **Oklahoma House 1998 (2026-09-23):** from the State Election Board's results page `98gencon.html` as archived by the Wayback Machine (`01fj_oklahoma_1998_parse.py`, `01fk_house_county_oklahoma_1998.R`, `01fl_oklahoma_1998_apply.R`); 77 counties, six district tables that tie to their STATE TOTAL lines and to the FEC. The same archive has no county House results for 1996 (President and Senator only) or 1990-1994, and the 1999-2000 Oklahoma Almanac (also checked) has only statewide and district totals, so Oklahoma 1990, 1992 and 1996 remain gaps (plus the unopposed years).
- **President and Senate release (2026-09-21):** `release/v0.2.0/` = House + President (1992-2024) + Senate (1990-2024) in the same three tables. Candidate-level long tables built (`02b/02c/02d`), assembled per the panel's provenance (`02zb`, master check exact: President 27,975 and Senate 37,227 county-years) and released by `03d_release_all_offices.R`. Historical President/Senate rows are shares-only (major-party nominees plus one 'All other candidates' row). Fixes made on the way: 8 states of the 2024 President (367 county-years) had been silently dropped (blank MEDSL vote mode), 127 county-years of AZ/IA/VT 2024 had over/undervotes in their totals, 515 special-election-only Senate county-years removed, Indiana Senate replaced by the ENR archive. `SOURCES.csv` now has an `office` column.
- **MEDSL QA sweep (2026-09-21):** `R/qa_medsl_sweep.R` compares every MEDSL-sourced House/Senate county total with the county's presidential total (T1), House vs Senate in the same county (T2) and checks Democratic/Republican share arithmetic (T3); output `R/output/qa_medsl_sweep_flags.csv`. Class-level defects found and fixed: 'Public Counter'/'Federal Votes' pseudo-rows (New York), blank party fields (GA/KY/ND 2016 and others), Georgia 2022 Senate general + runoff mixed (decisive round rule now in 01a). Independent check used: America Votes 33 (NY Senate 2018). New Jersey 2022 House (district totals repeated in every county) was replaced by the Division of Elections' official PDF (`02aa_house_nj_2022.R`, `01dj_nj_2022_apply.R`; 53 candidates and 12 district totals tie exactly). Still open: about 50 flagged county-years (see the corrections log); re-run the sweep after every MEDSL-touching change.
- **Data errors found and fixed on the way (all in `data_corrections_log.csv`):** MEDSL counted other offices (straight-party lines, Arkansas ballot rows) and more non-candidate lines
  ("Total Ballots Cast" in Maine 2024, New York "Affidavit"/"Absentee / Military"/"Over", blanks) as votes; our California hand transcriptions had typos (1992, 1996); and the
  old California 1998/2000 parse counted Reform-party columns as Republican (rebuilt, ties to every printed district total). Wikipedia is a cross-check only: it has typos too.
- Next for House: close the largest coverage gaps (mostly 1990-1998), resolve the last 260 unnamed candidates, and confirm source licenses.

## 10. Related files

- `R/output/data_corrections_log.csv`: every data error found and how it was handled.
- `R/output/elect_cty_final_provenance.rds` and `_summary.csv`; `elect_cty_final_missing_from_panel.csv`.
- `house_results_coverage.csv` and `R/house_coverage_tracker.R`: coverage by state and year.
- `R/data_creation/`: one script per source, with the reasoning in its header comments.

- **Independent state-level reconciliation, President (2026-09-21):** `R/qa_state_reconcile_pe.R` sums the release's presidential county rows by state and compares them with official state results: FEC "Federal Elections" workbooks for 2004-2020 (`R/data/fec_official/`), state totals only from FEC 2000 tables plus the two nominees parsed from the OCR'd 2000 book (`R/data_creation/01dk_fec_2000_pres_parse.py`), and, because the FEC has not published 2024, Wikipedia's certified-totals table (secondary source, marked as such). Result over 2000-2024 (48 states, 336 state-years): 658 nominee checks, 600 within 0.05%, 648 within 0.5%; state totals within 0.5% in 316 of 336. Real defects found: Maine 2004 (-7.3%), Mississippi 2004 (-3.2%, Oktibbeha), New York 2008/2020 (small), Maine 2020/2024 (-1%), three round-number single-digit errors (MD 2000, NJ 2004, WI 2012). Minor candidates cannot be compared (MEDSL lumps most into "Other"). 'Other' votes (write-ins/scattering) are off by 1-3% in about 20 state-years while the nominees match. Details: `data_corrections_log.csv` (six new flagged entries) and `R/output/qa_state_reconcile_pe_decomposition.csv`. Not yet done: 1992/1996 (FEC books exist), Senate and House (FEC has state and district results in the same workbooks).

- **House and Senate reconciliation vs the FEC (2026-09-21):** `R/qa_state_reconcile_congress.R` compares our House (per district) and Senate (per state) rows with the FEC's official congressional results, 2004-2022 (`R/data/fec_official/`; 2000 has no candidate table and the FEC has no 2024 book, so those years are not covered). It compares candidate-level Democratic and Republican totals (party lines collapsed, so fusion states work) and the printed district/state totals. House: 3,922 districts compared, 3,634 match on D, R and total within 0.5% (3,599 match D and R within 0.05%), 116 match D and R but not the total, 172 have a nominee off by 0.5% or more (134 by 2% or more). 244 official districts have no rows in ours (New York, Ohio, Michigan, Connecticut ...: the known gaps). Senate: 319 state-years, 284 match within 0.5%, 31 have a nominee off. Twenty-one House state-years have no district code in the source and were compared at state level. Findings are in `data_corrections_log.csv` (seven new entries): doubled counts (Idaho 2022, Iowa 2014, Massachusetts 2012, Louisiana 2016 Senate), 'Blank, Void, Scattering' pseudo-candidates counted as votes (New York House 2000-2014, 3.46M votes), Missouri's Kansas City bucket, missing counties (NH, DE, WV, RI, SC), Angus King labelled Democratic, name errors, and one single-digit error (Wyoming 2020 Senate).

- **Maine and Mississippi 2004 President fixed (2026-09-21):** MEDSL's 2004 county rows were incomplete in nearly every county (Maine 7.3% and Mississippi 3.2% below the certified totals). Replaced by `02ab_president_me_ms_2004.R` / `01dl_pe_me_ms_2004_apply.R` from Wikipedia's by-county tables (a new source origin `secondary_web`; the Mississippi table cites the state's certification PDF, saved in the repository and checked by eye for two counties); the county sums equal the FEC's certified statewide totals exactly, so these two state-years are no longer an independent check. `R/build_provenance.R` now gives every per-year official build (House, President, Senate) priority 0 and `02zb` reads state-specific `pe_<x>.rds` files like it does for the Senate. Rebuild order matters: provenance, 02z, 03b, 02z, **02zb**, 03a, 03d (03d reads the President/Senate long tables that 02zb writes).

- **Pseudo-candidate and doubled-vote fixes from the FEC reconciliation (2026-09-21):** Two classes of defect found by `qa_state_reconcile_congress.R` were fixed pipeline-wide:
  1. **Non-vote rows counted as candidates.** `PSEUDO_NAME_RE` (`R/long_helpers.R`, applied inside `finalize_long`) drops blank/void/over-/under-vote rows, "Blank, Void & Scattering", "Bvs Subtotal", "Times Blank Voted" and rejected/invalid write-ins from every long table; `PSEUDO_RE` (`01a_election_data_medsl.R`) was extended to match at the raw-MEDSL level too. Applied via `01dm_pseudo_candidate_rows_apply.R` (955 rows removed, 3.75M votes, across NY House 2000-2014, AL/WY/WA House, MEDSL Senate) — this is on top of, and consistent with, the earlier pseudo-row fixes.
  2. **Doubled county totals from redundant rollup rows.** A new filter in `read_precinct_file()` drops a precinct row whenever its own vote count exactly equals the sum of that candidate's other, real precincts in the same county (Idaho House+Senate 2022 "COUNTY TOTAL", Mississippi and Michigan House 2022, Oregon House 2018); Iowa House 2014 had a different pattern (a "Statewide" pseudo-county per district, fixed in `01ad_house_county_iowa.R`). Applied via `01dn_medsl_rollup_dedup_apply.R`. Massachusetts House 2012 has a similar but nested/inconsistent rollup structure and is still flagged, not fixed. Louisiana Senate 2016 was found to NOT be a doubling: MEDSL only carries the November all-candidates round, not the December runoff that is the actual decisive round — still flagged (needs a Louisiana source).
  - **Caution for future full reruns of `01a_election_data_medsl.R`:** several apply scripts (`01dd`, and by pattern likely others) patch `elect_pe_cty_medsl.rds`/`elect_he_cty_medsl.rds`/`elect_se_cty_medsl.rds` directly (not just the panel); re-sourcing `01a` in full silently reverts those patches (its `save_step()` overwrites unconditionally). This regressed the 2024 President blank-mode fix (367 county-years) and 3 Oregon 2024 House counties (the `candidate_level_party` label fix) during this session; both were caught by the master checks and repaired. Prefer diffing old vs. new shares files and folding in only the intended changes (as `01dn` now does), and re-run every downstream `01c*/01d*_apply.R` script that touches the same base file after any full `01a` rerun.
  - Rebuild order after any MEDSL raw-level change: `01a` (if changed) or the specific apply script, then `02a`/`02b`/`02c` (long tables), `build_provenance`, `02z`, `03b`, `02z`, `02zb`, `03a`, `03d`.
  - Result: House FEC-check BAD districts fell from 134 to about 99 (out of 3,922); QA sweep flags fell from 56 to 53.

- **Reproducibility / clean-rebuild check (2026-09-21):** `R/assemble_panel.R` rebuilds `elect_cty_final.rds` from scratch as the union of every `elect_{he,pe,se}_cty_*.rds` source file under the same priority rule `build_provenance.R` uses (per-year official build > MEDSL > historical), then diffs the result against the live panel. This is the test for whether the panel is a released, reproducible artifact rather than a file that only exists because of the specific order ~50 apply scripts happened to run in. Result: every panel row is now backed by a real source file (materialized `elect_pe_cty_medsl_cand_2024.rds` / `elect_he_cty_medsl_cand_2024.rds` for 130 rows that were previously patched in place with no source, via `01do_medsl_candidate_sum_sources.R`); Alabama 2012 and California 1998-2000 had stale/superseded duplicate source files causing genuine ambiguity, now fixed or tagged `SUPERSEDED_`; Kentucky and Florida House 2016 have a small (median 3-26 votes), undocumented discrepancy between their per-state files and MEDSL in about 60 counties that the panel resolves correctly but a from-scratch rebuild cannot re-derive without the panel to check against — flagged as a residual limitation, not fixed. Run `R/assemble_panel.R` after any change to a source file or the panel; it should print `PASS` (0 unmatched, 0 extra-in-scope, 0 value differences) apart from these two known, small, documented gaps and the special-election/pre-1990 rows that exist in source files but are intentionally out of the panel's scope.

- **House vs FEC, 1990-2002, plus a double-key audit (2026-09-21):** Extended the House-vs-FEC district-total check back from 2004 to 1990, 1992 and 2002 (`R/qa_state_reconcile_house_1990s.R`; the FEC has no clean workbook for these years, so 1990/1992 are OCR-parsed from its scanned books, `01dq_fec_1990_1992_house_parse.py`, and 2002 from a position-encoded worksheet, `01dp_fec_2002_house_senate_parse.py`). Of 822 usable district totals parsed, 434 (53%) tie within 0.5% of the FEC figure; 378 could not be matched to one of our districts (a district-numbering/coverage limit of this quick check, not investigated further); only 10 are flagged.
  - **The double-key audit this prompted found the check's OWN parser was wrong twice, and our data was right both times.** The 1990/1992 OCR misread a state name ("IOWA" as "!OVA"), so a state-tracking bug attributed Iowa's House results to the preceding state (Indiana) for 60 of ~430 1992 districts; naively summing the resulting duplicate totals produced a bogus 55%-low figure for 5 Indiana districts and 2 more states, which were logged as data defects before being caught by reading the Indiana Secretary of State's own 1992 report page images (our numbers matched exactly) and retracted. The check now drops (rather than sums) any district with more than one parsed total, since a duplicate signals a missed OCR state-header, not two real totals.
  - **South Carolina House 1990** (districts 3 and 5) was also flagged, and turned out to be a real disagreement between the FEC's scanned book and South Carolina's own official 1990-91 canvass (read directly from its page images) — our release matches the state's own canvass, which is internally consistent, and the FEC book appears to be the one in error.
  - **Oregon House 2002** (districts 1,2,4,5, and very nearly exactly 2x for district 3) is a confirmed, unresolved real anomaly: the FEC's clean 2002 workbook and our raw OpenElections source disagree, and direct inspection of the raw precinct file shows Multnomah County alone reporting more votes for Blumenauer than the FEC's whole-district certified total, with no rollup-row bug found to explain it. Flagged, not fixed; needs a different Oregon county-level source for the 2000s.
  - **Lesson for any future scanned-document parsing in this project:** treat a brand-new OCR/positional parser as a hypothesis, not ground truth, until at least one flagged result has been checked against the actual source page image — the parser is at least as likely to be wrong as the underlying data.

## 11. Current status and next steps (as of 2026-09-21, end of session)

**State:** release `v0.2.0` is current and reflects every fix below; `R/output/elect_cty_final.rds` (panel) and `R/output/data_corrections_log.csv` are the source of truth. No git repo — files are saved directly, nothing to commit.

**To resume, in priority order:**
1. **Oregon House 2000s.** A real, unexplained doubling-scale anomaly in the OpenElections source (2002 confirmed; 2000/2004 use the same file family and are unchecked) needs either a fix or a different source. See the "House vs FEC, 1990-2002" entry above.
2. **Extend `qa_state_reconcile_congress.R`/`qa_state_reconcile_pe.R` to 1990-1998** for President and Senate (only House has been checked this far back). The FEC books for 1990-1998 are already downloaded (`R/data/fec_official/federalelections9{0,2,4,6,8}.pdf`) but not yet parsed for President/Senate.
3. **Resolve the 378 unmatched 1990s House districts** in `qa_state_reconcile_house_1990s.csv` — likely a district-numbering/coverage limitation in the check script itself, not necessarily a data gap; worth a look before trusting the "53% match" figure as a ceiling.
4. **Kentucky/Florida House 2016** small per-county discrepancies (`R/assemble_panel.R` residual) — low priority, votes are off by single digits to a few hundred.
5. Remaining flagged items from earlier sweeps not yet closed: NY 2018 House (Monroe/Dutchess/Montgomery/Rensselaer/Oswego) and Senate (Lewis/Madison/Wyoming/Nassau); NY 2022 Chenango/Otsego; MI 2022 Midland; NH 2016 Strafford; MD 2020 Howard/Baltimore City; MS Yazoo 2022/Noxubee 2024; OK 2024 Canadian/Creek; NJ Bergen 2024; Missouri's Kansas City vote bucket (Senate D undercount); Maine 2012 Senate (Angus King mislabeled DEM instead of OTHER, Cynthia Dill should be DEM); Angus King-style name errors from `qa_state_reconcile_congress.R` (Bill Cassidy, Dwight Grotberg, Charles Summers) not yet corrected in the panel.
6. **Never independently benchmarked against a paid source** (America Votes/CQ, Dave Leip's Atlas) — the "most accurate free source" claim rests on completeness + the FEC/state-canvass checks above, not on beating the paid alternatives directly. If that comparison ever becomes possible (e.g. a sample of America Votes volumes), it would be the strongest remaining validation.
7. Longer-standing open items from `docs/DECISIONS.md` section 9 / the corrections log: license verification (1,448 `unverified` source rows), 185 House state-years with `source_not_found`, and the Shiny map app (not started).

**Caution carried forward:** don't re-source `01a_election_data_medsl.R` in full without immediately re-running `01dd`, `01dn` and any other apply script that patches `elect_{pe,he,se}_cty_medsl.rds` directly — it silently reverts them (see the "Pseudo-candidate and doubled-vote fixes" entry above). After any panel or source-file change, re-run in order: `build_provenance.R` → `R/assemble_panel.R` (should print PASS) → `02z` → `03b` → `02z` → `02zb` → `03a` → `03d`.
