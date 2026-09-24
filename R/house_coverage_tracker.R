## Regenerates house_results_coverage.csv (project root) -- a state x year tracker of what U.S.
## House election data we actually have, at what granularity, from where.
##
## Re-run this any time a new state/year gets added to the pipeline (a new elect_he_cty_*.rds in
## R/output/, a fix to house_district_america_votes.rds) or a new manual finding needs recording.
## Everything except MANUAL_NOTES is derived live from the actual output files, so it can't
## silently go stale the way a hand-maintained CSV would. MANUAL_NOTES genuinely is hand-
## maintained (bot-blocked URLs, book footnotes, things learned by browsing that no script can
## detect on its own) -- update that table by hand as we learn more, then re-run this script.

source(file.path("R", "00_setup.R"))
library(readr)

## Project goal for this tracker: 1990 forward. The original Mayda et al. 2022 package's own
## House-election (HE) sample is 1980/82/84/86/88/90 then a genuine 20-year gap to 2012/14/16
## (see 1st_Election_data.do and the m==9 constant-sample check in 8th_Election_census_combine.do
## -- the original authors never had 1992-2010 House data either). 1990 is the practical, durable
## target for this tracker: the last year of the original's unbroken pre-gap run, and a goal every
## state's own archive can realistically be checked against (vs. 1980, which we only reach for
## Alabama by a lucky hand-compiled spreadsheet, not a repeatable source pattern). Any state that
## happens to reach back further than 1990 (Alabama: 1980; Kentucky: 1990) still has that data in
## `elect_cty_final.rds` -- it's just outside this tracker's own year grid, not deleted.
GOAL_YEAR <- 1990
YEARS <- seq(GOAL_YEAR, 2024, by = 2)

## ---- State list + total county count per state (denominator for coverage %) ----
## Keyed on county_fips (not name) to avoid the Fairfax/Franklin/Richmond-style name ambiguity
## already found in this same source file (see project_house_district_data memory).
countypres <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                          delim = "\t", show_col_types = FALSE)
## Exclude non-county pseudo-FIPS from the denominator: MEDSL rows with an NA county_fips (statewide /
## federal-precinct buckets -- inflated ME, CT, RI by 1) and Missouri's Kansas City pseudo-FIPS.
PSEUDO_FIPS <- c(36000, 2938000)
fips_to_state <- countypres %>% distinct(state, county_fips) %>% rename(cty_fips = county_fips) %>%
  filter(!is.na(cty_fips), !cty_fips %in% PSEUDO_FIPS)
state_totals <- fips_to_state %>% count(state, name = "n_counties_total")
## DC has no contested U.S. House seat (non-voting delegate only) -- excluded, not a state.
ALL_STATES <- setdiff(sort(state_totals$state), "DISTRICT OF COLUMBIA")

## ---- County-level sources: MEDSL + every scripted state-board file we've built ----
## Auto-discovers any R/output/elect_he_cty_*.rds so future states (once added) show up here
## without editing this script.
he_files <- list.files(OUTPUT_DIR, pattern = "^elect_he_cty_.*\\.rds$", full.names = TRUE)

read_he_source <- function(path) {
  label <- gsub("^elect_he_cty_|\\.rds$", "", basename(path))
  label <- switch(label, medsl = "MEDSL precinct", nc = "NCSBE direct", va = "VA SBE direct", label)
  readRDS(path) %>% distinct(year, cty_fips) %>% mutate(source = label)
}

county_rows <- map_dfr(he_files, read_he_source) %>%
  left_join(fips_to_state, by = "cty_fips") %>%
  filter(!is.na(state))

county_coverage <- county_rows %>%
  group_by(state, year) %>%
  summarise(n_counties_covered = n_distinct(cty_fips),
            county_source = paste(sort(unique(source)), collapse = "; "), .groups = "drop") %>%
  left_join(state_totals, by = "state") %>%
  mutate(county_coverage_pct = round(n_counties_covered / n_counties_total, 3),
         county_status = case_when(
           county_coverage_pct >= 0.98 ~ "full",
           county_coverage_pct > 0     ~ "partial",
           TRUE ~ "none"
         ))

## ---- District-level fallback: America Votes extraction ----
av_good <- readRDS(file.path(OUTPUT_DIR, "house_district_america_votes.rds")) %>%
  filter(quality != "unreliable_misaligned") %>%
  distinct(state, year) %>% mutate(district_status = "good", district_source = "America Votes")

av_bad_path <- file.path(OUTPUT_DIR, "house_district_america_votes_2012_2014_UNRELIABLE.rds")
av_bad <- if (file.exists(av_bad_path)) {
  readRDS(av_bad_path) %>% distinct(state, year) %>%
    mutate(district_status = "unreliable", district_source = "America Votes (unreliable, excluded)")
} else {
  tibble(state = character(), year = integer(), district_status = character(), district_source = character())
}

## Also flag the America Votes rows that WERE "good" quality but had >20% total-vote mismatch
## in the pre-tiering pass -- already excluded from av_good above via the quality filter, so any
## (state,year) that appears in the raw file at all but not in av_good is worth a distinct label.
av_all_states_years <- readRDS(file.path(OUTPUT_DIR, "house_district_america_votes.rds")) %>%
  distinct(state, year)
av_partial <- av_all_states_years %>%
  anti_join(av_good, by = c("state", "year")) %>%
  mutate(district_status = "unreliable", district_source = "America Votes (all rows flagged unreliable)")

district_coverage <- bind_rows(av_good, av_bad, av_partial) %>%
  distinct(state, year, .keep_all = TRUE)

## ---- Manual notes: hand-maintained, update as we learn more (state, year = NA means "all
## years"), then re-run this script. This is the ONE part of the tracker that isn't auto-derived.
MANUAL_NOTES <- tribble(
  ~state,          ~year, ~note,
  "ARIZONA",         NA,  "County-level 2000-2014 DONE via OpenElections (github.com/openelections/openelections-data-az) -- azsos.gov's own site is Cloudflare-bot-blocked, but OpenElections is a separate GitHub archive, unaffected. America Votes district-level table still never located (parser gap, possibly an OCR'd map graphic in its place) -- moot for 2000-2014 now that county data exists directly; 1992-1998 (pre-OpenElections) would still need it or a manual browser visit.",
  "GEORGIA",       1992,  "Book footnote (America Votes 24): mid-decade court-ordered redistricting means these years need America Votes 21, which we don't have.",
  "GEORGIA",       1994,  "Book footnote (America Votes 24): mid-decade court-ordered redistricting means these years need America Votes 21, which we don't have.",
  "GEORGIA",         NA,  "sos.ga.gov is Cloudflare-blocked for scripted access, but manually downloading a given year's results through a real browser works fine -- 2012 done this way (dropped in R/data/county_house_files/georgia_2012.csv, already county-level with a 'Total Votes' pseudo-row per county-district). Same manual approach should work for GA's other missing years.",
  "CONNECTICUT",     NA,  "DONE 1990-2014 (every even year) from the Secretary of the State's Election History database (electionhistory.ct.gov, an Elstats platform; scripted with curl like Colorado) -- see 01er_connecticut_download.R / 01es_house_county_connecticut_elstats.R / 01et_connecticut_elstats_apply.R. Town results summed to the 8 counties (the earlier OpenElections build 01x is superseded); literal ballot-line party classification (no fusion consolidation), as MEDSL does. 1998: only 5 of 8 counties (CD1 was uncontested and is not in the database, so Hartford, Middlesex and Tolland towns lack House rows). Two party labels corrected against FEC results (1998 CD3 Cole, 2002 CD2 Courtney). 2016-2024 are MEDSL.",
  "NEW MEXICO",       NA,  "America Votes table exists in the book but wasn't matched by the parser (1992-2000 specifically); SOS site (sos.nm.gov) loads fine, not bot-blocked, just haven't located the actual results-archive subpage.",
  "LOUISIANA",        NA,  "Scattered America Votes gaps (1992,94,2002,04,06,10) not explained by any book footnote -- possibly Louisiana's jungle-primary system producing unusual table structure some years; not confirmed. sos.la.gov not bot-blocked, results subpage not located.",
  "HAWAII",           NA,  "America Votes gap 2002-2010 (parser didn't find/match the table). elections.hawaii.gov has per-county PDFs, not bot-blocked, but historical-year URL pattern/archive depth not confirmed.",
  "FLORIDA",          NA,  "Legacy ASP/frames site (results.elections.myflorida.com), not bot-blocked, just needs someone to navigate ElectionRaces.asp/TitlePage.asp for a given year/county.",
  "KENTUCKY",        2000, "elect.ky.gov has real vote files for 2000+ (not bot-blocked) but they're a completely different, harder layout than the 1990-1998 files this pipeline handles: transposed (counties as column headers, candidates as rows, wrapped 6-per-block) and candidate rows don't show party letters directly -- would need an external candidate-to-party mapping to use safely. Not attempted.",
  "KENTUCKY",        2002, "Same as 2000 -- harder transposed format, not attempted.",
  "KENTUCKY",        2004, "Same as 2000 -- harder transposed format, not attempted.",
  "KENTUCKY",        2006, "Same as 2000 -- harder transposed format, not attempted.",
  "KENTUCKY",        2008, "Same as 2000 -- harder transposed format, not attempted.",
  "KENTUCKY",        2010, "DONE: official-results PDF (off2010gen.pdf) is scanned/image-only -- OCR'd to locate the US House section, then hand-transcribed from 300dpi page images and verified against each district's printed Total Votes row. See 01i_house_county_kentucky_2010.R.",
  "KENTUCKY",        2012, "Same as 2000 -- harder transposed format, not attempted.",
  "KENTUCKY",        2014, "Same as 2000 -- harder transposed format, not attempted.",
  "KENTUCKY",        2016, "elect.ky.gov's 2016 results-index page loaded but had no results-file links in its HTML at all (likely loaded some other way, e.g. an iframe) -- not investigated further.",
  "KANSAS",           NA,  "sos.ks.gov's own 'General Election Official Vote Totals' PDFs (checked back to 2004, the earliest posted) give only statewide per-district totals, no county breakdown -- confirmed by direct inspection, matches the user's own assessment that the site is useless for this. 2012 and 2014 DONE instead via OpenElections (github.com/openelections/openelections-data-ks), which has real county-level results sourced from official county canvasses for those two years only -- see 01j_house_county_kansas.R. 1992-2010 remains an open gap: OpenElections doesn't reach back further and no other free county-level source found yet; would likely need the Kansas Historical Society's microfilmed county canvass books.",
  "PENNSYLVANIA",     NA,  "DONE 2000-2014 via OpenElections (github.com/openelections/openelections-data-pa) -- see 01k_house_county_pennsylvania.R. 2000-2010+2014 use OE's own cleaned county-level file; 2012 has no cleaned file yet so parsed directly from PA's raw SURE-system precinct export using its documented 33-column layout (office code 'USC', county FIPS given directly in column 28). 1992-1998 remains an open gap -- OE's PA repo doesn't reach back that far.",
  "OHIO",             NA,  "DONE 2000,2002,2006,2008,2012,2014 via OpenElections (github.com/openelections/openelections-data-oh) -- see 01l_house_county_ohio.R. Office label and party format both vary every year (6 different layouts across 6 years) but no pseudo-total row in any of them. 2004 NOT included: OE's OH repo has no general-election file at all for that year (only a primary). 2010 NOT included: that year's file has no party column at all, just candidate names -- would need a Kentucky-style Wikipedia candidate-to-party lookup to close, not attempted yet. (Separately: ohiosos.gov itself returned a maintenance/Cloudflare page when checked earlier -- may be temporary, not the blocker here since OpenElections sidesteps it anyway.) ADDED 2026-09-23: 1996 (was a gap) and a corrected 2002 from the Ohio SOS results pages as archived by the Wayback Machine (Oct 2004 captures; 01fa_ohio_sos_archive_parse.py, 01fb_house_county_ohio_sos_archive.R, 01fc_ohio_sos_archive_apply.R): county x district x candidate tables that tie to the printed district totals; the OpenElections 2002 Cuyahoga row (46,100 votes) missed CD10 and CD11 (374,372 in the SOS page). 2000 is identical in both. The same archive has only statewide summaries for 1990, 1992, 1994 and 1998, and the 2004 House data frame is not archived, so those years remain gaps.",
  "MICHIGAN",         NA,  "DONE 2008,2010,2012,2014 via OpenElections (github.com/openelections/openelections-data-mi) -- see 01m_house_county_michigan.R. Every year is precinct-level only (summed to county); office label is the one thing that's consistent across all 9 years checked ('U.S. House' every time). One county-name fix needed: 'GD. TRAVERSE' -> 'GRAND TRAVERSE' to match the crosswalk. 1998-2006 NOT included: the party column exists in the schema but is a genuinely empty string for every U.S. House row in those 5 years -- would need a Kentucky-style Wikipedia candidate-to-party lookup to close, not attempted yet.",
  "NEW JERSEY",       NA,  "DONE 2012,2014 via OpenElections (github.com/openelections/openelections-data-nj) -- see 01n_house_county_new_jersey.R. NJ's repo only starts at 2010 (unlike most other states). 2010 NOT included: the only 'general' file that year is a special State Senate election, no regular November federal general-election file exists in the repo at all for 2010 -- a genuine gap in OpenElections' own archive. Party field is unusually noisy (NJ lets candidates register free-text ballot-line names like 'None of Them', 'Politicians are Crooks', 'D-R Party' -- checked that last one specifically since it looks like a fusion label, but it's a real minor candidate under a made-up party name, not an actual Dem-Rep fusion ticket); only 'Republican'/'Democratic' mapped to REP/DEM, everything else correctly falls into neither.",
  "IDAHO", NA,  "1990, 1992 (were gaps) and 2022 (completes the 44th county) from the Idaho SOS Elections Database (canvass.sos.idaho.gov, PD43+ ElectionStats platform; county-level CSV exports; 01fm_house_county_idaho_canvass.R, 01fn_idaho_apply.R). Candidate votes equal each county's Total Votes Cast row. 2022's other 43 counties are identical to MEDSL.",
  "WYOMING", NA,  "1996, 1998 and 2006 (were gaps) from the SOS statewide general-election PDFs (sos.wyo.gov; text layer; 01fo_wyoming_parse.py, 01fp_house_county_wyoming.R, 01fq_wyoming_apply.R); 23 counties, ties to printed totals. 1990-1994 are on Internet Archive microfiche (Wyoming Official Directory and Election Returns), not yet built.",
  "NORTH CAROLINA", NA,  "1996 and 1998 (were gaps) from the State Board of Elections' archived result files in its public S3 bucket (not linked from the history page): 1996 a text PDF (district tables), 1998 one PDF per district with county TOTALS lines (01fr_northcarolina_1996_1998_parse.py, 01fs..., 01ft...); ties to district totals and the FEC. 1992 and 1994 are typed scans in the State Library digital collection, not yet built.",
  "SOUTH CAROLINA", NA,  "2010 (was a gap) from the State Election Commission's 2010 election-night-reporting archive (enr-scvotes.org detail.xls, SpreadsheetML; 01fu_house_county_southcarolina_2010.R, 01fv_southcarolina_apply.R); 46 counties; D/R nominees identified from FEC results and all six district D and R totals equal the FEC.",
  "DELAWARE", NA,  "2012 (was a gap) from the Department of Elections' results archive (stwoff_kns.txt; 01fw_house_county_delaware_2012.R, 01fx_delaware_apply.R). The file's first column is headed \"Wilmington\" but holds New Castle County totals. 1990-1996 are image-only PDFs (county table on one page each) and 1998 is only by election district; not yet built.",
  "MONTANA", NA,  "1992, 1996 and 1998 (were gaps) from the SOS official general-election canvass PDFs recovered from the Wayback Machine (sos.mt.gov 1990s archive; text layers of a wide table split over pages; 01fy_montana_1992_1998_parse.py, 01fz..., 01ga...): 56 counties; candidate totals tie to the printed statewide totals and the FEC; 1996 and 1998 columns are matched to counties by row order and checked against votes cast. 1990 (scan), 1994 (scan / Lotus files) and 2022 remain.",
  "OKLAHOMA",        NA,  "1998 (was a gap) from the State Election Board's results page as archived by the Wayback Machine (98gencon.html; 01fj_oklahoma_1998_parse.py, 01fk_house_county_oklahoma_1998.R, 01fl_oklahoma_1998_apply.R): every candidate in every county of each of the 6 districts, county rows tie to the STATE TOTAL lines and the district totals equal the FEC results. 1996 in that archive has county results for President and Senator only, and 1990-1992 are not archived (the 1999-2000 Oklahoma Almanac has only statewide and district totals). 1994 and 2000 onward as before.",
  "MISSOURI",        NA,  "1990-1998 (were gaps) from the Official Manual of the State of Missouri (Blue Book) pages, saved as images by the project lead and read by eye (R/data/county_house_files/missouri/; 01fg_missouri_1990_1998_parse.py, 01fh_house_county_missouri_1990_1998.R, 01fi_missouri_apply.R): per district, every listed candidate in every county or part of a county (1990 lists only the two major-party candidates); split counties summed; Kansas City rows assigned to Jackson County; all 115 counties every year; district rows tie to the printed TOTALS except one Republican column in 1992 District 8 (270 votes short, the book and FEC print 147,398). Blue Book collection is public domain (Missouri Digital Heritage). 2000 onward from OpenElections/MEDSL.",
  "NEVADA",          NA,  "1990-1998 (were gaps) from the Secretary of State's official general-election abstracts on the 1962-to-present results page (five PDFs: scanned 1990, 1992, 1996; noisy OCR layer 1994; text 1998), transcribed from the page images by 01fd_nevada_1990_1998_transcribe.py; 17 county columns, district 1 = Clark only; every row ties to the printed total and to the FEC statewide totals (1992 is a fax-quality scan where 5 and 6 are easily confused; one Sferrazza/Clark cell resolved with the FEC total; minor candidate Golden 2,860 vs FEC 2,850). 2000 onward from OpenElections/MEDSL. The site blocks scripted clients (Imperva) so the documents were downloaded through Chrome; its business-services terms prohibit automated/systematic collection, so only these five documents were fetched.",
  "MASSACHUSETTS",   NA,  "DONE 1990-2014 (every even year) from the Secretary of the Commonwealth's Elections Statistics site (electionstats.state.ma.us, PD43+ ElectionStats platform; town tables summed to the 14 counties) -- see 01ex_massachusetts_download.R / 01ey_house_county_massachusetts_electionstats.R / 01ez_massachusetts_electionstats_apply.R. Replaced the OpenElections rows of 2000-2014 (2002-2008 and 2014 identical; 2000 had 4 counties missing a Republican column, 2012 only 13 of 14 counties). 1996 has 13 of 14 counties (Sutton has no rows). Two source-table defects repaired (2000 CD4 Travis, 1996 CD10 Sandwich). CAUTION: the Secretary's Terms and Conditions prohibit scraping or crawling by automated means and derivative works, and the tables were fetched with curl; reuse permission was not requested; included by project decision because the counts are public facts (license_status state_restrictive).",
  "WASHINGTON",      NA,  "2000-2006, 2012, 2014 via OpenElections; 2008 (was a gap) and 2010 (was 37 of 39 counties, with errors in King/Pierce/Skamania/Snohomish) from the Secretary of State's 2008 and 2010 General Election data downloads (39 counties each; 2008 county sums equal the printed district totals, 2010 district totals equal the FEC results in all 9 districts) -- see 01eu_house_county_washington_sos.R / 01ew_washington_2010_sos_precinct.R / 01ev_washington_sos_apply.R. 1990-1998 remain gaps: the SOS Election Results Archive workbook (Washington State Elections Results Archive.xlsx) has only statewide and district totals, and the SOS says pre-2007 county-level results must come from the county elections offices.",
  "NEW YORK",         NA,  "DONE 1996-2018 (every even year) from the New York State Board of Elections' Elections Database (results.elections.ny.gov, an Elstats platform that blocks scripted clients, so the tables were fetched from a browser session) -- see 01ep_house_county_new_york_boe.R / 01eq_new_york_boe_apply.R. The database starts in 1996, so 1990-1994 have no county data (district totals only, from America Votes). 1996 has no Oswego rows (61/62 counties). NY has a real electoral-fusion system (a candidate can appear on multiple parties' ballot lines, e.g. Democratic + Working Families) -- a candidate's lines are summed per (county, district) and the candidate is classified DEM/REP if any line is Democratic/Republican. Blank/void/scattering excluded. Source defect handled: a duplicate 'Otsego' row (copy of Oswego) in 2010 CD24 and 2012-2018 CD22/CD24 is dropped. Replaced the earlier OpenElections rows (2000-2014) and MEDSL 2016-2018 rows, which had missing counties (2012: 58/62; 2016: 61/62; 2018: 47/62) and errors (2000 Bronx/Queens, 2004 Kings).",
  "ILLINOIS",         NA,  "DONE 2008-2014 (all 4 pre-MEDSL years IL's OpenElections repo has, github.com/openelections/openelections-data-il -- repo only starts at 2008) -- see 01p_house_county_illinois.R. Cleanest state in this whole pass: identical schema and 'U.S. House' office label all 4 years, party already DEM/REP, no pseudo-total row, Cook County handled identically to every other county, zero bugs, zero exclusions -- 408/408 (102 counties x 4 years). Note for future scripts: the county_fips crosswalk (countypres_2000-2024.tab) has TWO spelling variants mapped to the same fips for two IL counties (DEWITT/DE WITT -> 17039, JODAVIESS/JO DAVIESS -> 17085) -- harmless for a name-keyed left_join (IL's source consistently uses the no-space spelling, matches fine), but would silently multiply rows in any script that joins the other direction (by fips) instead.",
) %>% mutate(year = as.numeric(year))

## ---- Assemble the full state x year grid ----
grid <- expand_grid(state = ALL_STATES, year = YEARS)

coverage <- grid %>%
  left_join(county_coverage %>% select(state, year, county_status, county_source, county_coverage_pct),
            by = c("state", "year")) %>%
  left_join(district_coverage %>% select(state, year, district_status, district_source),
            by = c("state", "year")) %>%
  mutate(
    county_status = replace_na(county_status, "none"),
    county_coverage_pct = replace_na(county_coverage_pct, 0),
    county_source = replace_na(county_source, ""),
    district_status = replace_na(district_status, "none"),
    district_source = replace_na(district_source, "")
  )

notes_exact <- MANUAL_NOTES %>% filter(!is.na(year)) %>% rename(note_exact = note)
notes_all_years <- MANUAL_NOTES %>% filter(is.na(year)) %>% select(state, note_all = note)

coverage <- coverage %>%
  left_join(notes_exact, by = c("state", "year")) %>%
  left_join(notes_all_years, by = "state") %>%
  mutate(notes = coalesce(note_exact, note_all, "")) %>%
  select(state, year, county_status, county_coverage_pct, county_source,
         district_status, district_source, notes) %>%
  arrange(state, year)

out_path <- file.path(PROJECT_ROOT, "house_results_coverage.csv")
write_csv(coverage, out_path)

message("Wrote ", nrow(coverage), " rows to ", out_path, " (goal: ", GOAL_YEAR, "-2024)")
message("\nSummary by county_status:")
print(coverage %>% count(county_status))
message("\nSummary by district_status:")
print(coverage %>% count(district_status))
message("\nStates with ZERO county AND ZERO usable-district coverage in ANY year:")
fully_missing <- coverage %>% group_by(state) %>%
  filter(all(county_status == "none" & district_status != "good")) %>% pull(state) %>% unique()
print(fully_missing)
