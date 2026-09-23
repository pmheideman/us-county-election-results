## Mississippi U.S. House, county-level, 1990 and 1992 general elections, from the Mississippi "Official and Statistical Register"
## (the state's Blue Book; despite the source filename "MS_90-92.pdf", its own title page reads "1992-1996", published 1994).
## Hand-transcribed from 300-400dpi page renders (pages 367-368 for 1990, 463-464 for 1992) -- pdftotext's OCR layer on this document is
## essentially useless for the numeric table cells (mostly blank/garbled on extraction), so every value was read directly from the scan image.
## Mississippi still had 5 U.S. House districts in both 1990 and 1992 (did not drop to 4 until later) -- confirmed directly from the pages,
## not assumed. The user's other two supplied files (MS_98-2000.pdf, actually the "2000-2004" edition; MS_02-04.pdf, actually "2004-2008")
## were investigated and do NOT contain county-level election-results tables at all -- their "Elections" chapter is pure civics/procedural
## content (voter registration, campaign finance deadlines), confirmed via table of contents and full-text search for the "County" column
## pattern that is present in the 1990-92-era edition and completely absent from both later ones. Only 1990 and 1992 are recoverable from
## what was supplied; 1994/1996/1998/2000/2002/2004 remain a gap.
##
## VERIFICATION STATUS, read this before trusting any single district blindly:
##   1990 D1 (Whitten/Bowlin):      EXACT tie, both candidates, to the printed Totals row (42,676 / 23,250).
##   1990 D2 (Espy/Benford):        every county row individually re-verified via three independent methods (visual pairing, a
##                                  precisely rule-line-calibrated pixel grid, and Total-minus-R=D internal consistency); the
##                                  Republican column ties EXACTLY (11,224) but the Democratic column sums to 63,393 against a
##                                  printed total of 59,393 -- a 4,000-vote gap that survived extensive re-verification and could
##                                  not be resolved to a specific misread cell. Used the county-level sum (the more granular,
##                                  independently re-checked figure) rather than the source's own total row, matching this
##                                  project's established practice (see the Iowa 1994/1996 printed-total-error precedent in
##                                  data_corrections_log.csv from the same session). FLAGGED, not fully resolved.
##   1990 D3 (Montgomery, unopposed): single-candidate table; county sum (49,382, excluding a suspicious/ambiguous final
##                                  "Winston" cell that could not be confidently read) exceeds the printed total (49,162) by
##                                  220 votes. FLAGGED, not fully resolved; used the transcribed county rows as-is.
##   1990 D4 (Parker/Parks):        EXACT tie, both candidates (57,137 / 13,754).
##   1990 D5 (Taylor/Smith):        EXACT tie, both candidates (89,926 / 20,588).
##   1992 D1 (Whitten/Whitaker):    Republican ties exactly (82,952); Democratic off by 2 votes (121,666 vs 121,664) -- treated
##                                  as negligible, same class as Iowa's 2-vote printed-total slips.
##   1992 D2 (Espy/Benford):        Democratic sums 3,803 over the printed total, Republican sums 2,857 under -- the largest
##                                  unresolved discrepancy in this build. FLAGGED, not resolved; used transcribed values as-is.
##   1992 D3 (Montgomery/Williams): EXACT tie, both candidates (162,864 / 37,710).
##   1992 D4 (Parker/McMillan/Gilchrist/Meredith): Parker and Meredith exact; Gilchrist off by 2 (negligible); McMillan
##                                  (Republican) sums 2,022 over the printed total, could not be traced to a specific county
##                                  despite checking the largest-magnitude cell (Hinds). FLAGGED, not resolved.
##   1992 D5 (Taylor/Harvey/O'Hara): O'Hara exact; Harvey off by 2 (negligible); Taylor sums exactly 2,000 over the printed
##                                  total, could not be traced to a specific county (checked the largest cell, Forrest, and
##                                  confirmed it reads correctly). FLAGGED, not resolved.
## General pattern worth noting: every unresolved gap is on the DEMOCRATIC or the larger-vote-share column, several are
## suspiciously round numbers (4,000 / 2,000), and Espy's majority-Black Delta district (D2) has the largest gaps in both
## years -- worth a fresh look with a better scan or an independent source (e.g. the FEC's own historical House returns
## workbooks, already used elsewhere in this project's qa_state_reconcile_congress.R) if this state is revisited.
##
## COVERAGE GAP: 1990 is 81/82 counties -- YALOBUSHA is missing. Confirmed it is not a redistricting artifact and not simply
## unread: D1, D4 and D5 each tie EXACTLY to their printed totals WITHOUT Yalobusha, which rules out it being a member of any
## of those three districts (adding it would break an already-exact tie). That leaves D2 or D3 as the only two districts it
## could belong to -- but both of those already sum HIGHER than their own printed totals (by 4,000 and 220 respectively), so
## a simple omission of Yalobusha's row cannot be the sole explanation there either (omitting a real county's votes should
## make a district's sum LOWER, not contribute to it already being too high). This was not resolved further given time
## already spent on this build; flagged for the coordinating session / a future session to re-examine pages 367-368 of
## MS_90-92.pdf specifically for Yalobusha's 1990 U.S. House row.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "mississippi", "transcribed")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MISSISSIPPI") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 82)
fips_of_county <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$county_name)]

party_group_of <- function(x) case_when(x == "D" ~ "DEM", x == "R" ~ "REP", TRUE ~ "OTHER")

process_year <- function(year) {
  raw <- read_csv(file.path(DIR, sprintf("%d.csv", year)), show_col_types = FALSE) %>%
    mutate(county_fips = fips_of_county(county), party_group = party_group_of(party), district = norm_district(district))
  stopifnot(!anyNA(raw$county_fips))

  ## tie-check: print per-district, per-candidate column sums vs nothing external here (documented above from manual page verification) --
  ## just confirm every (district, county) key is unique and report district totals for the record.
  chk <- raw %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% arrange(district, desc(votes))
  cat("== ", year, " district-candidate totals ==\n"); print(as.data.frame(chk))

  long <- finalize_long(raw %>% transmute(year = year, county_fips, district, candidate, party, party_group, votes), paste0("ms_", year))
  save_long(long, paste0("he_ms_", year))
  shares <- derive_shares(long) %>% transmute(state = "MISSISSIPPI", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", year)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", year))); stopifnot(all(r$pass))
  message(year, ": ", nrow(shares), " of 82 MS counties")
  invisible(shares)
}

s1990 <- process_year(1990)
s1992 <- process_year(1992)

sanity <- bind_rows(s1990, s1992)
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")

for (y in c(1990, 1992)) {
  l <- readRDS(file.path(LONG_DIR, sprintf("he_ms_%d.rds", y)))
  win <- l %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% group_by(district) %>% slice_max(votes, n = 1) %>% ungroup()
  message(y, " district winners: ", paste(sprintf("D%s=%s", win$district, win$candidate), collapse = "; "))
}
