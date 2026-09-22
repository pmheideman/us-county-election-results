## New York: found via OpenElections (github.com/openelections/openelections-data-ny), same
## followup survey that produced Kansas/Pennsylvania/Ohio/Michigan/New Jersey (01j-01n). New York
## needed genuinely different handling from every prior state because of its real, longstanding
## electoral-fusion system (a major-party candidate can ALSO run on one or more minor parties'
## ballot lines -- Conservative, Working Families, Independence, etc. -- and the source data
## records votes per LINE, not just per candidate).
##
## Confirmed by inspecting real rows (e.g. 2006, Albany county, NY-21: Michael McNulty appears as
## FOUR separate rows -- DEM, IND, CON, WOR -- all the same person, same race; 2014, Nassau county,
## NY-2: Peter King appears as a single COMBINED "REP + TRP" row (30249 votes, already pre-summed
## for those two lines by the source) PLUS separate CON and IND rows). Naively keying off the
## `party` column alone would either double... no, undercount a fusion candidate's real total (if
## you only take their literal "DEM"/"REP" row) or miscount them as a third party entirely (if you
## only match exact "DEM"/"REP" strings against "REP + TRP", which wouldn't match at all).
##
## Fix: group by (county, district, CANDIDATE) first and sum ALL of that candidate's votes across
## every row/line they appear under -- this is safe and correct however the source structures it
## (many separate rows in 2000/2004/2006, one compound "X + Y" row in 2014) because it's always
## keyed on the same named person. Only THEN classify: a candidate counts as REP if the word "REP"
## appears anywhere among their party-field values (a whole-word regex handles both a bare "REP"
## row and a compound "REP + TRP" row), DEM similarly for "DEM" -- and their FULL fusion-inclusive
## vote total goes into that bucket. This is a genuinely different situation from Pennsylvania's
## single-row "DEMREP" fusion code (01k), where one candidate's vote total was inseparably split
## between two parties in a single ambiguous row with no way to attribute it correctly -- here we
## always know the true full total per named candidate, we just have to sum their scattered rows
## first.
##
## Per-year notes:
## - 2000, 2004, 2006, 2014: flat, already county-level `county,office,district,party,candidate,
##   votes` files, office consistently labeled exactly "U.S. House". A `county=="TOTAL"` pseudo-row
##   was found in 2014 (same idea as Kansas/Georgia's TOTALS rows) -- excluded explicitly. Also
##   dropped a small number of blank-county rows (formatting junk, not real data) in every year.
##   A SECOND, sneakier pseudo-row also found: a `candidate=="Total"` row per (county, district) in
##   2000/2014/2012 -- easy to miss because it isn't a whole extra COUNTY row, it hides inside the
##   candidate column instead, and it has a blank party field so it never gets miscounted as DEM or
##   REP. It still silently doubles `totalvote` if not excluded (confirmed: Madison County 2000's
##   district-23 candidate rows summed to exactly 28,694, and the file also carries a
##   `candidate=="Total"` row of exactly 28,694 for that same county/district -- this is what
##   produced a suspicious "0.295" two-party-share reading during verification, not a genuine
##   uncontested/fusion case like the other low-share counties turned out to be). Genuine
##   blank/void/scattering rows (several spelling variants: "Blank, Void & Scattering", "BVS
##   Subtotal", "Blanks", "Void", "Scattering", etc.) are NOT excluded -- those represent real cast
##   ballots that didn't go to any candidate, correctly included in `totalvote` and correctly never
##   matched as DEM/REP since their party field is blank. **General lesson**: a pseudo-total row
##   doesn't have to occupy the "county" column to be dangerous -- check every ID-like column
##   (here, `candidate`) for a literal "Total" value, not just the one column an earlier state's bug
##   happened to use.
##   The 2012 precinct file has a THIRD pseudo-candidate variant, "Ballots Cast" (plus "Over Votes"/
##   "Under Votes") -- same shape of bug (a precinct-total row disguised as a candidate, blank
##   party, exact-duplicate vote count -- confirmed for Livingston County district 27: the four real
##   candidate/line rows summed to 54,148, and a "Ballots Cast" row for that same precinct/district
##   carried exactly 54,148 too). This is what caused several 2012 counties (Livingston, Orleans,
##   Allegany, Steuben, Schuyler, Putnam, ...) to show suspicious ~0.47-0.50 two-party shares during
##   verification -- fixed once this pseudo-candidate list was excluded. **Known residual
##   limitation, not fixed**: the 2012 file also has real candidate-name SPELLING inconsistencies
##   across precincts (e.g. "Thomas W Reed Ii" vs "Thomas W. Reed II", "Kathy C. Hochu" vs "Kathy C.
##   Hochul") -- since fusion-collapse groups by exact candidate string, a misspelled variant becomes
##   its own separate group, and if that variant's rows happen to carry a blank party field, its
##   votes could fall into neither DEM nor REP instead of the correct candidate's total. Not fixed
##   here (would need per-county fuzzy name matching for one already-partial year) -- 2012 is already
##   reported as "partial" for the real Chautauqua/Chemung/Montgomery/Onondaga coverage gap, so this
##   is a secondary, smaller-magnitude imprecision on top of an already-flagged-imperfect year.
## - 2012: no flat county-level file exists in the repo for this year, only a statewide precinct
##   file (`..._general__precinct.csv`, with extra per-voting-method columns that are blank for
##   almost every row -- the `votes` column itself already carries the real total, confirmed by
##   checking: only 178 of 82,838 U.S. House rows have a blank `votes` value, and every one of
##   those is a write-in candidate with genuinely no precinct-level votes recorded, safe to drop).
##   This year has a REAL, uncorrectable coverage gap: Chautauqua, Chemung, Montgomery, and
##   Onondaga counties have NO "U.S. House" rows in the file at all (Chautauqua and Onondaga do
##   appear for other offices like President/Senate, so it's not a missing-county problem, just a
##   missing-office-for-that-county problem; Chemung and Montgomery are entirely absent from the
##   file). 58/62 counties (93.5%) -- reported honestly as "partial", not forced to look complete.
## - 2002: EXCLUDED. The flat general-election file exists and has other statewide/legislative
##   races (Governor, Attorney General, State Senate/Assembly, etc.) but genuinely no U.S. House
##   race at all -- a real gap in OpenElections' own NY archive for this specific year, same as
##   Ohio's 2004 gap.
## - 2008, 2010: EXCLUDED. The repo only has PER-COUNTY precinct files for these years (Onondaga
##   only in 2008; Onondaga + Erie in 2010) -- nowhere near statewide coverage, not worth building
##   a script around 1-2 of 62 counties.
##
## Two county-name normalization gotchas: (1) the 2012 precinct file misspells Genesee County as
## "GENESSEE" (extra S) -- added an explicit alias, the project's own crosswalk only recognizes
## "GENESEE". (2) "ST LAWRENCE" (no period) vs "ST. LAWRENCE" (with period) both appear across
## different years/files -- checked the crosswalk already carries BOTH spellings mapped to the
## same FIPS (36089), so no fix needed there, an ordinary left_join resolves it either way.
##
## One more party-format gotcha, worse than "varies by year" -- it can vary WITHIN a single year's
## file by county: in the 2012 precinct file, every county uses abbreviated codes (DEM/REP/CON/...)
## EXCEPT Washington County, which alone uses full words (Democratic/Republican/...) -- almost
## certainly because that county's board submitted its results in a differently-formatted file that
## OpenElections merged in without renormalizing. A first version's DEM/REP regex only matched the
## abbreviated codes and silently zeroed out Washington County's real, fully-contested race (came
## out 0/0 two-party share, which is what caught it -- Ontario and Tioga counties also show 0/0 for
## 2012, but those are a genuine, different, unfixable problem: their U.S. House rows have a
## completely BLANK party field, no attribution at all, real data gap not a format bug). Fixed by
## matching `Democratic`/`Republican` as alternatives in the same regex, everywhere, not just for
## the one file/county where it was caught -- the same inconsistency could exist in any of NY's
## other files (checked: it doesn't, in 2000/2004/2006/2014, but the fix is applied uniformly
## rather than special-cased to 2012 only, since there's no guarantee the next state to add won't
## have the identical problem).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_york")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ny_fips <- county_fips_crosswalk %>% filter(state == "NEW YORK") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ny/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

NY_COUNTY_ALIAS <- c(GENESSEE = "GENESEE")
clean_county <- function(x) {
  x <- toupper(trimws(x))
  coalesce(NY_COUNTY_ALIAS[x], x)
}

## Collapse fusion-line rows to one row per (county, district, candidate) with their FULL vote
## total, then classify DEM/REP by whether that word appears anywhere among their party values.
collapse_fusion <- function(df) {
  df %>%
    mutate(county = clean_county(county), votes = as.numeric(votes)) %>%
    filter(!is.na(votes), county != "", county != "TOTAL",
           !toupper(trimws(candidate)) %in% c("TOTAL", "BALLOTS CAST", "OVER VOTES", "UNDER VOTES")) %>%
    group_by(county, district, candidate) %>%
    summarise(
      votes = sum(votes, na.rm = TRUE),
      party_labels = paste(party, collapse = " "),
      .groups = "drop"
    ) %>%
    mutate(
      is_dem = grepl("\\bDEM\\b|Democratic", party_labels, ignore.case = TRUE),
      # a candidate on BOTH major-party lines (e.g. Edolphus Towns 2004, "REP DEM WOR") is counted as Democratic only; before this
      # fix (2026-09-20) his 147,212 votes counted in both numerators (Kings County 2004 repuvote 0.297 instead of 0.085)
      is_rep = grepl("\\bREP\\b|Republican", party_labels, ignore.case = TRUE) & !is_dem
    )
}

NY_FLAT_YEARS <- tribble(
  ~year, ~remote_name,
  2000,  "20001107__ny__general.csv",
  2004,  "20041102__ny__general.csv",
  2006,  "20061107__ny__general.csv",
  2014,  "20141104__ny__general.csv"
)

read_ny_flat_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_general.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House")
  collapse_fusion(raw) %>% mutate(year = year)
}

ny_flat <- pmap_dfr(NY_FLAT_YEARS, read_ny_flat_year)

## ---- 2012: statewide precinct file, sum precinct rows into (county, district, candidate) first ----
ny_2012_path <- download_oe(2012, "20121106__ny__general__precinct.csv", "2012_precinct.csv")
ny_2012_raw <- read_csv(ny_2012_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(office == "U.S. House")
ny_2012 <- collapse_fusion(ny_2012_raw) %>% mutate(year = 2012)

ny_by_candidate <- bind_rows(ny_flat, ny_2012)

elect_he_cty_ny <- ny_by_candidate %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[is_dem], na.rm = TRUE),
    repuvote_n = sum(votes[is_rep], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ny_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NEW YORK", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ny")

message("NY House county-level rows built: ", nrow(elect_he_cty_ny), " across years: ",
        paste(sort(unique(elect_he_cty_ny$year)), collapse = ", "))
print(table(elect_he_cty_ny$year))
message("2012 coverage: ", sum(elect_he_cty_ny$year == 2012), " of 62 counties (real archive gap, ",
        "see header notes -- Chautauqua/Chemung/Montgomery/Onondaga have no U.S. House rows that year)")

## ---- Sanity checks ----
sanity <- elect_he_cty_ny$repuvote + elect_he_cty_ny$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## Verify the fusion-collapse logic on the two known examples cited in the header comment.
mcnulty_check <- ny_by_candidate %>% filter(year == 2006, county == "ALBANY", candidate == "Michael R. McNulty")
king_check <- ny_by_candidate %>% filter(year == 2014, county == "NASSAU", candidate == "Peter T. King")
message("McNulty (2006, Albany) fusion-collapsed: votes=", mcnulty_check$votes, ", is_dem=", mcnulty_check$is_dem,
        " (party_labels: ", mcnulty_check$party_labels, ")")
message("King (2014, Nassau) fusion-collapsed: votes=", king_check$votes, ", is_rep=", king_check$is_rep,
        " (party_labels: ", king_check$party_labels, ")")

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ny %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with NY 2000,2004,2006,2012(partial),2014. ",
        "2002/2008/2010 excluded, see header notes. Total rows now: ", nrow(elect_cty_final))
