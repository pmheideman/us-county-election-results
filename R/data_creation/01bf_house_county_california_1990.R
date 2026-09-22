## California 1990: same manual-transcription approach as 1992/1994/1996 (`01bg`/`01bh`/`01be`),
## applied to `CA1990-sov-complete.pdf`. Same story as 1992/1994: the extracted text layer is badly
## character-corrupted (e.g. "Finl ~ Di<tri«" for "First Congressional District", "Coogrc&&,." for
## "Congress,") and was NOT trusted for any digit -- every number in `1990_raw.csv` (committed
## alongside this script) was read directly off each page rendered at 220dpi (`pdftoppm -r 220`).
## The extracted text was used only to roughly locate the section (pages 32-40 of 82, page footers
## "20"-"28").
##
## **1990 uses California's PRE-1990-census congressional map** -- 45 districts, not the 52 used by
## 1992/1994/1996/1998/2000 (post-reapportionment). District headers are spelled as words ("First
## Congressional District", ... "Forty-Fifth Congressional District"), encoded here with plain
## numerals 1-45 in the raw CSV for consistency with every other year's file. Also unlike every
## later year checked, this format has NO "Votes not Cast in Race" column at all -- county rows are
## exactly (n_party_codes) numbers with nothing extra to discard, and multi-county districts here
## always have a printed "District Totals" + "Percent" row (used as the field name is literal --
## `County`/`Percent` labels appear per-district, unlike later years' shared party-code-row style,
## but the underlying party-code-then-county-rows-then-totals structure is the same).
##
## **Real ballot-composition quirks found and handled, distinct from 1992/1994's "no Republican
## candidate" pattern**:
## - CD-18 (Lehman) was completely UNOPPOSED -- only one candidate on the ballot at all, no Rep, no
##   third party, "Percent" prints as a flat 100. Encoded with `0` in the Rep position, same
##   treatment as any other no-Republican district.
## - **CD-43 and CD-45 have NO DEMOCRATIC CANDIDATE at all** -- a new pattern not seen in 1992/1994
##   (those only ever lacked a Republican). The printed candidate order for both is Rep-Inc first,
##   then Lib/P&F -- encoded with an explicit `0` inserted in the FIRST (Dem) position so the raw
##   CSV's rank-based Dem=col1/Rep=col2 convention stays valid without a per-district code
##   exception in the aggregation script (same technique as 1992/1994's missing-Republican rows,
##   just applied to the other party).
## - CD-32's Orange County row prints as a literal `0`/`0` (a genuine zero-vote sliver of the
##   county touching this district, not a data error) -- included as read; contributes nothing to
##   any total either way.
##
## No large (>5%) unresolved district-total discrepancies found this year (checked every district's
## county-row sum against its printed total using the same practical few-percent tolerance
## established for 1996 -- nothing here needed a targeted re-zoom).

source(file.path("R", "00_setup.R"))
library(readr)

## Same read approach as 1992/1994/1996's scripts -- read_csv()'s rectangular-file inference
## silently mishandles this ragged (variable-column-count) CSV, so bypass it: read raw lines, split
## by comma.
raw_lines <- readLines(
  file.path(PROJECT_ROOT, "R", "data", "county_house_files", "ca_manual_transcription", "1990_raw.csv")
)
raw_lines <- raw_lines[nchar(trimws(raw_lines)) > 0]
split_fields <- strsplit(raw_lines, ",", fixed = TRUE)
max_fields <- max(lengths(split_fields))
raw <- as_tibble(do.call(rbind, lapply(split_fields, function(x) {
  length(x) <- max_fields
  x
})), .name_repair = "minimal")
names(raw) <- paste0("X", seq_len(max_fields))
raw <- raw %>% mutate(across(-X2, as.numeric))

## Variable column count per row (district, county, then 1-4 candidate vote columns) -- sum all
## candidate columns present for totalvote. Dem is always the first candidate column, Rep always
## the second (explicit 0 for no-Democrat districts CD-43/CD-45, and no-Republican district CD-18,
## see header comment).
long <- raw %>%
  rename(district = X1, county = X2) %>%
  pivot_longer(cols = -c(district, county), values_to = "votes", names_to = "col") %>%
  filter(!is.na(votes))

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ca_fips <- county_fips_crosswalk %>% filter(state == "CALIFORNIA") %>% select(county_name, county_fips)
stopifnot(nrow(ca_fips) == 58)

is_first_two <- long %>% group_by(district, county) %>% mutate(rank = row_number()) %>% ungroup()

county_party <- is_first_two %>%
  mutate(party = case_when(rank == 1 ~ "DEM", rank == 2 ~ "REP", TRUE ~ "OTHER")) %>%
  group_by(county) %>%
  summarise(
    demovote_n = sum(votes[party == "DEM"]),
    repuvote_n = sum(votes[party == "REP"]),
    totalvote = sum(votes),
    .groups = "drop"
  )

elect_he_cty_ca_1990 <- county_party %>%
  left_join(ca_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "CALIFORNIA", year = 1990L, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ca_1990")

message("CA 1990 House county-level rows built: ", nrow(elect_he_cty_ca_1990), " (of possible 58)")
sanity <- elect_he_cty_ca_1990$repuvote + elect_he_cty_ca_1990$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

missing <- ca_fips %>% filter(!county_fips %in% elect_he_cty_ca_1990$cty_fips)
if (nrow(missing) > 0) {
  message("Missing ", nrow(missing), " counties (expected -- 1990's older 45-district map may not ",
          "touch every county the way the 52-district map does): ", paste(missing$county_name, collapse = ", "))
} else {
  message("Full 58/58 California counties.")
}

message("Statewide totalvote sum: ", sum(elect_he_cty_ca_1990$totalvote))
message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
