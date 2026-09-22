## California 1994: same manual-transcription approach as 1996/1992 (`01be`/`01bg`), applied to
## `CA1994-sov-complete.pdf`. An EARLIER pass at this file (before the systematic page-by-page
## image-reading approach was adopted) tried parsing the file's extracted text directly and found
## it badly unreliable -- not just garbled labels but real STRUCTURAL breakage on some pages
## (county names glued to numbers with no space, a "District Totals" row split across 6+ separate
## lines instead of one). That was true of the raw TEXT LAYER; the rendered PAGE IMAGES are, same
## as 1992/1996, perfectly legible -- every number in `1994_raw.csv` (committed alongside this
## script) was read directly off each page rendered at 220dpi (`pdftoppm -r 220`), never from the
## extracted text, which was used only to roughly locate the section (pages 62-69 of 132, "1st
## Congressional District" through "52nd Congressional District", page footers "43"-"50").
##
## Format: same as 1992/1996/1998/2000 -- a party-code row, county rows, a "District Totals" +
## "Percent" row for multi-county districts (single-county districts print no separate totals row
## -- the one county row IS the total). Full 52 districts (post-1990-census map).
##
## **Three districts have NO Republican candidate at all** (CD-18... no, correction: CD-33 Roybal-
## Allard vs. only a P&F candidate, CD-37 Tucker vs. only a Libertarian, both safe Democratic LA
## seats) -- same pattern as 1992's 4 no-Republican districts, encoded the same way: an explicit `0`
## in the Rep position so the raw CSV's rank-based Dem=col1/Rep=col2 convention stays valid for
## every row without a per-district exception in the aggregation script.
##
## No large (>5%) unresolved district-total discrepancies found this year (checked every district's
## county-row sum against its printed total using the same practical few-percent tolerance
## established for 1996 -- nothing here needed a targeted re-zoom).

source(file.path("R", "00_setup.R"))
library(readr)

## Same read approach as 1992/1996's scripts -- read_csv()'s rectangular-file inference silently
## mishandles this ragged (variable-column-count) CSV, so bypass it: read raw lines, split by comma.
raw_lines <- readLines(
  file.path(PROJECT_ROOT, "R", "data", "county_house_files", "ca_manual_transcription", "1994_raw.csv")
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

## Variable column count per row (district, county, then 2-6 candidate vote columns) -- sum all
## candidate columns present for totalvote. Dem is always the first candidate column, Rep always
## the second (explicit 0 for the 2 no-Republican districts, see header comment).
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

elect_he_cty_ca_1994 <- county_party %>%
  left_join(ca_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "CALIFORNIA", year = 1994L, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ca_1994")

message("CA 1994 House county-level rows built: ", nrow(elect_he_cty_ca_1994), " (of possible 58)")
sanity <- elect_he_cty_ca_1994$repuvote + elect_he_cty_ca_1994$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

missing <- ca_fips %>% filter(!county_fips %in% elect_he_cty_ca_1994$cty_fips)
if (nrow(missing) > 0) {
  message("Missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
} else {
  message("Full 58/58 California counties.")
}

message("Statewide totalvote sum: ", sum(elect_he_cty_ca_1994$totalvote))
message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
