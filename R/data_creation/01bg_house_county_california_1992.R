## California 1992: same manual-transcription approach as 1996 (`01be`), applied to
## `CA1992-sov-complete.pdf`. Unlike 1996, this PDF's embedded text layer is NOT image-only -- it
## has real extracted text -- but that text is badly character-corrupted (garbled digits like
## "23,3815" for a real number, garbled labels like "Dell.." for "Dem", "Ub" for "Lib"). Confirmed
## via a direct spot-check earlier in this project's California work that this corruption reaches
## the DIGITS themselves, not just labels (a district's own county-row sum came out ~2,000 votes off
## its printed total when read from the extracted text) -- so, same as 1996, every number here was
## read directly off each page RENDERED AS AN IMAGE (`pdftoppm -r 220`), never from the extracted
## text, which was used only earlier in this project to roughly locate the section (pages 37-44 of
## 84, "1st Congressional District" through "52nd Congressional District", page footers "26"-"33").
##
## Format: same as 1996/1998/2000 -- a party-code row, county rows, a "District Totals" + "Percent"
## row for multi-county districts (single-county districts print no separate totals row -- the one
## county row IS the total). Full 52 districts (post-1990-census map, same as 1996/1998/2000 --
## unlike 1990's older 45-ish-district map).
##
## **Four districts have NO Republican candidate at all** (CD-18 Condit, CD-31 Martinez, CD-32
## Dixon, CD-37 Tucker -- all safe Democratic seats that year) -- the party-code row for these
## skips straight from "Dem" to the next real party (Lib/P&F) with no "Rep" column in between.
## Encoded these rows with an explicit `0` in the Rep position (rather than omitting the column)
## so the raw CSV's rank-based Dem=col1/Rep=col2 convention (same as 1996) stays valid for every
## row without a per-district exception in the aggregation script.
##
## No large (>5%) unresolved district-total discrepancies found this year (checked every district's
## county-row sum against its printed total using the same practical few-percent tolerance
## established for 1996 -- nothing here needed a targeted re-zoom the way two 1996 districts did).

source(file.path("R", "00_setup.R"))
library(readr)

## Same read approach as 1996's script (`01be`) -- read_csv()'s rectangular-file inference silently
## mishandles this ragged (variable-column-count) CSV, so bypass it: read raw lines, split by comma.
raw_lines <- readLines(
  file.path(PROJECT_ROOT, "R", "data", "county_house_files", "ca_manual_transcription", "1992_raw.csv")
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
## the second (explicit 0 for the 4 no-Republican districts, see header comment).
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

elect_he_cty_ca_1992 <- county_party %>%
  left_join(ca_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "CALIFORNIA", year = 1992L, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ca_1992")

message("CA 1992 House county-level rows built: ", nrow(elect_he_cty_ca_1992), " (of possible 58)")
sanity <- elect_he_cty_ca_1992$repuvote + elect_he_cty_ca_1992$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

missing <- ca_fips %>% filter(!county_fips %in% elect_he_cty_ca_1992$cty_fips)
if (nrow(missing) > 0) {
  message("Missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
} else {
  message("Full 58/58 California counties.")
}

message("Statewide totalvote sum: ", sum(elect_he_cty_ca_1992$totalvote))
message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
