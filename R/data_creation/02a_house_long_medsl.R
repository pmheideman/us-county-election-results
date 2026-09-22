## Candidate-level LONG table for MEDSL U.S. House, 2016-2024 (regular general elections; see docs/DECISIONS.md).
## Uses 01a's exact read_precinct_file() (extracted by parsing the file) with the SAME options as the shares panel's final MEDSL targets
## (01ce_medsl_pseudo_special_apply.R): party fixes, pseudo-row + special-election exclusion, and for NJ and OR 2024 also the blank-party
## fill, external overrides and candidate-level party. Output rows are county x district x candidate x party LINE (a candidate on a fusion
## ticket appears once per line, e.g. New York's Republican and Conservative lines); the app aggregates by candidate for display.
##
## Cleaning done here (display only; party_group and votes are exactly what the shares used):
##   * candidate: the most-voted spelling per (year, state, district, letters-only key), title-cased.
##   * party: the raw party label of the line; a blank label takes the candidate's primary label, else the party group's name.
## Display cleaning (canonical names, party labels) is done by finalize_long() in R/long_helpers.R.
## Acceptance test: shares derived from this table must equal elect_he_cty_medsl.rds row for row (check_long_vs_source).
## Output: R/output/long/he_medsl.rds

source(file.path("R", "00_setup.R"))
library(readr)
source(file.path("R", "long_helpers.R"))
exprs <- parse(file = file.path("R", "data_creation", "01a_election_data_medsl.R"))
keep <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                 as.character(e[[2]]) %in% c("RAW_DIR", "detect_delim", "PSEUDO_RE", "read_precinct_file"), NA)
eval(exprs[keep])

rd <- function(year, full) read_precinct_file(file.path(RAW_DIR, paste0("house_", year, ".raw")), year, party_fallback = TRUE,
        fill_blank_party = TRUE, exclude_pseudo = TRUE, candidate_level_party = full, exclude_special = TRUE, by_candidate = TRUE)
std  <- purrr::map_dfr(c(2016, 2018, 2020, 2022, 2024), rd, full = FALSE)
full <- rd(2024, full = TRUE)
raw <- bind_rows(std %>% mutate(st = as.integer(county_fips) %/% 1000) %>% filter(!(year == 2024 & st %in% c(34, 41))),
                 full %>% mutate(st = as.integer(county_fips) %/% 1000) %>% filter(st %in% c(34, 41))) %>%
  filter(!is.na(county_fips))
message("raw candidate-level rows: ", nrow(raw))

## a handful of rows (e.g. 3 rows / 204 votes in NY 2018) have no candidate name in the source: keep the votes, label them
raw$candidate[is.na(raw$candidate) | !nzchar(trimws(raw$candidate))] <- "Unnamed (name missing in source)"
raw <- raw %>% transmute(year, county_fips, district, candidate, party = party_label, party_group = case_when(party_std == "DEMOCRAT" ~ "DEM", party_std == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER"), votes)
long <- finalize_long(raw, "medsl")
save_long(long, "he_medsl")
message("long rows: ", nrow(long), " | county-years: ", nrow(distinct(long, year, county_fips)), " | candidates: ", nrow(distinct(long, year, state_fips, district, candidate)))

## ---- acceptance test ---------------------------------------------------------------------------------------------------------------------------
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))
print(res)

message("\nsample (a split county and a fusion line):")
print(as.data.frame(long %>% filter(year == 2024, county_fips == 36001) %>% arrange(district, desc(votes)) %>% head(12)))
message("\nparty labels most common:"); print(as.data.frame(long %>% count(party, party_group, sort = TRUE) %>% head(14)))
