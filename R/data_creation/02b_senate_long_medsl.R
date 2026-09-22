## Candidate-level LONG table for MEDSL U.S. Senate, 2016-2024 (regular general elections; special elections excluded per docs/DECISIONS.md).
## Same construction as 02a_house_long_medsl.R (01a's exact read_precinct_file() with the same options as the panel's final MEDSL targets: party fixes, pseudo-row and special-election
## exclusion, and for NJ and OR 2024 the blank-party fill / candidate-level party). District is blank (statewide office).
## Acceptance test: shares derived from this table must equal elect_se_cty_medsl.rds row for row. Output: R/output/long/se_medsl.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
exprs <- parse(file = file.path("R", "data_creation", "01a_election_data_medsl.R"))
keep <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                 as.character(e[[2]]) %in% c("RAW_DIR", "detect_delim", "PSEUDO_RE", "read_precinct_file"), NA)
eval(exprs[keep])
rd <- function(year, full) read_precinct_file(file.path(RAW_DIR, paste0("senate_", year, ".raw")), year, party_fallback = TRUE,
        fill_blank_party = TRUE, exclude_pseudo = TRUE, candidate_level_party = full, exclude_special = TRUE, by_candidate = TRUE)
std  <- purrr::map_dfr(c(2016, 2018, 2020, 2022, 2024), rd, full = FALSE)
full <- rd(2024, full = TRUE)
raw <- bind_rows(std %>% mutate(st = as.integer(county_fips) %/% 1000) %>% filter(!(year == 2024 & st %in% c(34, 41))),
                 full %>% mutate(st = as.integer(county_fips) %/% 1000) %>% filter(st %in% c(34, 41))) %>% filter(!is.na(county_fips))
message("raw candidate-level rows: ", nrow(raw))
raw$candidate[is.na(raw$candidate) | !nzchar(trimws(raw$candidate))] <- "Unnamed (name missing in source)"
raw <- raw %>% transmute(year, county_fips, district = NA_character_, candidate, party = party_label, party_group = case_when(party_std == "DEMOCRAT" ~ "DEM", party_std == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER"), votes)
long <- finalize_long(raw, "medsl", office = "senate"); long$stage[long$year == 2022 & long$state_fips == 13] <- "runoff"     # Georgia 2022: the December runoff is the decisive round (01a decisive_round)
save_long(long, "se_medsl")
message("long rows: ", nrow(long), " | county-years: ", nrow(distinct(long, year, county_fips)), " | candidates: ", nrow(distinct(long, year, state_fips, candidate)))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_se_cty_medsl.rds")))
print(as.data.frame(long %>% count(party, party_group, sort = TRUE) %>% head(10)))
