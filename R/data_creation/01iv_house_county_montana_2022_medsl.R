## Montana U.S. House 2022, county level, from MEDSL's 2022 House precinct file (R/data/raw_election/house_2022.raw). The panel had no Montana 2022 House rows:
## every Montana row in that file has an EMPTY `mode`, which 01a reads as NA, and its mode de-duplication (keep mode == "TOTAL" where a precinct has one)
## then drops all of them. Montana reports one row per precinct and candidate (no mode breakdown), so the rows are summed as they are.
## Checks (stop on failure): no pseudo-candidates or write-in rows; 56 counties; every candidate's statewide district total equals the FEC 2022 figure
## (R/output/fec_congress_rows.rds). Outputs: he_medslmt_2022 long and elect_he_cty_medslmt_2022 (folded in by 01iw_montana_2022_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
raw0 <- read_tsv(file.path(PROJECT_ROOT, "R/data/raw_election/house_2022.raw"), col_types = cols(.default = "c"), show_col_types = FALSE) %>% filter(state_po == "MT")
stopifnot(nrow(raw0) > 0, all(is.na(raw0$mode) | raw0$mode == ""), all(raw0$writein == "FALSE"), all(raw0$special == "FALSE"), all(raw0$stage == "GEN"),
          !any(grepl(PSEUDO_NAME_RE, raw0$candidate, ignore.case = TRUE)))
raw <- raw0 %>% transmute(year = 2022L, county_fips = as.integer(county_fips), district = sprintf("%02d", as.integer(district)), candidate,
                          party_group = case_when(party_simplified == "DEMOCRAT" ~ "DEM", party_simplified == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER"),
                          party = case_when(party_simplified == "DEMOCRAT" ~ "Democratic", party_simplified == "REPUBLICAN" ~ "Republican", TRUE ~ tools::toTitleCase(tolower(party_detailed))),
                          votes = as.numeric(votes)) %>%
  group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop")
stopifnot(n_distinct(raw$county_fips) == 56, all(raw$county_fips %/% 1000 == 30))
fec <- readRDS(file.path(OUTPUT_DIR, "fec_congress_rows.rds")) %>% filter(state_po == "MT", year == 2022, !is.na(last), !is.na(gen)) %>% transmute(district, last = toupper(last), gen)
chk <- raw %>% group_by(district, candidate) %>% summarise(v = sum(votes), .groups = "drop") %>% rowwise() %>%
  mutate(fec = { f <- fec$gen[fec$district == district & vapply(fec$last, function(l) grepl(l, candidate, fixed = TRUE), NA)]; if (length(f) == 1) f else NA_real_ }) %>% ungroup()
print(as.data.frame(chk)); stopifnot(!anyNA(chk$fec), all(chk$v == chk$fec))
long <- finalize_long(raw, "medslmt_2022"); save_long(long, "he_medslmt_2022")
shares <- derive_shares(long) %>% transmute(state = "MONTANA", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 56)
f <- file.path(OUTPUT_DIR, "elect_he_cty_medslmt_2022.rds"); saveRDS(shares, f); r <- check_long_vs_source(long, f); stopifnot(all(r$pass))
message("2022: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
