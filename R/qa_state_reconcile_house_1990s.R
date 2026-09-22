## Extends the House FEC reconciliation (qa_state_reconcile_congress.R) back to 1990, 1992 and 2002 using the FEC's scanned 1990/1992
## books (OCR text, parsed by 01dq_fec_1990_1992_house_parse.py: district TOTALS only reliable at scale, candidate-level rows are lower
## confidence and used only as a secondary D/R check) and the 2002 House workbook (parsed by 01dp_fec_2002_house_senate_parse.py, a
## position-encoded sheet, not a normal table). Coverage is PARTIAL by design (OCR quality and layout quirks limit how many states/
## districts parse cleanly): 1990 covers 48 states / 404 districts, 1992 covers 39 states / ~430 districts, 2002 covers 47 states / 413
## districts, out of 435 (fewer where a state has only one at-large district, e.g. AK/DE/MT/ND/SD/VT/WY are single-district).
## Output: R/output/qa_state_reconcile_house_1990s.csv (district TOTAL only) and a printed D/R candidate-level summary where available.
source(file.path("R", "00_setup.R")); library(readr)
SKIP <- c("AK", "HI", "DC")
rel <- read_csv(file.path(PROJECT_ROOT, "release", "v0.2.0", "us_county_results_long.csv"), col_types = cols(.default = "c", votes = "d", year = "i"), show_col_types = FALSE) %>%
  filter(office == "house", year %in% c(1990, 1992, 2002))
at_large <- rel %>% group_by(year, state_po) %>% summarise(al = all(district %in% c("00", "")), .groups = "drop") %>% filter(al)
uh <- rel %>% mutate(district = ifelse(is.na(district) | district == "", "00", sprintf("%02d", suppressWarnings(as.integer(district))))) %>%
  left_join(at_large %>% mutate(al = TRUE), by = c("year", "state_po")) %>% mutate(district = ifelse(!is.na(al), "00", district)) %>%
  group_by(year, state_po, district) %>% summarise(our_dem = sum(votes[party_group == "DEM"]), our_rep = sum(votes[party_group == "REP"]), our_total = sum(votes), n_counties = n_distinct(county_fips), .groups = "drop")

read_off <- function(csv_path, year) {
  d <- read_csv(csv_path, show_col_types = FALSE, col_types = cols(district = "c")) %>% filter(!state_po %in% SKIP)
  ## a (state, district) key with MORE THAN ONE "Total Votes:" line means the OCR state-header detector missed a state-name transition
  ## somewhere upstream (confirmed: "IOWA" rendered "!OVA" by the OCR, so its candidates and total were attributed to the previous state,
  ## "INDIANA" -- summing would silently produce a wrong combined total, e.g. Indiana district 1 read as 473,133 = 211,824 (real) + 261,309
  ## (Iowa's district 1, mislabeled) sourced from the SAME PDF, not two genuinely different counts). Such keys are DROPPED (become MISSING),
  ## which is a safe failure mode, rather than summed into a confident but wrong number.
  raw_tot <- d %>% filter(is_total) %>% transmute(year = year, state_po, district = sprintf("%02d", suppressWarnings(as.integer(district))), off_total = total) %>% filter(!is.na(off_total))
  dup_keys <- raw_tot %>% count(state_po, district) %>% filter(n > 1)
  if (nrow(dup_keys)) message("  dropping ", nrow(dup_keys), " (state,district) keys with duplicate 'Total Votes:' hits in ", basename(csv_path), " (likely a garbled state-header transition): ", paste(paste0(dup_keys$state_po, "-", dup_keys$district), collapse = ", "))
  tot <- raw_tot %>% anti_join(dup_keys, by = c("state_po", "district"))
  cand <- d %>% filter(!is_total, !is.na(votes)) %>% mutate(cls = case_when(grepl("^D|^Dem", party, ignore.case = TRUE) ~ "D", grepl("^R|^Rep", party, ignore.case = TRUE) ~ "R", TRUE ~ "O")) %>%
    transmute(year = year, state_po, district = sprintf("%02d", suppressWarnings(as.integer(district))), cls, votes)
  list(tot = tot, cand = cand)
}
o90 <- read_off(file.path(PROJECT_ROOT, "R/data/fec_official/fec1990_house.csv"), 1990)
o92 <- read_off(file.path(PROJECT_ROOT, "R/data/fec_official/fec1992_house.csv"), 1992)
o02 <- read_off(file.path(PROJECT_ROOT, "R/data/fec_official/fec2002_house.csv"), 2002)
tots <- bind_rows(o90$tot, o92$tot, o02$tot)
cand <- bind_rows(o90$cand, o92$cand, o02$cand) %>% group_by(year, state_po, district) %>% summarise(off_dem = sum(votes[cls == "D"]), off_rep = sum(votes[cls == "R"]), .groups = "drop")

ch <- tots %>% left_join(cand, by = c("year", "state_po", "district")) %>% left_join(uh, by = c("year", "state_po", "district")) %>%
  mutate(rel_tot = (our_total - off_total) / off_total, rel_dem = ifelse(off_dem > 0, (our_dem - off_dem) / off_dem, NA), rel_rep = ifelse(off_rep > 0, (our_rep - off_rep) / off_rep, NA),
         status_tot = case_when(is.na(our_total) ~ "MISSING", abs(rel_tot) < 0.0005 ~ "ok (<0.05%)", abs(rel_tot) < 0.005 ~ "small (<0.5%)", abs(rel_tot) < 0.02 ~ "check (0.5-2%)", TRUE ~ "BAD (>=2%)"))
write_csv(ch, file.path(OUTPUT_DIR, "qa_state_reconcile_house_1990s.csv"))
cat("districts with a parsed FEC total:", nrow(ch), "| year x state coverage:\n"); print(as.data.frame(ch %>% distinct(year, state_po) %>% count(year)))
cat("\ndistrict TOTAL by year x status:\n"); print(as.data.frame(ch %>% count(year, status_tot) %>% tidyr::pivot_wider(names_from = status_tot, values_from = n, values_fill = 0)))
cat("\nworst mismatches:\n"); print(as.data.frame(ch %>% filter(status_tot %in% c("BAD (>=2%)", "check (0.5-2%)")) %>% mutate(rel_tot = round(rel_tot, 3)) %>%
  select(year, state_po, district, off_total, our_total, rel_tot, n_counties) %>% arrange(desc(abs(rel_tot))) %>% head(40)))
nd <- ch %>% filter(!is.na(off_dem), !is.na(our_dem), off_dem > 0); cat("\ncandidate-level D check (n=", nrow(nd), "): within 2%:", sum(abs(nd$rel_dem) < 0.02, na.rm = TRUE), "\n")
