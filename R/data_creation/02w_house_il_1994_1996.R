## Illinois 1994 and 1996 U.S. House by county, from the State Board of Elections "Official Vote" books (scanned, OCR text layer; R/data/county_house_files/'IL_1994 ge_*.pdf', 'IL_1996 ge_*.pdf').
## Per district ("REPRESENTATIVE IN CONGRESS <ORDINAL> CONGRESSIONAL DISTRICT") the book prints each candidate's statewide total and a county table (a county in 2+ districts has a row per district).
## The OCR text was parsed by il_parse_9496.py / il_build_9496.py (R/data/raw_house_county_open_states/illinois_official/) into il_1994_1996_transcription.csv and il_1994_1996_printed_totals.csv;
## every district's county sums equal the printed candidate totals exactly (40 of 40 districts); the few OCR gaps were read from the page images (R/output/il_ocr_corrections_1994_1996.csv).
## This script: county FIPS, names, party labels, long and shares files, acceptance tests, and cross-checks (same county sets as the 1998 State Board files - the 1992 map was in use through 2000 -,
## House total vs the presidential/Senate total in the panel).
## Outputs: R/output/long/he_il_1994.rds, he_il_1996.rds; R/output/elect_he_cty_il_1994.rds, elect_he_cty_il_1996.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "illinois_official")
tr <- read_csv(file.path(D, "il_1994_1996_transcription.csv"), col_types = cols(.default = "c", year = "i", district = "i", votes = "d"))
pt <- read_csv(file.path(D, "il_1994_1996_printed_totals.csv"), col_types = cols(.default = "c", year = "i", district = "i", printed_total = "d")) %>% mutate(party_code = ifelse(is.na(party_code), "", party_code))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "ILLINOIS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 102)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)], party_code = ifelse(is.na(party_code), "", party_code)); stopifnot(!anyNA(tr$county_fips), !anyNA(tr$votes))
## ---- names: proper case with the punctuation the book prints
SPECIAL <- c("RAY LaHOOD" = "Ray LaHood", "STEPHEN DE LA ROSA" = "Stephen De La Rosa", "JESSE L. JACKSON JR" = "Jesse L. Jackson Jr.", "GERALD C. \"JERRY\" WELLER" = "Gerald C. \"Jerry\" Weller",
             "CHARLES \"CHUCK\" MOBLEY" = "Charles \"Chuck\" Mobley", "ELIZABETH ANNE (BETTY) HULL" = "Elizabeth Anne (Betty) Hull", "RONALD (RON) BARTOS" = "Ronald (Ron) Bartos", "LIONEL O. PITTMAN" = "Lionel O. Pittman",
             "WILLIAM O. LIPINSKI" = "William O. Lipinski", "G. DOUGLAS STEPHENS" = "G. Douglas Stephens", "J. DENNIS HASTERT" = "J. Dennis Hastert", "H. DANIEL DRUCK" = "H. Daniel Druck")
nice <- function(z) ifelse(z %in% names(SPECIAL), SPECIAL[z], gsub("\\b([A-Z])\\.", "\\1.", tools::toTitleCase(tolower(z))))
tr$candidate <- nice(tr$candidate); pt$candidate <- nice(pt$candidate)
PARTY <- c(DEM = "Democratic", REP = "Republican", LIB = "Libertarian", L1B = "Libertarian", UIP = "United Independents", REF = "Reform", IND = "Independent", NAT = "Natural Law", SOL = "Illinois Solidarity")
lab <- function(pc) ifelse(pc == "", "Write-In", ifelse(pc %in% names(PARTY), PARTY[pc], pc))
tr <- tr %>% mutate(party_group = case_when(party_code == "DEM" ~ "DEM", party_code == "REP" ~ "REP", TRUE ~ "OTHER"), party = lab(party_code))             # party_group BEFORE the label is set
raw <- tr %>% transmute(year, county_fips, district = sprintf("%02d", district), candidate, party, party_group, votes)
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
## ---- acceptance: county sums == printed totals
ck <- raw %>% group_by(year, district, candidate) %>% summarise(sum = sum(votes), .groups = "drop") %>% full_join(pt %>% transmute(year, district = sprintf("%02d", district), candidate, printed = printed_total), by = c("year", "district", "candidate")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(ck), "| county sums equal the printed candidate total:", sum(ck$ok, na.rm = TRUE), "\n"); print(as.data.frame(ck %>% filter(!ok | is.na(ok)))); stopifnot(all(ck$ok))
## ---- county sets per district vs the 1998 State Board files (same map)
l98 <- readRDS(file.path(LONG_DIR, "he_il_sbe_1998.rds")) %>% distinct(district, county_fips) %>% mutate(county_fips = as.integer(county_fips))
for (y in c(1994, 1996)) { a <- raw %>% filter(year == y) %>% distinct(district, county_fips); dif <- anti_join(a, l98, by = c("district", "county_fips")); dif2 <- anti_join(l98, a, by = c("district", "county_fips"))
  cat(y, ": county-district pairs", nrow(a), "| not in the 1998 map:", nrow(dif), "| in the 1998 map but missing here:", nrow(dif2), "\n"); if (nrow(dif) + nrow(dif2)) { print(as.data.frame(dif)); print(as.data.frame(dif2)) } }
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
stopifnot(!any(panel$sample == "HE" & panel$cty_fips %/% 1000 == 17 & panel$year < 1998))
for (y in c(1994, 1996)) {
  r <- raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes)
  long <- finalize_long(r, paste0("il_", y))
  nm <- r %>% distinct(district, candidate) %>% mutate(k = paste(district, gsub("[^A-Z]", "", toupper(candidate)))); k <- paste(long$district, gsub("[^A-Z]", "", toupper(long$candidate)))
  long$candidate <- nm$candidate[match(k, nm$k)]; stopifnot(!anyNA(long$candidate))                       # finalize_long strips punctuation: restore the book's names
  save_long(long, paste0("he_il_", y))
  shares <- derive_shares(long) %>% transmute(state = "ILLINOIS", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_il_%d.rds", y)))
  rc <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_il_%d.rds", y))); stopifnot(all(rc$pass))
  ref_s <- if (y == 1996) "PE" else "SE"; ref <- panel %>% filter(sample == ref_s, year == y, cty_fips %/% 1000 == 17) %>% select(cty_fips, ref = totalvote)
  if (!nrow(ref)) { ref_s <- "PE"; ref <- panel %>% filter(sample == "PE", year == y - 2, cty_fips %/% 1000 == 17) %>% select(cty_fips, ref = totalvote) }
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(sprintf("%d: counties %d | districts %d | split counties %d | candidates %d | median dem %.3f rep %.3f | House / %s total: min %.2f median %.2f max %.2f (%d counties)\n", y, n_distinct(long$county_fips), n_distinct(long$district),
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), n_distinct(long$district, long$candidate, long$party), median(shares$demovote), median(shares$repuvote), ref_s, min(rr$ratio, na.rm = TRUE), median(rr$ratio, na.rm = TRUE), max(rr$ratio, na.rm = TRUE), sum(!is.na(rr$ratio))))
  print(as.data.frame(rr %>% filter(ratio < 0.6 | ratio > 1.1) %>% select(cty_fips, totalvote, ref, ratio) %>% head(10)))
}
