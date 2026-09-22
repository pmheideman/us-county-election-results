## South Carolina U.S. House by county, general elections 1994, 1996 and 1998, from the State Election Commission election reports
## (R/data/county_house_files/SC_Election_Report_1994-1995.pdf pp.33-34, SC_Election_Report_1995-1996.pdf pp.44-45, SC_Election_Report_1997-1998.pdf pp.46-47):
## typed computer printouts, scanned. The OCR text layer is scrambled (county names and number columns separate), so the page images were READ BY HAND into
## R/data/raw_house_county_open_states/south_carolina_official/sc_1994_1998_transcription.csv (built by build_sc_1994_1998_transcription.py, which also asserts every
## district against the printed STATE TOTAL row; the printed totals are in sc_1994_1998_printed_totals.csv).
## Blocks: 'REPRESENTATIVE IN CONGRESS DISTRICT 001..006', one row per county (46 counties; splits: Aiken D2/D3, Laurens D3/D4, Beaufort D2/D6, Berkeley D1/D6,
## Calhoun D2/D6, Charleston D1/D6, Colleton D2/D6, Darlington D5/D6, Dorchester D1/D6, Lee D5/D6, Orangeburg D2/D6, Richland D2/D6, Sumter D5/D6).
## Party codes as printed: R, D, L, NL Natural Law, NP (1998 D1 Innella; Wikipedia: Natural Law), PT Patriot, RF Reform (Peter Ashy ran on both lines, 950 + 804), W write-in.
## Named write-in candidates (1996 D3 Bill Ramsay, 1998 D3 Ron Gilreath) have party code W. DEM group only for D, REP group only for R, everything else OTHER.
## Checks here: county sums == printed totals for every candidate; county counts per district; House total vs the nearest presidential total in the panel.
## Outputs: R/output/long/he_sc_<year>.rds, R/output/elect_he_cty_sc_<year>.rds for 1994, 1996, 1998 (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_carolina_official")
tr <- read_csv(file.path(DIR, "sc_1994_1998_transcription.csv"), col_types = cols(district = "c", county = "c", candidate = "c", party_code = "c", votes = "i"))
pt <- read_csv(file.path(DIR, "sc_1994_1998_printed_totals.csv"), col_types = cols(district = "c", candidate = "c", party_code = "c"))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "SOUTH CAROLINA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 46)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(tr$county_fips))

## ---- verification 1: county sums == printed STATE TOTAL for every candidate line ----------------------------------------------------------------------
cmp <- tr %>% group_by(year, district, candidate, party_code) %>% summarise(county_sum = sum(votes), .groups = "drop") %>% left_join(pt, by = c("year", "district", "candidate", "party_code")) %>% mutate(diff = county_sum - printed_total)
stopifnot(!anyNA(cmp$printed_total), all(cmp$diff == 0)); cat("ALL", nrow(cmp), "candidate lines tie exactly to the printed STATE TOTAL rows\n")

PARTY <- c(R = "Republican", D = "Democratic", L = "Libertarian", NL = "Natural Law", NP = "Natural Law", PT = "Patriot", RF = "Reform", W = "Write-In")
raw <- tr %>% filter(!(votes == 0)) %>% transmute(year, county_fips, district, candidate, party = PARTY[party_code], party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes)
stopifnot(!anyNA(raw$party))
NAMES <- c("James (Jim) Clyburn" = "James E. \"Jim\" Clyburn")       # punctuation is stripped by finalize_long(); restored below
for (y in c(1994L, 1996L, 1998L)) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(-year) %>% mutate(year = y), paste0("sc_", y))
  long$candidate <- gsub("\\bJr\\b\\.?$", "Jr.", long$candidate); long$candidate[toupper(long$candidate) == "WRITE-IN"] <- "Write-In"
  ## names as printed lose their punctuation in finalize_long: restore the ones that need it
  fix <- c("Floyd D Spence" = "Floyd D. Spence", "James E Bryan Jr." = "James E. Bryan Jr.", "Jerry L Fowler" = "Jerry L. Fowler", "Joseph F Innella" = "Joseph F. Innella", "Maurice T Raiford" = "Maurice T. Raiford",
           "Linda L Pennington" = "Linda L. Pennington", "Darrell E Curry" = "Darrell E. Curry", "C Faye Walters" = "C. Faye Walters", "Larry L Bigham" = "Larry L. Bigham", "John M Spratt" = "John M. Spratt",
           "P G Joshi" = "P. G. Joshi", "Savita P Joshi" = "Savita P. Joshi", "George C Taylor" = "George C. Taylor", "Peter J Ashy" = "Peter J. Ashy", "James E Clyburn" = "James E. Clyburn",
           "James Jim Clyburn" = "James E. \"Jim\" Clyburn", "James E Jim Clyburn" = "James E. \"Jim\" Clyburn")
  hit <- long$candidate %in% names(fix); long$candidate[hit] <- fix[long$candidate[hit]]
  save_long(long, paste0("he_sc_", y))
  shares <- derive_shares(long) %>% transmute(state = "SOUTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_sc_%d.rds", y)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_sc_%d.rds", y))) %>% select(source, keys_source, matched, mismatched, pass))
  cat(y, ": candidates", n_distinct(long$district, long$candidate, long$party), "| districts", n_distinct(long$district), "| counties", n_distinct(long$county_fips), "| split counties",
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "\n")
  print(as.data.frame(long %>% group_by(district) %>% summarise(counties = n_distinct(county_fips), votes = sum(votes))))
  pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == y - ifelse(y == 1996, 0, ifelse(y == 1994, 2, 2))) %>% select(cty_fips, pe = totalvote)
  r <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
  cat("House", y, "/ presidential", ifelse(nrow(pe), unique(y - ifelse(y == 1996, 0, 2)), NA), "total: n", sum(!is.na(r$ratio)), "min", round(min(r$ratio, na.rm = TRUE), 2), "median", round(median(r$ratio, na.rm = TRUE), 2), "max", round(max(r$ratio, na.rm = TRUE), 2), "\n")
}
sc_he <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 45, year %in% c(1994, 1996, 1998))
cat("SC House rows already in the panel for 1994/1996/1998:", nrow(sc_he), "\n")
