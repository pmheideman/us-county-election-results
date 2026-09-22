## Maine and Mississippi 2004 PRESIDENT by county, replacing MEDSL's incomplete 2004 rows (2026-09-21).
## The FEC reconciliation (R/qa_state_reconcile_pe.R) showed MEDSL's 2004 county counts 7.3% (Maine) and 3.2% (Mississippi) below the certified state totals, in nearly every county
## (Maine: 15 of 16 counties low, Cumberland -17%; Mississippi: 78 of 82 counties low, Oktibbeha 9,742 vs 16,279).
## Source: the by-county tables of Wikipedia's "2004 United States presidential election in Maine / Mississippi" (secondary; the Mississippi table cites the Mississippi Secretary of State's Amended Certification,
## 22 March 2005, which is saved here as ms_2004_cert_pres.pdf and was checked by eye for Bolivar and Oktibbeha). Acceptance test: the county sums equal the FEC's certified statewide Kerry, Bush and total votes exactly.
## Rows per county: Kerry (DEM), Bush (REP), and "All other candidates (not itemized in the source)" = total - Kerry - Bush.
## Outputs: R/output/long/pe_me_2004.rds, pe_ms_2004.rds; R/output/elect_pe_cty_me_2004.rds, elect_pe_cty_ms_2004.rds
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/ms_me_2004/pres_2004_county_me_ms.csv"), col_types = cols(.default = "c", kerry = "d", bush = "d", other = "d", total = "d"), show_col_types = FALSE)
FEC <- tibble::tribble(~state_po, ~kerry, ~bush, ~total, "ME", 396842, 330201, 740752, "MS", 458094, 684981, 1152145)   # FEC "Federal Elections 2004", state results (R/data/fec_official/2004pres.xls)
sums <- d %>% group_by(state_po) %>% summarise(kerry = sum(kerry), bush = sum(bush), total = sum(total), n = n(), .groups = "drop"); print(as.data.frame(sums))
stopifnot(all(sums$kerry == FEC$kerry[match(sums$state_po, FEC$state_po)]), all(sums$bush == FEC$bush[match(sums$state_po, FEC$state_po)]), all(sums$total == FEC$total[match(sums$state_po, FEC$state_po)]), sums$n[sums$state_po == "ME"] == 16, sums$n[sums$state_po == "MS"] == 82)
stopifnot(all(d$kerry + d$bush + d$other == d$total))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state_po %in% c("ME", "MS"), year == 2020, !is.na(county_fips)) %>% distinct(state_po, county_name, county_fips)
norm <- function(z) gsub("[^a-z]", "", tolower(z)); xw <- xw %>% mutate(key = paste(state_po, norm(county_name))); d <- d %>% mutate(key = paste(state_po, norm(county)), county_fips = as.integer(xw$county_fips[match(key, xw$key)]))
stopifnot(!anyNA(d$county_fips), !anyDuplicated(d$county_fips))
raw <- bind_rows(d %>% transmute(state_po, year = 2004L, county_fips, district = NA_character_, candidate = "John F. Kerry", party = "Democratic", party_group = "DEM", votes = kerry),
                 d %>% transmute(state_po, year = 2004L, county_fips, district = NA_character_, candidate = "George W. Bush", party = "Republican", party_group = "REP", votes = bush),
                 d %>% transmute(state_po, year = 2004L, county_fips, district = NA_character_, candidate = "All other candidates (not itemized in the source)", party = "Other", party_group = "OTHER", votes = other))
for (st in c("ME", "MS")) {
  r <- raw %>% filter(state_po == st) %>% select(-state_po); long <- finalize_long(r, paste0(tolower(st), "_2004"), office = "president"); save_long(long, paste0("pe_", tolower(st), "_2004"))
  sh <- derive_shares(long) %>% transmute(state = ifelse(st == "ME", "MAINE", "MISSISSIPPI"), year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(sh, file.path(OUTPUT_DIR, paste0("elect_pe_cty_", tolower(st), "_2004.rds")))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, paste0("elect_pe_cty_", tolower(st), "_2004.rds"))) %>% select(source, keys_source, matched, pass))
}
