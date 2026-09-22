## Assemble the President and Senate LONG tables (1990+, 48 states) from the per-source long tables, choosing for each county-year the source the shares panel used
## (elect_cty_final_provenance.rds: pe_cty_historical / pe_cty_medsl / se_cty_historical / se_cty_medsl), then run the master check against the panel.
## Outputs: R/output/long/pe_long_all.rds, se_long_all.rds, pe_se_long_coverage.csv
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
scope <- function(d) d %>% filter(year >= 1990, year %% 2 == 0, !((cty_fips %/% 1000) %in% c(2, 11, 15)))
one <- function(sample_code, office, files) {
  pv <- scope(prov %>% filter(sample == sample_code)); pn <- scope(panel %>% filter(sample == sample_code))
  parts <- purrr::imap_dfr(files, function(f, src) readRDS(file.path(LONG_DIR, f)) %>% mutate(county_fips = as.integer(county_fips)) %>% inner_join(pv %>% filter(source == src) %>% transmute(year, county_fips = cty_fips), by = c("year", "county_fips")))
  d <- derive_shares(parts); j <- d %>% inner_join(pn, by = c("year", "cty_fips", "sample"), suffix = c(".long", ".panel"))
  bad <- j %>% filter(!(abs(demovote.long - demovote.panel) < 1e-9 & abs(repuvote.long - repuvote.panel) < 1e-9 & abs(totalvote.long - totalvote.panel) < 0.5))
  unc <- pn %>% anti_join(d, by = c("year", "cty_fips"))
  message(toupper(office), " MASTER CHECK: ", nrow(j) - nrow(bad), " of ", nrow(j), " covered county-years reproduce the panel exactly; mismatched: ", nrow(bad), " | panel keys without long rows: ", nrow(unc))
  if (nrow(bad)) print(as.data.frame(bad %>% mutate(st = cty_fips %/% 1000) %>% count(st, year) %>% arrange(desc(n)) %>% head(10)))
  if (nrow(unc)) print(as.data.frame(unc %>% mutate(st = cty_fips %/% 1000) %>% count(year, st) %>% arrange(year, st) %>% head(12)))
  parts }
extra_pe <- list.files(LONG_DIR, "^pe_[a-z0-9_]+\\.rds$"); extra_pe <- extra_pe[!extra_pe %in% c("pe_long_all.rds", "pe_historical.rds", "pe_medsl.rds")]; names(extra_pe) <- sub("^pe_", "pe_cty_", sub("\\.rds$", "", extra_pe))   # state-specific official builds (e.g. pe_me_2004.rds -> pe_cty_me_2004)
pe <- one("PE", "president", c(pe_cty_historical = "pe_historical.rds", pe_cty_medsl = "pe_medsl.rds", pe_cty_medsl_cand_2024 = "pe_medsl.rds", extra_pe))   # _cand_2024: 01do_medsl_candidate_sum_sources.R; save_long(pe, "pe_long_all")
extra <- list.files(LONG_DIR, "^se_[a-z0-9_]+\\.rds$"); extra <- extra[!extra %in% c("se_long_all.rds", "se_historical.rds", "se_medsl.rds")]; names(extra) <- sub("^se_", "se_cty_", sub("\\.rds$", "", extra))      # state-specific official builds (e.g. se_in_enr_2018.rds -> se_cty_in_enr_2018)
se <- one("SE", "senate", c(se_cty_historical = "se_historical.rds", se_cty_medsl = "se_medsl.rds", extra)); save_long(se, "se_long_all")
message("President long rows: ", nrow(pe), " | county-years: ", nrow(distinct(pe, year, county_fips)), " | Senate long rows: ", nrow(se), " | county-years: ", nrow(distinct(se, year, county_fips)))
