## Ohio U.S. House, county level, 1996 / 2000 / 2002, from the Ohio Secretary of State's own results pages as archived by the Wayback Machine (October 2004 captures of
## www.sos.state.oh.us/sos/results/; parsed by 01fa_ohio_sos_archive_parse.py into R/data/raw_house_county_open_states/ohio_sos_archive/us_house_county_district.csv).
## The pages give, per congressional district, the votes of every candidate in every county in that district (a county split between districts appears in each; the county total is the sum),
## with the printed district TOTAL row tying to the county rows (checked in the parser). 1990, 1992, 1994 and 1998 pages in the archive are statewide summaries only (no House by county), and the
## 2004 House page's data frame is not archived, so only these three years can be built here.
## Party: the code in parentheses after each candidate: D -> DEM, R -> REP, everything else (N, L, I, WI, ...) OTHER. Write-in columns of 2002 (WI) are kept as OTHER candidates.
## Outputs: R/output/long/he_ohsosarc_<year>.rds and R/output/elect_he_cty_ohsosarc_<year>.rds for APPLY_YEARS; prints a comparison with the panel rows that already exist.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/ohio_sos_archive/us_house_county_district.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), county = toupper(trimws(county)))
oh <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "OHIO", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(county = toupper(trimws(county_name)))
stopifnot(nrow(oh) == 88)
unk <- d %>% filter(!county %in% oh$county) %>% count(year, county); cat("county names not matching the Ohio crosswalk:\n"); print(as.data.frame(unk))
ALIAS <- c(`CUYAHOGA TOTAL` = "CUYAHOGA")          # 2002 CD10 and CD11 label their (whole) Cuyahoga portion "Cuyahoga Total"
d <- d %>% mutate(county = ifelse(county %in% names(ALIAS), ALIAS[county], county))
stopifnot(all(d$county %in% oh$county))
raw <- d %>% inner_join(oh %>% select(county, county_fips), by = "county") %>%
  mutate(party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "" ~ "Other", TRUE ~ party_code)) %>%
  group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
chk <- raw %>% distinct(year, district, candidate, party_group) %>% count(year, district, party_group) %>% filter(party_group != "OTHER", n > 1); cat("district-years with more than one DEM or REP candidate:", nrow(chk), "\n"); print(as.data.frame(chk))
APPLY_YEARS <- c(1996, 2002)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("ohsosarc_", y)); shares <- derive_shares(long) %>% transmute(state = "OHIO", year, cty_fips, sample, demovote, repuvote, totalvote)
  if (y %in% APPLY_YEARS) { save_long(long, paste0("he_ohsosarc_", y)); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ohsosarc_%d.rds", y)))
    r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ohsosarc_%d.rds", y))); stopifnot(all(r$pass)) }
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 39)
sh_all <- derive_shares(raw %>% transmute(year, county_fips, votes, party_group, office = "house")) %>% rename(b_dem = demovote, b_rep = repuvote, b_tot = totalvote)
cmp <- sh_all %>% left_join(panel %>% select(year, cty_fips, demovote, repuvote, totalvote), by = c("year", "cty_fips")) %>% group_by(year) %>%
  summarise(oh_counties = n(), in_panel = sum(!is.na(totalvote)), identical = sum(!is.na(totalvote) & abs(b_dem - demovote) < 1e-9 & abs(b_rep - repuvote) < 1e-9 & abs(b_tot - totalvote) < 0.5),
            within_1pct_total = sum(!is.na(totalvote) & abs(b_tot / totalvote - 1) < 0.01), max_dem_diff = suppressWarnings(round(max(abs(b_dem - demovote), na.rm = TRUE), 4)), .groups = "drop")
cat("\nOhio archive county-year rows vs existing panel rows:\n"); print(as.data.frame(cmp))
