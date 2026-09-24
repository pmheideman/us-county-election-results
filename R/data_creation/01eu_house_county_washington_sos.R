## Washington U.S. House, county level, 2008 and 2010, from the Secretary of State's election data downloads (https://www.sos.wa.gov/elections/data-research/election-data-and-maps/
## election-results-and-voters-pamphlets/ -> "2008 General Election" / "2010 General Election" Data (.zip); the Election Results Archive page itself
## (washington-state-election-results-archive) offers only a statewide/district-level workbook, no counties, and nothing before 2007 at county level).
## 2008 (was a gap: the OpenElections repo has no 2008 file): R/data/raw_house_county_open_states/washington_archive/2008/2008-general-data/2008GenCumResultsallCounties.xls, first sheet
##   (county x congressional district x candidate, RaceJurisdictionTypeName == "Congressional"; 39 counties, 9 districts) with party labels ("Prefers Democratic Party", "Prefers G.O.P. Party")
##   from 2008GenAllContestsFinal.xls of the same download. Check: county sums equal the district totals printed in 2008GenAllContestsFinal.xls for every candidate.
## 2010 (was partial, 37 of 39 counties): built from the SOS county precinct files by 01ew_washington_2010_sos_precinct.R (all 39 counties; district totals equal the FEC's official Democratic and
##   Republican votes in all 9 districts). The earlier OpenElections-based rows had two misspelled counties dropped and errors in King and Pierce (CD9), Pierce (CD6), Skamania (CD3), Snohomish (CD2).
## Outputs: R/output/long/he_wasos_<year>.rds and R/output/elect_he_cty_wasos_<year>.rds (2008, 2010); folded into the panel by 01ev_washington_sos_apply.R.
source(file.path("R", "data_creation", "02w_common.R"))
library(readxl)
SOS <- file.path(RAW_ROOT, "washington_archive")
wa_fips <- fips_of("WASHINGTON"); stopifnot(nrow(wa_fips) == 39)
nc <- function(x) toupper(trimws(x))
to_group <- function(x) { x <- toupper(trimws(x)); case_when(x %in% c("D", "DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM", x %in% c("R", "REP", "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }

## ---- 2008 ----
w8 <- read_excel(file.path(SOS, "2008/2008-general-data/2008GenCumResultsallCounties.xls"), sheet = 1) %>% filter(RaceJurisdictionTypeName == "Congressional")
a8 <- read_excel(file.path(SOS, "2008/2008-general-data/2008GenAllContestsFinal.xls")) %>% filter(RaceJurisdictionTypeName == "Congressional", CountyName == "Total")
stopifnot(nrow(w8) == 100, n_distinct(w8$CountyName) == 39, nrow(a8) == 18)
party8 <- a8 %>% transmute(candidate = trimws(BallotName), party = case_when(grepl("Democrat", Party) ~ "Democratic", grepl("G\\.?O\\.?P|Republican", Party) ~ "Republican", TRUE ~ trimws(gsub("[()]|Prefers|Party", "", Party))))
chk <- w8 %>% group_by(RaceJurisdictionName, candidate = trimws(BallotName)) %>% summarise(v = sum(Votes), .groups = "drop") %>%
  inner_join(a8 %>% transmute(RaceJurisdictionName, candidate = trimws(BallotName), official = Vote), by = c("RaceJurisdictionName", "candidate"))
stopifnot(nrow(chk) == 18, all(chk$v == chk$official))
cat("2008: county sums equal the printed district totals for all 18 candidates\n")
r8 <- w8 %>% transmute(year = 2008, county = nc(CountyName), district = sub("U.S. Congressional District ", "", RaceJurisdictionName), candidate = trimws(BallotName), votes = as.numeric(Votes)) %>%
  left_join(party8, by = "candidate") %>% mutate(party_group = to_group(party)); stopifnot(!anyNA(r8$party))

## ---- 2010 (from 01ew_washington_2010_sos_precinct.R) ----
r10 <- readRDS(file.path(OUTPUT_DIR, "wa_2010_sos_county_district_candidate.rds")) %>% transmute(year = 2010, county, district = as.character(cong), candidate, party, party_group = to_group(party), votes)
stopifnot(n_distinct(r10$county) == 39)

raw <- bind_rows(r8 %>% select(year, county, district, candidate, party, party_group, votes), r10 %>% select(year, county, district, candidate, party, party_group, votes)) %>%
  inner_join(wa_fips, by = c("county" = "county_name")) %>% select(-county)
stopifnot(all(table(distinct(raw, year, county_fips)$year) == 39))
for (y in c(2008, 2010)) {
  lab <- "wasos"
  long <- finalize_long(raw %>% filter(year == y), paste0(lab, "_", y)); save_long(long, paste0("he_", lab, "_", y))
  shares <- derive_shares(long) %>% transmute(state = "WASHINGTON", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_%s_%d.rds", lab, y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_%s_%d.rds", lab, y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 53, year == 2010)
sh <- derive_shares(raw %>% filter(year == 2010) %>% transmute(year, county_fips, votes, party_group, office = "house")) %>% inner_join(panel %>% select(cty_fips, pd = demovote, pr = repuvote, pt = totalvote), by = "cty_fips")
cat("2010: counties in old panel ", nrow(panel), "; identical to the rebuilt rows: ", sum(abs(sh$demovote - sh$pd) < 1e-9 & abs(sh$repuvote - sh$pr) < 1e-9 & abs(sh$totalvote / sh$pt - 1) < 1e-9), "; max difference in the Democratic share ", round(max(abs(sh$demovote - sh$pd)), 4), "\n", sep = "")
