## Fills the pre-2000 (President) and pre-2016 (Senate) gap left by 01a_election_data_medsl.R.
##
## MEDSL's county-level election files only go back to 2000 (President) and 2016 (House/Senate
## precinct files) -- see 01a's header notes. The original paper's Table 1 sample runs 1990-2016
## across all three offices, using the authors' paywalled CQ Press / Leip's Atlas / ICPSR data.
## No free source could be found for county-level HOUSE returns before 2016 (MEDSL's own
## multi-year House series is district-level only, and congressional districts don't map onto
## counties). But a free, CC0-licensed source *does* exist for President and Senate:
##
##   Algara, Carlos & Sharif Amlani. "Partisanship & Nationalization in American Elections:
##   Evidence from Presidential, Senatorial, & Gubernatorial Elections in the U.S. Counties,
##   1872-2020." Electoral Studies (2021). Replication data: Harvard Dataverse,
##   https://doi.org/10.7910/DVN/DGUMFI (CC0 1.0) -- county-level President 1868-2020 and
##   Senate 1908-2020, sourced from CQ Press / ICPSR United States Historical Election Returns.
##
## This script builds elect_pe_cty_historical.rds / elect_se_cty_historical.rds in the same
## (year, cty_fips, sample, demovote, repuvote, totalvote) shape as 01a's elect_pe/se_cty_medsl.rds,
## using RAW_COUNTY_VOTE_TOTALS (all candidates, not just the two-party total) as the vote-share
## denominator to match 01a's convention (MEDSL's own `totalvotes`/summed office totals are
## likewise all-candidate, not two-party).

source(file.path("R", "00_setup.R"))

RAW_HIST_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_election_historical")

load_rdata_obj <- function(path) {
  e <- new.env()
  load(path, envir = e)
  get(ls(e)[1], envir = e)
}

## ---- President, 1868-2020 (one row per county-year already; no dedup needed) ----
pres_hist <- load_rdata_obj(file.path(RAW_HIST_DIR, "presidential_county_returns_1868_2020.Rdata"))

elect_pe_cty_historical <- pres_hist %>%
  filter(!is.na(raw_county_vote_totals), raw_county_vote_totals > 0) %>%
  transmute(
    year = election_year,
    cty_fips = as.integer(fips),
    sample = "PE",
    demovote = democratic_raw_votes / raw_county_vote_totals,
    repuvote = republican_raw_votes / raw_county_vote_totals,
    totalvote = raw_county_vote_totals
  ) %>%
  save_step("elect_pe_cty_historical")

## ---- Senate, 1908-2020 ----
## ~2% of county-years have two Senate contests the same year (a regular "General" election for
## one seat class plus a "Special" election filling a vacancy in another class) -- verified there
## are zero county-years with more than one General-type row in 1980-2020, so preferring
## election_type=="G" (and falling back to whatever's there when only a Special exists) gives
## exactly one row per county-year with no arbitrary tie-breaking needed.
sen_hist <- load_rdata_obj(file.path(RAW_HIST_DIR, "us_senate_county_returns_1908_2020.Rdata"))

sen_has_general <- sen_hist %>%
  group_by(fips, election_year) %>%
  summarise(has_general = any(election_type == "G"), .groups = "drop")

elect_se_cty_historical <- sen_hist %>%
  left_join(sen_has_general, by = c("fips", "election_year")) %>%
  filter(!has_general | election_type == "G") %>%
  filter(!is.na(raw_county_vote_totals), raw_county_vote_totals > 0) %>%
  transmute(
    year = election_year,
    cty_fips = as.integer(fips),
    sample = "SE",
    demovote = democratic_raw_votes / raw_county_vote_totals,
    repuvote = republican_raw_votes / raw_county_vote_totals,
    totalvote = raw_county_vote_totals
  ) %>%
  save_step("elect_se_cty_historical")

## ---- Stitch with MEDSL: historical source fills the gap before MEDSL's own coverage begins,
## MEDSL is kept as-is for the years it already covers (2000+ President, 2016+ House/Senate) so
## none of the already-validated 01a output is second-guessed. House has no historical fill --
## the pre-2016 House gap remains open (see project memory / conversation notes).
pe_medsl <- readRDS(file.path(OUTPUT_DIR, "elect_pe_cty_medsl.rds"))
se_medsl <- readRDS(file.path(OUTPUT_DIR, "elect_se_cty_medsl.rds"))
he_medsl <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))

pe_final <- bind_rows(elect_pe_cty_historical %>% filter(year < 2000), pe_medsl)
se_final <- bind_rows(elect_se_cty_historical %>% filter(year < 2016), se_medsl)

elect_cty_final <- bind_rows(pe_final, se_final, he_medsl) %>%
  filter(!is.na(repuvote), !is.na(demovote), !is.na(cty_fips)) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")
