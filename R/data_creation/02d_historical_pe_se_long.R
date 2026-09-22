## "Shares-only" LONG tables for President 1992-1996 and Senate 1990-2014 from Algara & Amlani's county returns (Harvard Dataverse doi:10.7910/DVN/DGUMFI, CC0 1.0; CQ Press / ICPSR).
## The source gives, per county-year, the Democratic and Republican NOMINEES' names and votes and the county total (all candidates) but nothing about other candidates. Each county-year becomes:
##   Democratic nominee (DEM), Republican nominee (REP), and "All other candidates (not itemized in the source)" (OTHER) = county total - D - R (omitted when 0).
## Same filters as 01b: Senate rows of the general election ("G"); special-only state-years are NOT used here (docs/DECISIONS.md: special elections are ignored). President 1992 and 1996; Senate even years 1990-2014.
## Acceptance test: derived shares equal elect_pe_cty_historical.rds / elect_se_cty_historical.rds for the covered keys (special-only keys are reported).
## Outputs: R/output/long/pe_historical.rds, se_historical.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
e <- new.env(); load(file.path(PROJECT_ROOT, "R/data/raw_election_historical/presidential_county_returns_1868_2020.Rdata"), envir = e); pe <- get("pres_elections_release", e)
load(file.path(PROJECT_ROOT, "R/data/raw_election_historical/us_senate_county_returns_1908_2020.Rdata"), envir = e); se <- get(setdiff(ls(e), "pres_elections_release")[1], e)
mk <- function(d, office, yr, src) {
  d <- d %>% filter(!is.na(raw_county_vote_totals), raw_county_vote_totals > 0, year %in% yr, !((as.integer(fips) %/% 1000) %in% c(2, 11, 15)))
  oth <- pmax(d$raw_county_vote_totals - d$democratic_raw_votes - d$republican_raw_votes, 0); stopifnot(all(d$raw_county_vote_totals - d$democratic_raw_votes - d$republican_raw_votes >= 0))
  raw <- bind_rows(
    d %>% transmute(year, county_fips = as.integer(fips), district = NA_character_, candidate = dem_nominee, party = "Democratic", party_group = "DEM", votes = democratic_raw_votes),
    d %>% transmute(year, county_fips = as.integer(fips), district = NA_character_, candidate = rep_nominee, party = "Republican", party_group = "REP", votes = republican_raw_votes),
    d %>% transmute(year, county_fips = as.integer(fips), district = NA_character_, candidate = "All other candidates (not itemized in the source)", party = "Other", party_group = "OTHER", votes = oth) %>% filter(votes > 0))
  raw$candidate[is.na(raw$candidate) | !nzchar(raw$candidate)] <- "Unnamed (name missing in source)"
  finalize_long(raw, src, office = office) }
pe1 <- pe %>% mutate(year = election_year); L1 <- mk(pe1, "president", c(1992, 1996), "algara_amlani"); save_long(L1, "pe_historical")
se1 <- se %>% mutate(year = election_year) %>% filter(election_type == "G"); L2 <- mk(se1, "senate", seq(1990, 2014, 2), "algara_amlani"); save_long(L2, "se_historical")
cat("President rows", nrow(L1), "| Senate rows", nrow(L2), "\n")
r1 <- check_long_vs_source(L1, file.path(OUTPUT_DIR, "elect_pe_cty_historical.rds"), years = c(1992, 1996)); print(r1)
src <- readRDS(file.path(OUTPUT_DIR, "elect_se_cty_historical.rds")) %>% filter(year %in% seq(1990, 2014, 2), !((cty_fips %/% 1000) %in% c(2, 11, 15)))
d2 <- derive_shares(L2); j <- src %>% full_join(d2, by = c("year", "cty_fips", "sample"), suffix = c(".s", ".l"))
cat("Senate: source keys", nrow(src), "| long keys", nrow(d2), "| only in source (special-only state-years):", sum(is.na(j$totalvote.l)), "| only in long:", sum(is.na(j$totalvote.s)), "| mismatched:",
    sum(!is.na(j$totalvote.l) & !is.na(j$totalvote.s) & !(abs(j$demovote.l - j$demovote.s) < 1e-9 & abs(j$repuvote.l - j$repuvote.s) < 1e-9 & abs(j$totalvote.l - j$totalvote.s) < 0.5)), "\n")
print(as.data.frame(j %>% filter(is.na(totalvote.l)) %>% mutate(st = cty_fips %/% 1000) %>% count(year, st) %>% arrange(year, st) %>% head(30)))
