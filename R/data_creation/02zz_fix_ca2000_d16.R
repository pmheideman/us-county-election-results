## *** OBSOLETE 2026-09-20: 02c_california_1998_2000_rebuild.R reproduces these Santa Clara 2000 CD-16 values directly; this script is now a no-op (it detects the corrected state). ***
##
## Correction for California 2000, Congressional District 16 (Santa Clara County), found by the long-table acceptance test (West fork):
## 01bd's parser took the wrong header row for this one district block, read only the first 2 of 4 vote columns and attached the wrong party
## text ("Thayn"/"Umphress"), so Lofgren's 115,118 votes counted as neither party and 7,415 votes (Umphress, Klein) were missing.
## Source: CA Secretary of State 2000 Statement of Vote (R/data/county_house_files/CA2000-sov-complete.pdf, "16th Congressional District";
## the statewide summary page prints the same totals). "Votes not cast in race" (12,742) is not a candidate vote and is excluded.
## This is a POST-STEP: re-running 01bd or 02w_house_long_california_historical.R reproduces the original error, so re-run THIS script after them.
## Patches: R/output/long/he_ca_historical.rds, R/output/elect_he_cty_ca_historical.rds, R/output/elect_cty_final.rds (Santa Clara 2000 only).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
FIPS <- 6085L; YEAR <- 2000L
correct <- tibble(candidate = c("Zoe Lofgren", "Horace \"Gene\" Thayn", "Dennis Michael Umphress", "Edward J. Klein"),
                  party = c("Democratic", "Republican", "Libertarian", "Natural Law"),
                  party_group = c("DEM", "REP", "OTHER", "OTHER"), votes = c(115118, 37213, 4742, 2673))
lf <- file.path(LONG_DIR, "he_ca_historical.rds"); long <- readRDS(lf)
old <- long %>% filter(year == YEAR, county_fips == FIPS, district == "16")
if (nrow(old) == 4 && all(old$party_group == "OTHER" | old$party_group %in% c("DEM", "REP")) && identical(sort(old$votes), sort(correct$votes))) {
  message("CA 2000 D16 already corrected"); quit(save = "no") }
stopifnot(nrow(old) == 2, sum(old$votes) == 115118 + 37213)          # the known bad state: two rows, two mislabeled columns
new <- correct %>% transmute(year = YEAR, office = "house", state_fips = 6L, county_fips = FIPS, district = "16", stage = "general",
                             candidate, party, party_group, votes, source = unique(long$source)[1])
long2 <- bind_rows(long %>% anti_join(old, by = c("year", "county_fips", "district", "candidate")), new)
stopifnot(!anyNA(long2$votes))
sh <- derive_shares(long2) %>% filter(year == YEAR, cty_fips == FIPS)
message(sprintf("Santa Clara 2000 shares: dem %.4f rep %.4f total %s", sh$demovote, sh$repuvote, format(sh$totalvote, big.mark = ",")))
for (f in c(file.path(OUTPUT_DIR, "elect_he_cty_ca_historical.rds"), file.path(OUTPUT_DIR, "elect_cty_final.rds"))) {
  x <- readRDS(f); i <- which(x$sample == "HE" & x$year == YEAR & x$cty_fips == FIPS); stopifnot(length(i) == 1)
  message(basename(f), " before: dem ", round(x$demovote[i], 4), " rep ", round(x$repuvote[i], 4), " total ", x$totalvote[i])
  x$demovote[i] <- sh$demovote; x$repuvote[i] <- sh$repuvote; x$totalvote[i] <- sh$totalvote; saveRDS(x, f)
}
saveRDS(long2 %>% arrange(year, county_fips, district, desc(votes)), lf)
