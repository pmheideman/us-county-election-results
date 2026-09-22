## Indiana U.S. House by county, 2016-2024, from the Election Division's ENR archive JSON (downloaded by 01cs_indiana_enr_download.R):
## Root$OfficeCategory$Regions$Region[] = one entry per congressional district: RegionSummary (statewide district totals per candidate) and Races$Race[] = one entry per COUNTY
## (Jurisdiction FIPS, candidates with TOTAL_VOTES). Counties in 2+ districts appear once per district.
## Checks: county votes per candidate add up to the district's RegionSummary TOTAL; county count per district; House total vs the presidential/Senate total in the panel; comparison with the panel's MEDSL rows.
## Outputs: R/output/long/he_in_enr_<year>.rds, R/output/elect_he_cty_in_enr_<year>.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(jsonlite); library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_enr")
pull <- function(y) {                                     # special elections (Indiana 2022 district 2 unexpired term) are skipped, per docs/DECISIONS.md
  d <- fromJSON(file.path(DIR, sprintf("usrep_%d.json", y)), simplifyVector = FALSE)$Root
  regs <- d$OfficeCategory$Regions$Region; if (!is.null(regs$MAP_JURISDICTIONID)) regs <- list(regs)
  purrr::map_dfr(regs, function(rg) {
    if (!grepl("^[0-9]+$", rg$MAP_JURISDICTION_NAME)) { message("  ", y, ": region '", rg$MAP_JURISDICTION_NAME, "' skipped (not a regular district: special election)"); return(NULL) }
    dist <- sprintf("%02d", as.integer(rg$MAP_JURISDICTION_NAME))
    sm <- rg$RegionSummary$Race$Candidates$Candidate; if (!is.null(sm$NAME_ON_BALLOT)) sm <- list(sm)
    summ <- purrr::map_dfr(sm, function(c) tibble(candidate = c$NAME_ON_BALLOT, party = c$PARTY, printed = as.numeric(c$TOTAL)))
    rc <- rg$Races$Race; if (!is.null(rc$OFFICEID)) rc <- list(rc)
    cty <- purrr::map_dfr(rc, function(r) { cs <- r$Candidates$Candidate; if (!is.null(cs$NAME_ON_BALLOT)) cs <- list(cs)
      purrr::map_dfr(cs, function(c) tibble(county_name = r$Jurisdiction$JURISDICTION_NAME, fips = r$Jurisdiction$FIPS, jtype = r$Jurisdiction$JURISDICTION_TYPE, candidate = c$NAME_ON_BALLOT, party = c$PARTY_ABBREV, votes = as.numeric(c$TOTAL_VOTES))) })
    cty %>% mutate(district = dist) %>% left_join(summ %>% rename(party_s = party), by = "candidate")
  }) %>% mutate(year = y)
}
all <- purrr::map_dfr(c(2016, 2018, 2020, 2022, 2024), pull)
cat("jurisdiction types:", paste(unique(all$jtype), collapse = ", "), "\n"); cat("party codes:", paste(names(table(all$party)), table(all$party), collapse = "; "), "\n")
chk <- all %>% group_by(year, district, candidate, party) %>% summarise(sum = sum(votes), n_cty = n(), printed = first(printed), .groups = "drop") %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(chk), "| county sums equal the district summary total:", sum(chk$ok, na.rm = TRUE), "\n"); print(as.data.frame(chk %>% filter(!ok | is.na(ok))))
print(as.data.frame(all %>% distinct(year, district, fips) %>% count(year, district) %>% tidyr::pivot_wider(names_from = district, values_from = n)))

## ---- long table, shares, acceptance test -----------------------------------------------------------------------------------------------------------------
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", G = "Green", I = "Independent")
raw <- all %>% transmute(year, county_fips = as.integer(fips), district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)   # party_group BEFORE the label overwrites the code
stopifnot(!anyNA(raw$county_fips), !anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_fips)
stopifnot(all(raw$county_fips %in% xw$county_fips), nrow(xw) == 92)
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); panel_in <- pe %>% filter(sample == "HE", cty_fips %/% 1000 == 18)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("in_enr_", y)); save_long(long, paste0("he_in_enr_", y))
  shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_in_enr_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_in_enr_%d.rds", y))); stopifnot(all(r$pass))
  ref_year <- if (y %% 4 == 0) y else NA
  ref <- pe %>% filter(sample == "PE", year == ifelse(y %% 4 == 0, y, y - 2)) %>% select(cty_fips, ref = totalvote)
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  old <- panel_in %>% filter(year == y) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
  dd <- shares %>% inner_join(old, by = "cty_fips") %>% mutate(same = abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 & abs(totalvote - o_tot) < 0.5)
  cat(sprintf("%d: counties %d | median dem %.3f rep %.3f | House/presidential %s total: min %.2f median %.2f max %.2f | panel rows %d: identical %d, differing %d\n", y, n_distinct(long$county_fips), median(shares$demovote), median(shares$repuvote),
              ifelse(y %% 4 == 0, "same-year", "previous-cycle"), min(rr$ratio, na.rm = TRUE), median(rr$ratio, na.rm = TRUE), max(rr$ratio, na.rm = TRUE), nrow(old), sum(dd$same), sum(!dd$same)))
  write.csv(dd %>% filter(!same), file.path(OUTPUT_DIR, sprintf("in_enr_%d_vs_panel_differences.csv", y)), row.names = FALSE)
}
