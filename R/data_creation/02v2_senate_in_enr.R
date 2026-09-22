## Indiana U.S. Senate by county, 2016, 2018, 2022, 2024, from the Election Division's ENR archive JSON (downloaded by 01cs_indiana_enr_download.R; same archive as 02v_house_in_enr.R).
## Statewide race: Root$OfficeCategory$Regions$Region[] = one region per COUNTY (MAP_FIPS, RegionSummary$Race$Candidates$Candidate[] with NAME_ON_BALLOT, PARTY, TOTAL); StatewideSummary = printed statewide totals.
## Checks: county votes per candidate add up to the statewide candidate total; 92 counties; comparison with the panel's MEDSL rows. Outputs: R/output/long/se_in_enr_<year>.rds, R/output/elect_se_cty_in_enr_<year>.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(jsonlite)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_enr")
lst <- function(x) if (!is.null(x$NAME_ON_BALLOT)) list(x) else x
pull <- function(y) { d <- fromJSON(file.path(DIR, sprintf("ussen_%d.json", y)), simplifyVector = FALSE)$Root
  tot <- purrr::map_dfr(lst(d$StatewideSummary$Race$Candidates$Candidate), function(c) tibble(candidate = c$NAME_ON_BALLOT, printed = as.numeric(c$TOTAL)))
  cty <- purrr::map_dfr(d$OfficeCategory$Regions$Region, function(rg) { rc <- rg$Races$Race; if (!is.null(rc$OFFICEID)) rc <- list(rc); stopifnot(length(rc) == 1)                 # the region's summary repeats the STATEWIDE totals; the county's own votes are in Races$Race
      purrr::map_dfr(lst(rc[[1]]$Candidates$Candidate), function(c) tibble(county_fips = as.integer(rg$MAP_FIPS), candidate = c$NAME_ON_BALLOT, party = c$PARTY_ABBREV, votes = as.numeric(c$TOTAL_VOTES))) })
  list(cty = cty %>% mutate(year = y), tot = tot %>% mutate(year = y)) }
res <- lapply(c(2016, 2018, 2022, 2024), pull); cty <- bind_rows(lapply(res, `[[`, "cty")); tot <- bind_rows(lapply(res, `[[`, "tot"))
ck <- cty %>% group_by(year, candidate) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(tot, by = c("year", "candidate")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(ck), "| county sums equal the statewide totals:", sum(ck$ok, na.rm = TRUE), "| counties:", paste(unique(ck$n_cty), collapse = ","), "\n"); print(as.data.frame(ck %>% filter(!ok | is.na(ok))))
stopifnot(all(ck$ok), all(cty$county_fips %/% 1000 == 18))
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", G = "Green", I = "Independent", O = "Other")
raw <- cty %>% mutate(wi = grepl("\\(W/I\\)", candidate), candidate = trimws(gsub("\\s*\\(W/I\\)", "", candidate))) %>%
  transmute(year, county_fips, district = NA_character_, candidate, party_group = case_when(party == "D" & !wi ~ "DEM", party == "R" & !wi ~ "REP", TRUE ~ "OTHER"), party = ifelse(wi, "Write-In", PARTY[party]), votes)   # party_group BEFORE the label
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("in_enr_", y), office = "senate"); save_long(long, paste0("se_in_enr_", y))
  sh <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(sh, file.path(OUTPUT_DIR, sprintf("elect_se_cty_in_enr_%d.rds", y)))
  stopifnot(all(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_se_cty_in_enr_%d.rds", y)))$pass))
  old <- panel %>% filter(sample == "SE", year == y, cty_fips %/% 1000 == 18) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
  dd <- sh %>% inner_join(old, by = "cty_fips") %>% mutate(same = abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 & abs(totalvote - o_tot) < 0.5)
  cat(sprintf("%d: counties %d | median dem %.3f rep %.3f | panel SE rows %d: identical %d, differing %d\n", y, n_distinct(long$county_fips), median(sh$demovote), median(sh$repuvote), nrow(old), sum(dd$same), sum(!dd$same))) }
