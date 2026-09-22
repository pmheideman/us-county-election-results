## Assemble the per-source candidate-level long tables (R/output/long/he_<x>.rds) into ONE House long table and a county summary.
##
## Which source supplies each county-year: exactly the one the shares panel used (elect_cty_final_provenance.rds), so that the master
## acceptance test is exact: shares derived from the assembled long table must equal the panel's House rows for every covered key.
## Scope filters (docs/DECISIONS.md): year >= 1990; Alaska (02), Hawaii (15) and DC (11) dropped.
## Reports: panel keys with no long rows yet (by source), and any mismatch against the panel.
## Outputs: R/output/long/house_long_all.rds, R/output/long/house_county_summary.rds, R/output/house_long_coverage.csv

source(file.path("R", "00_setup.R"))
source(file.path("R", "long_helpers.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE")
prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds")) %>% filter(sample == "HE") %>% select(year, cty_fips, source)
prov <- prov %>% filter(year >= 1990, !(cty_fips %/% 1000) %in% c(2, 11, 15))
panel <- panel %>% filter(year >= 1990, !(cty_fips %/% 1000) %in% c(2, 11, 15))

files <- list.files(LONG_DIR, pattern = "^he_.*\\.rds$", full.names = TRUE)
avail <- tibble(file = files, source = sub("^he_", "he_cty_", sub("\\.rds$", "", basename(files))))
need <- prov %>% count(source, name = "panel_keys")
## a long file may cover several shares files (e.g. he_la_sos.rds holds he_cty_la_sos and he_cty_la_sos_2024; he_ky.rds holds the KY files):
## when a source has no file of its own, fall back to the longest existing prefix (strip trailing _<suffix> parts). Rows are always
## restricted to that source's own panel keys below, so the fallback cannot leak other years.
resolve_file <- function(src) { s <- src; while (nzchar(s)) { hit <- avail$file[avail$source == s]; if (length(hit)) return(hit[1]); s2 <- sub("_[^_]+$", "", s); if (s2 == s) break; s <- s2 }; NA_character_ }
cover <- need %>% mutate(file = vapply(source, resolve_file, "")) %>% mutate(has_long = !is.na(file))
message("panel House keys (1990+, 48 states): ", nrow(prov), " from ", nrow(need), " sources; long tables available for ",
        sum(cover$has_long), " sources covering ", sum(cover$panel_keys[cover$has_long]), " keys")

parts <- lapply(cover$file[cover$has_long], function(f) tryCatch(readRDS(f), error = function(e) { message("cannot read ", f); NULL }))
stopifnot(!any(vapply(parts, is.null, NA)))
names(parts) <- cover$source[cover$has_long]
long <- purrr::imap_dfr(parts, function(l, src) {
  l %>% mutate(county_fips = as.integer(county_fips)) %>%
    inner_join(prov %>% filter(source == src) %>% transmute(year, county_fips = cty_fips), by = c("year", "county_fips"))
})
stopifnot(all(long$party_group %in% c("DEM", "REP", "OTHER")))

## Display clean-up shared by every source: non-breaking spaces (they stopped Utah 2014 names from being capitalised, "Rob bishop") and names whose words start in lower case.
long <- long %>% mutate(candidate = gsub("[\u00a0\u2007\u202f]", " ", candidate)) %>%
  mutate(candidate = ifelse(grepl("(^| )[a-z]", candidate) & !grepl("^Unnamed", candidate), pretty_name(candidate), candidate))
## The table BEFORE names are applied is saved too: 03b_candidate_names_wikipedia.R reads THAT one, so re-running it is idempotent
## (it always sees the original placeholder / surname-only names, never names it assigned itself).
save_long(long %>% arrange(year, county_fips, district, desc(votes)), "house_long_raw")
## ---- full candidate names from R/data/raw_election/candidate_name_overrides.csv (03b_candidate_names_wikipedia.R) -------------------------------------
## Only NAMES change. Rows that end up with the same name in a county-district (e.g. Texas "Cuellar" + "Henry Cuellar" fragments) are merged.
ov_file <- file.path(PROJECT_ROOT, "R", "data", "raw_election", "candidate_name_overrides.csv")
if (file.exists(ov_file)) {
  ov <- readr::read_csv(ov_file, col_types = readr::cols(.default = "c")) %>% transmute(year = as.integer(year), state_fips = as.integer(state_fips), district, candidate = candidate_old, candidate_new)
  tot0 <- sum(long$votes); n0 <- nrow(long)
  long <- long %>% left_join(ov, by = c("year", "state_fips", "district", "candidate")) %>% mutate(candidate = dplyr::coalesce(candidate_new, candidate)) %>% select(-candidate_new) %>%
    group_by(across(-votes)) %>% summarise(votes = sum(votes), .groups = "drop")
  stopifnot(abs(sum(long$votes) - tot0) < 1e-6)
  message("name overrides applied: ", nrow(ov), " renames available; rows ", n0, " -> ", nrow(long), " after merging same-name fragments; total votes unchanged")
}
long <- long %>% arrange(year, county_fips, district, desc(votes))
save_long(long, "house_long_all")

## ---- master acceptance test vs the panel (covered keys only) ------------------------------------------------------------------------------
d <- derive_shares(long)
j <- d %>% inner_join(panel, by = c("year", "cty_fips", "sample"), suffix = c(".long", ".panel"))
bad <- j %>% filter(!(abs(demovote.long - demovote.panel) < 1e-9 & abs(repuvote.long - repuvote.panel) < 1e-9 & abs(totalvote.long - totalvote.panel) < 0.5))
message("MASTER CHECK: ", nrow(j) - nrow(bad), " of ", nrow(j), " covered county-years reproduce the panel exactly; mismatched: ", nrow(bad))
if (nrow(bad)) print(as.data.frame(bad %>% mutate(st = cty_fips %/% 1000) %>% count(st, year) %>% arrange(desc(n)) %>% head(15)))
unc <- prov %>% anti_join(d, by = c("year" = "year", "cty_fips" = "cty_fips"))
message("panel keys still WITHOUT long rows: ", nrow(unc), " (", round(100 * nrow(unc) / nrow(prov), 1), "%)")
write.csv(cover %>% select(source, panel_keys, has_long) %>% left_join(unc %>% count(source, name = "keys_without_long"), by = "source") %>%
            mutate(keys_without_long = ifelse(is.na(keys_without_long), 0L, keys_without_long)) %>% arrange(desc(keys_without_long)),
          file.path(OUTPUT_DIR, "house_long_coverage.csv"), row.names = FALSE)

## ---- county summary (cross-district totals by party group) -------------------------------------------------------------------------------------
summ <- long %>% group_by(year, state_fips, county_fips) %>%
  summarise(total_votes = sum(votes), dem_votes = sum(votes[party_group == "DEM"]), rep_votes = sum(votes[party_group == "REP"]),
            other_votes = sum(votes[party_group == "OTHER"]), n_districts = n_distinct(district), .groups = "drop") %>%
  mutate(dem_two_party_share = ifelse(dem_votes + rep_votes > 0, dem_votes / (dem_votes + rep_votes), NA_real_),
         rep_share_of_total = rep_votes / total_votes)
save_long(summ, "house_county_summary")
message("long rows: ", nrow(long), " | county-years: ", nrow(summ), " | split counties (>1 district): ", sum(summ$n_districts > 1, na.rm = TRUE))
