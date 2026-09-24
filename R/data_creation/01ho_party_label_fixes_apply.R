## Carry R/data/party_label_corrections.csv (applied by finalize_long in every long build) into the existing long files, their shares files and the panel.
## Found 2026-09-24 by checking every district-year with only one major party (807) against the FEC / Clerk of the House candidate lists: 7 nominees
## had the wrong party label (WY 2022, OR 2008, PA 2002, TN 1998, TX 2006, VA 1994, WV 2002). Gate: every correction matches rows; only the affected
## county-years' shares change; county totals do not change; the patched shares rows are re-derived from the patched long rows. Idempotent.
## NOTE: re-running an OLD-style shares builder (01a MEDSL, 01bc Oregon, Pennsylvania/West Virginia 01 scripts) recomputes shares without these fixes:
## re-run this script afterwards.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); source(file.path("R", "apply_helper.R"))
fx <- readr::read_csv(file.path(PROJECT_ROOT, "R/data/party_label_corrections.csv"), show_col_types = FALSE, col_types = readr::cols(.default = "c")) %>%
  mutate(year = as.integer(year), state_fips = STATE_FIPS_OF_PO[state_po], district = norm_district(district), ckey = gsub("[^A-Z]", "", toupper(candidate)))
all_long <- readRDS(file.path(OUTPUT_DIR, "long", "house_long_all.rds"))
fx$src <- sapply(seq_len(nrow(fx)), function(i) { s <- all_long %>% filter(year == fx$year[i], state_fips == fx$state_fips[i], district == fx$district[i],
  gsub("[^A-Z]", "", toupper(candidate)) == fx$ckey[i]) %>% distinct(source) %>% pull(source); stopifnot(length(s) == 1); s })
print(fx %>% select(year, state_po, district, candidate, party_group, src))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); changed <- 0
for (src in unique(fx$src)) {
  lf <- file.path(OUTPUT_DIR, "long", paste0("he_", src, ".rds")); sf <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", src, ".rds"))
  long <- readRDS(lf); fixed <- apply_party_label_corrections(long)
  stopifnot(isTRUE(all.equal(long$votes, fixed$votes)))
  f_src <- fx %>% filter(src == !!src)
  keys <- fixed %>% mutate(ck = gsub("[^A-Z]", "", toupper(candidate))) %>%
    semi_join(f_src, by = c("year", "state_fips", "district", "ck" = "ckey")) %>% distinct(year, county_fips)
  stopifnot(nrow(keys) > 0)  # idempotent: keys come from the corrected candidates, whether or not this run changes them
  n_hit <- sum(long$party_group != fixed$party_group | long$party != fixed$party)
  sh <- readRDS(sf)
  new_sh <- derive_shares(fixed %>% semi_join(keys, by = c("year", "county_fips")))
  old <- sh %>% semi_join(new_sh, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips); new_sh <- new_sh %>% arrange(year, cty_fips)
  stopifnot(nrow(old) == nrow(new_sh), all(abs(old$totalvote - new_sh$totalvote) < 1e-6))   # county totals unchanged
  sh2 <- sh %>% rows_update(new_sh %>% select(year, cty_fips, sample, demovote, repuvote), by = c("year", "cty_fips", "sample"))
  stopifnot(nrow(sh2) == nrow(sh), sum(sh2$demovote != sh$demovote | sh2$repuvote != sh$repuvote, na.rm = TRUE) <= nrow(new_sh), identical(is.na(sh2$demovote), is.na(sh$demovote)))
  saveRDS(fixed, lf); saveRDS(sh2, sf)
  pk <- panel %>% semi_join(new_sh, by = c("year", "cty_fips", "sample"))
  stopifnot(nrow(pk) == nrow(new_sh))
  panel <- panel %>% rows_update(new_sh %>% select(year, cty_fips, sample, demovote, repuvote), by = c("year", "cty_fips", "sample"))
  changed <- changed + nrow(new_sh); message(src, ": ", n_hit, " long rows relabelled this run, ", nrow(new_sh), " county-years re-derived")
}
saveRDS(panel, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel county-years updated: ", changed)
