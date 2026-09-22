## Materialize the only panel rows that were patched in place without a source file of their own (found by R/build_provenance.R status == "modified"), so that
## every panel row is an exact copy of a row in some R/output/elect_*_cty_*.rds file and the panel can be ASSEMBLED from source files (R/assemble_panel.R). 2026-09-21.
##   * President 2024, Arizona / Iowa / Vermont (127 counties): 01dc_pe_se_panel_apply.R set them to the CANDIDATE sums of the MEDSL long table (MEDSL's own
##     totalvotes column includes over/under votes); elect_pe_cty_medsl.rds still holds the totalvotes-based values.
##   * House 2024, Oregon (3 counties): 01ca_medsl_fixes_apply.R set them from read_precinct_file(candidate_level_party = TRUE) (a raw party-label misalignment);
##     elect_he_cty_medsl.rds is the plain aggregation.
## Both are the shares derived from the MEDSL long tables (pe_medsl.rds / he_medsl.rds), which the master checks already tie to the panel.
## Outputs: R/output/elect_pe_cty_medsl_cand_2024.rds, R/output/elect_he_cty_medsl_cand_2024.rds (per-year names -> provenance priority 0)
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
mk <- function(long_file, keys, out) {
  d <- derive_shares(readRDS(file.path(LONG_DIR, long_file))) %>% inner_join(keys, by = c("year", "cty_fips", "sample"))
  stopifnot(nrow(d) == nrow(keys))
  chk <- d %>% inner_join(panel, by = c("year", "cty_fips", "sample"), suffix = c("", ".p"))
  stopifnot(all(abs(chk$demovote - chk$demovote.p) < 1e-9 & abs(chk$repuvote - chk$repuvote.p) < 1e-9 & abs(chk$totalvote - chk$totalvote.p) < 0.5))   # equals the panel
  saveRDS(d %>% mutate(state = NA_character_), file.path(OUTPUT_DIR, out)); message(out, ": ", nrow(d), " rows")
}
pe_keys <- panel %>% filter(sample == "PE", year == 2024, cty_fips %/% 1000 %in% c(4, 19, 50)) %>% distinct(year, cty_fips, sample)
he_keys <- tibble(year = 2024, cty_fips = c(41017, 41019, 41025), sample = "HE")
mk("pe_medsl.rds", pe_keys, "elect_pe_cty_medsl_cand_2024.rds")
mk("he_medsl.rds", he_keys, "elect_he_cty_medsl_cand_2024.rds")
