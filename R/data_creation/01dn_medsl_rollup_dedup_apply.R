## Fold in the rebuilt MEDSL House/Senate shares (01a_election_data_medsl.R, rerun in full 2026-09-21) after two fixes found by the FEC reconciliation:
##  (1) a new ROLLUP_PRECINCT_RE filter in read_precinct_file() drops a redundant "rollup" precinct row (its own vote count exactly equals the sum of the
##      candidate's other, real precincts in the same county) -- confirmed doubling: Idaho House+Senate 2022 (43 counties each, "COUNTY TOTAL" precinct),
##      Mississippi House 2022 and Michigan House 2022 (1 county each), Oregon House 2018 (1 county);
##  (2) PSEUDO_RE extended (Times Blank Voted, Rejected Write-Ins, Blank (2), Write-In: Invalid Write-In -- see also 01dm, which already patched these into
##      the panel by filtering the LONG tables directly; this full rebuild reproduces the same numbers from the raw side, so no double-application here).
## Re-sourcing 01a in full also picked up a few small, unrelated pre-existing corrections that simply hadn't been re-triggered since being written (Oregon
## House 2024, 3 counties: Dan Ruby's votes were misclassified LIBERTARIAN vs the candidate-level party fix; Maryland House 2016, 1 county, 3 votes).
## Indiana Senate 2022 shows 4 counties with all-zero raw votes in this file (a genuine MEDSL gap, unrelated to either fix above) -- EXCLUDED here: Indiana
## Senate is sourced from se_in_enr (official ENR data, priority 0), never from MEDSL, so this has no effect on the panel either way.
## Gate: only county-years whose current panel source is elect_he_cty_medsl/elect_se_cty_medsl change; positive dem/rep medians; everything else unchanged. Backup: backup_rollup/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_rollup"
old_he <- readRDS(file.path(bk, "elect_he_cty_medsl.rds")); old_se <- readRDS(file.path(bk, "elect_se_cty_medsl.rds"))
new_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds")); new_se <- readRDS(file.path(OUTPUT_DIR, "elect_se_cty_medsl.rds"))
diff <- function(old, new) old %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c(".o", ".n")) %>%
  filter(is.finite(totalvote.n), totalvote.n > 0, abs(demovote.o - demovote.n) > 1e-9 | abs(repuvote.o - repuvote.n) > 1e-9 | abs(totalvote.o - totalvote.n) > 0.5) %>%
  transmute(year, cty_fips, sample, demovote = demovote.n, repuvote = repuvote.n, totalvote = totalvote.n, state = NA_character_)
new <- bind_rows(diff(old_he, new_he), diff(old_se, new_se))
## Oregon (and New Jersey) 2024 House use candidate_level_party in the LONG table (02a) to fix a raw label misalignment (Dan Ruby mislabeled LIBERTARIAN
## instead of DEMOCRAT in Harney and 2 other counties); that correction is NOT reflected in elect_he_cty_medsl.rds's own plain aggregation (01a never sets
## candidate_level_party for its own house_wide/senate_wide build), so a few OR 2024 counties differ between elect_he_cty_medsl.rds and he_medsl.rds even
## though both are "current". The panel already holds the CORRECT (candidate_level_party) values (01ca_medsl_fixes_apply.R); exclude OR 2024 House here so
## this fold-in does not silently revert that earlier fix back to the plain, mislabeled aggregation.
new <- new %>% filter(!(sample == "HE" & year == 2024 & cty_fips %/% 1000 == 41))
message("changed county-years (excluding the Indiana zero-vote rows): ", nrow(new)); print(as.data.frame(new %>% mutate(st = cty_fips %/% 1000) %>% count(sample, st, year) %>% arrange(desc(n))))
stopifnot(!anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0))
is_new <- function(d) d$sample %in% new$sample & d$year %in% new$year & d$cty_fips %in% new$cty_fips   # coarse pre-filter, exact match below
blk <- panel %>% mutate(.i = row_number()) %>% inner_join(new %>% transmute(year, cty_fips, sample, .flag = TRUE), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(blk) == nrow(new))   # every changed key exists exactly once in the panel
p_src <- prov %>% inner_join(new %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(all(p_src$source %in% c("he_cty_medsl", "se_cty_medsl")))   # gate: only currently-MEDSL-sourced rows are being touched
if (!file.exists(file.path(bk, "elect_cty_final_before_rollup.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_rollup.rds"))
panel2 <- panel; panel2[blk$.i, c("demovote", "repuvote", "totalvote")] <- new[match(paste(blk$year, blk$cty_fips, blk$sample), paste(new$year, new$cty_fips, new$sample)), c("demovote", "repuvote", "totalvote")]
stopifnot(nrow(panel2) == nrow(panel))
unchanged <- setdiff(seq_len(nrow(panel)), blk$.i)
stopifnot(isTRUE(all.equal(panel[unchanged, ] %>% arrange(year, cty_fips, sample), panel2[unchanged, ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
sh <- new %>% mutate(st = cty_fips %/% 1000) %>% group_by(sample) %>% summarise(med_dem = median(demovote), med_rep = median(repuvote), .groups = "drop"); print(as.data.frame(sh)); stopifnot(all(sh$med_dem > 0.1), all(sh$med_rep > 0.1))
saveRDS(panel2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(panel2), " (", nrow(new), " rows updated in place)")
