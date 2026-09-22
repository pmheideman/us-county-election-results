## Add the 3 missing New York 2018 Senate county rows (Jefferson, Niagara, Ulster) from America Votes 33 (elect_se_cty_av33_2018.rds; 02b2_senate_ny_2018_av33.R). 2026-09-21.
## Gate: the keys are absent from the panel; positive shares; everything else unchanged. Backup: backup_pese/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); saveRDS(panel, "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese/elect_cty_final_before_ny_av33.rds")
new <- readRDS(file.path(OUTPUT_DIR, "elect_se_cty_av33_2018.rds")) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 3, all(new$totalvote > 0), all(new$demovote > 0.3), !any(paste(new$year, new$cty_fips) %in% paste(panel$year[panel$sample == "SE"], panel$cty_fips[panel$sample == "SE"])))
p2 <- bind_rows(panel, new[, names(panel)]); stopifnot(nrow(p2) == nrow(panel) + 3, anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0); saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("added 3 rows; panel ", nrow(panel), " -> ", nrow(p2))
