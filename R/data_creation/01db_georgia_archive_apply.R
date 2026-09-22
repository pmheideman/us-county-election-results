## Fold the Georgia Secretary of State archived results 1990-1998 (elect_he_cty_ga_archive_<year>.rds; 02y_house_ga_archive.R) into the panel. Georgia had House rows from 2000 only. 2026-09-21.
## Gate: no Georgia House rows before 2000; positive dem/rep medians; everything else unchanged. Backup: backup_ga/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_ga"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ga.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ga.rds"))
stopifnot(all(panel$year[panel$sample == "HE" & panel$cty_fips %/% 1000 == 13] >= 2000))
yrs <- seq(1990, 1998, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_ga_archive_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == 13), nrow(new) == 5 * 159)
sh <- new %>% group_by(year) %>% summarise(med_dem = median(demovote), med_rep = median(repuvote), .groups = "drop"); print(as.data.frame(sh)); stopifnot(all(sh$med_dem > 0.15), all(sh$med_rep > 0.15))
p2 <- bind_rows(panel, new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel %>% arrange(year, cty_fips, sample), p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 13 & year <= 1998)) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("added ", nrow(new), " Georgia House county-years; panel rows ", nrow(panel), " -> ", nrow(p2))
