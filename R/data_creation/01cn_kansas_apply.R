## Fold the official Kansas House county results 1990-2010 (elect_he_cty_ks_<year>.rds; built by 02k_house_ks_1990_1996.R, 02k_house_ks_1998_2004.R, 02k_house_ks_2006_2010.R
## from the Secretary of State's election-statistics books) into the panel. Kansas had no House rows before 2012. 2026-09-21.
## Gate: the panel must have no Kansas House rows for 1990-2010 (only 2012+); afterwards everything outside these 11 Kansas-year blocks is checked to be unchanged. Backup in scratchpad backup_ks/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_ks"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ks.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ks.rds"))
is_ks <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 20
stopifnot(all(panel$year[is_ks] >= 2012))
yrs <- seq(1990, 2010, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_ks_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$sample == "HE"), all(new$cty_fips %/% 1000 == 20), nrow(new) == 11 * 105)
p2 <- bind_rows(panel, new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel %>% arrange(year, cty_fips, sample), p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 20 & year <= 2010)) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("added ", nrow(new), " Kansas House county-years; panel rows ", nrow(panel), " -> ", nrow(p2))
