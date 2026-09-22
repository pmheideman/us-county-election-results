## Fold the official Minnesota House county results 1990-2010 (elect_he_cty_mn_<year>.rds; built by 02q_house_mn_1990_1998.R and 02q_house_mn_2000_2010.R from the Secretary of State
## election books) into the panel. Minnesota had no House rows before 2012. 2026-09-21.
## Gate: no Minnesota House rows for 1990-2010 in the panel; afterwards everything outside these 11 Minnesota-year blocks is checked unchanged. Backup in scratchpad backup_mn/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_mn"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_mn.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_mn.rds"))
is_mn <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 27
stopifnot(all(panel$year[is_mn] >= 2012))
yrs <- seq(1990, 2010, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_mn_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$sample == "HE"), all(new$cty_fips %/% 1000 == 27), nrow(new) == 11 * 87)
p2 <- bind_rows(panel, new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel %>% arrange(year, cty_fips, sample), p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 27 & year <= 2010)) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("added ", nrow(new), " Minnesota House county-years; panel rows ", nrow(panel), " -> ", nrow(p2))
