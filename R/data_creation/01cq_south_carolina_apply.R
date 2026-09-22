## Fold the South Carolina House county results 1990-2006 (elect_he_cty_sc_<year>.rds; 02s_house_sc_1990_1992.R, 02s_house_sc_1994_1998.R, 02s_house_sc_2000_2006.R, from the State Election
## Commission election reports) into the panel. South Carolina had only 2008 and 2012+ rows. 2026-09-21.
## Gate: no SC House rows for 1990-2006 in the panel; afterwards everything outside these 9 SC-year blocks is checked unchanged. Backup in scratchpad backup_sc2/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_sc2"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_sc2.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_sc2.rds"))
is_sc <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 45
stopifnot(all(panel$year[is_sc] >= 2008))
yrs <- seq(1990, 2006, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_sc_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$sample == "HE"), all(new$cty_fips %/% 1000 == 45), nrow(new) == 9 * 46)
p2 <- bind_rows(panel, new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel %>% arrange(year, cty_fips, sample), p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 45 & year <= 2006)) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("added ", nrow(new), " South Carolina House county-years; panel rows ", nrow(panel), " -> ", nrow(p2))
