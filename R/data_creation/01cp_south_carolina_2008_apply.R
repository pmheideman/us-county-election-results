## Add the 18 missing South Carolina 2008 House county rows from the State Election Commission ENR snapshot (elect_he_cty_sc_2008.rds; 02t_house_sc_2008.R).
## The panel already had 28 of the 46 counties; those are identical to this build (checked in 02t). Gate: panel SC 2008 HE has exactly those 28 rows. Backup in scratchpad backup_sc/. 2026-09-21.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_sc"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_sc.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_sc.rds"))
is_sc <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 45
stopifnot(sum(is_sc & panel$year == 2008) == 28)
new <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_sc_2008.rds")) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 46, !anyNA(new$totalvote))
chk <- panel %>% filter(is_sc, year == 2008) %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
stopifnot(nrow(chk) == 28, all(abs(chk$demovote - chk$demovote.n) < 1e-9 & abs(chk$repuvote - chk$repuvote.n) < 1e-9 & abs(chk$totalvote - chk$totalvote.n) < 0.5))
add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample")); stopifnot(nrow(add) == 18)
p2 <- bind_rows(panel, add[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("added ", nrow(add), " rows; panel rows ", nrow(panel), " -> ", nrow(p2))
