## Fold the Indiana ENR-archive U.S. Senate county results 2016, 2018, 2022, 2024 (elect_se_cty_in_enr_<year>.rds; 02v2_senate_in_enr.R) into the panel, replacing the MEDSL Senate rows for Indiana
## (MEDSL had 92 / 51 / 35 / 92 counties and 59 / 44 / 1 / 35 differing rows: the same problems as its House rows). 2026-09-21.
## Gate: every existing Indiana SE row 2016-2024 is from MEDSL (provenance); positive dem/rep medians; everything else unchanged. Backup: backup_pese/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
saveRDS(panel, "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese/elect_cty_final_before_in_senate.rds")
yrs <- c(2016, 2018, 2022, 2024); is_blk <- panel$sample == "SE" & panel$cty_fips %/% 1000 == 18 & panel$year %in% yrs
pv <- prov %>% filter(sample == "SE", cty_fips %/% 1000 == 18, year %in% yrs); stopifnot(all(pv$source == "se_cty_medsl"), nrow(pv) == sum(is_blk))
new <- purrr::map_dfr(sprintf("elect_se_cty_in_enr_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), nrow(new) == 4 * 92, all(new$totalvote > 0)); sh <- new %>% group_by(year) %>% summarise(d = median(demovote), r = median(repuvote)); stopifnot(all(sh$d > 0.15), all(sh$r > 0.15))
old <- panel[is_blk, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n")); nch <- sum(!(abs(old$demovote - old$demovote.n) < 1e-9 & abs(old$repuvote - old$repuvote.n) < 1e-9 & abs(old$totalvote - old$totalvote.n) < 0.5))
message("existing MEDSL Senate rows: ", sum(is_blk), "; changed: ", nch, "; rows added: ", nrow(new) - sum(is_blk))
p2 <- bind_rows(panel[!is_blk, ], new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
blk <- function(d) d$sample == "SE" & d$cty_fips %/% 1000 == 18 & d$year %in% yrs
stopifnot(isTRUE(all.equal(panel[!blk(panel), ] %>% arrange(year, cty_fips, sample), p2[!blk(p2), ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
