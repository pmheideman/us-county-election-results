## Fold the Indiana Election Division ENR archive results (elect_he_cty_in_enr_<year>.rds, 2016, 2018, 2020, 2022, 2024; 02v_house_in_enr.R) into the panel, replacing the MEDSL Indiana House rows.
## Found in the comparison: MEDSL's Indiana rows had (a) county totals DOUBLED (2016 Hamilton 63,882 vs 31,931; 2024 Hendricks 160,804 vs 80,387, St. Joseph 217,886 vs 108,943),
## (b) a district missing (2016 Grant: no Democratic votes; 2018 Vermillion/Fountain?: see R/output/in_enr_<year>_vs_panel_differences.csv), (c) differences of 0.1-1% (precinct data vs the certified result),
## and only 35-92 of 92 counties in 2018-2022 (MEDSL Indiana coverage is partial in 2018 (51), 2020 (53) and 2022 (35)). 2026-09-21.
## Gate: every existing Indiana House row for 2016-2024 must be from MEDSL (provenance); new shares must have positive Democratic/Republican medians; everything else unchanged. Backup: backup_in2/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_in2"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_in2.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_in2.rds"))
yrs <- c(2016, 2018, 2020, 2022, 2024)
is_blk <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 18 & panel$year %in% yrs
pv <- prov %>% filter(sample == "HE", cty_fips %/% 1000 == 18, year %in% yrs); stopifnot(all(pv$source == "he_cty_medsl"), nrow(pv) == sum(is_blk))
new <- purrr::map_dfr(sprintf("elect_he_cty_in_enr_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 5 * 92)
sh <- new %>% group_by(year) %>% summarise(med_dem = median(demovote), med_rep = median(repuvote), .groups = "drop"); print(as.data.frame(sh)); stopifnot(all(sh$med_dem > 0.15), all(sh$med_rep > 0.15))
old <- panel[is_blk, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
chg <- old %>% filter(!(abs(demovote - demovote.n) < 1e-9 & abs(repuvote - repuvote.n) < 1e-9 & abs(totalvote - totalvote.n) < 0.5))
message("existing MEDSL rows: ", sum(is_blk), "; changed: ", nrow(chg), " (", paste(names(table(chg$year)), table(chg$year), sep = ": ", collapse = ", "), "); rows added: ", nrow(new) - sum(is_blk))
p2 <- bind_rows(panel[!is_blk, ], new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel[!is_blk, ] %>% arrange(year, cty_fips, sample), p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 18 & year %in% yrs)) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
