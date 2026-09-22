## Fold the Illinois State Board of Elections county results 1998-2024 (elect_he_cty_il_sbe_<year>.rds; 02w_house_il_sbe.R) into the panel. 2026-09-21.
##   1998-2006: rows ADDED (Illinois had no House rows before 2008);
##   2008-2014: the existing OpenElections rows are IDENTICAL to the official files (checked: 102 of 102 in each year), nothing changes (the SBE builds become the recorded source);
##   2016-2024: the MEDSL rows are REPLACED where they differ (2016: 48, 2018: 16, 2020: 22, 2022: 36, 2024: 62 county-years; e.g. 2022 Cook 857,598 vs 1,404,768 and DuPage 130,050 vs 338,272).
## Gate: no Illinois House rows before 2008; 2008-2014 identical to the SBE; 2016-2024 all from MEDSL (provenance); positive dem/rep medians; everything else unchanged. Backup: backup_il/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_il"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_il.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_il.rds"))
yrs <- seq(1998, 2024, 2)
is_il <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 17
stopifnot(all(panel$year[is_il] >= 2008))
new <- purrr::map_dfr(sprintf("elect_he_cty_il_sbe_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == length(yrs) * 102)
sh <- new %>% group_by(year) %>% summarise(med_dem = median(demovote), med_rep = median(repuvote), .groups = "drop"); stopifnot(all(sh$med_dem > 0.15), all(sh$med_rep > 0.15))
old <- panel[is_il, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n")); same <- with(old, abs(demovote - demovote.n) < 1e-9 & abs(repuvote - repuvote.n) < 1e-9 & abs(totalvote - totalvote.n) < 0.5)
stopifnot(all(same[old$year %in% seq(2008, 2014, 2)]), all((prov %>% filter(sample == "HE", cty_fips %/% 1000 == 17, year >= 2016))$source == "he_cty_medsl"))
message("existing Illinois rows: ", sum(is_il), "; identical to SBE: ", sum(same), "; replaced: ", sum(!same), " (", paste(names(table(old$year[!same])), table(old$year[!same]), sep = ": ", collapse = ", "), "); rows added: ", nrow(new) - nrow(old))
blk <- function(d) d$sample == "HE" & d$cty_fips %/% 1000 == 17
p2 <- bind_rows(panel[!is_il, ], new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel[!blk(panel), ] %>% arrange(year, cty_fips, sample), p2[!blk(p2), ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
