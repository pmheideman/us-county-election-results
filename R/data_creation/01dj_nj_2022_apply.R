## Replace the 21 New Jersey 2022 House county-years (MEDSL repeats each district's total in every county: 7.83M votes vs 2.61M actual)
## with the Division of Elections' official results (elect_he_cty_nj_2022.rds; 02aa_house_nj_2022.R: 53 candidates and 12 district totals tie exactly). 2026-09-21.
## Gate: the 21 rows come from MEDSL (provenance); positive dem/rep medians; everything else unchanged. Backup: backup_nj22/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_nj22"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_nj22.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_nj22.rds"))
blk <- function(d) d$sample == "HE" & d$year == 2022 & d$cty_fips %/% 1000 == 34
new <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_nj_2022.rds")) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 21, sum(blk(panel)) == 21, !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), median(new$demovote) > 0.15, median(new$repuvote) > 0.15)
stopifnot(all((prov %>% filter(sample == "HE", year == 2022, cty_fips %/% 1000 == 34))$source == "he_cty_medsl"))
p2 <- bind_rows(panel[!blk(panel), ], new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel[!blk(panel), ] %>% arrange(year, cty_fips, sample), p2[!blk(p2), ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
message("MEDSL total votes ", sum(panel$totalvote[blk(panel)]), " -> official ", sum(new$totalvote))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
