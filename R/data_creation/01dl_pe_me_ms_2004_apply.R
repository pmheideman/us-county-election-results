## Replace the Maine (16) and Mississippi (82) 2004 presidential county-years with the certified-total-tied rebuilds (elect_pe_cty_me_2004 / _ms_2004; 02ab_president_me_ms_2004.R). 2026-09-21.
## MEDSL's rows were 7.3% (ME) and 3.2% (MS) short of the FEC certified state totals. Gate: the 98 rows come from MEDSL (provenance); positive dem/rep medians; everything else unchanged. Backup: backup_pe0405/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pe2004"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_pe2004.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_pe2004.rds"))
blk <- function(d) d$sample == "PE" & d$year == 2004 & (d$cty_fips %/% 1000) %in% c(23, 28)
new <- bind_rows(readRDS(file.path(OUTPUT_DIR, "elect_pe_cty_me_2004.rds")), readRDS(file.path(OUTPUT_DIR, "elect_pe_cty_ms_2004.rds"))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 98, sum(blk(panel)) == 98, !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), median(new$demovote) > 0.15, median(new$repuvote) > 0.15)
stopifnot(all((prov %>% filter(sample == "PE", year == 2004, cty_fips %/% 1000 %in% c(23, 28)))$source == "pe_cty_medsl"))
p2 <- bind_rows(panel[!blk(panel), ], new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel[!blk(panel), ] %>% arrange(year, cty_fips, sample), p2[!blk(p2), ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
old <- panel[blk(panel), ] %>% mutate(st = cty_fips %/% 1000) %>% group_by(st) %>% summarise(old_total = sum(totalvote)); nw <- new %>% mutate(st = cty_fips %/% 1000) %>% group_by(st) %>% summarise(new_total = sum(totalvote)); print(as.data.frame(left_join(old, nw, by = "st")))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
