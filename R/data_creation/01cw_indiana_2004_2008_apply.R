## Fold the official Indiana 2004, 2006 and 2008 House county results (elect_he_cty_in_<year>.rds; 02u_house_in_2004.R, _2006.R, _2008.R; image-only Election Division election reports,
## every cell read from the page images) into the panel, REPLACING the flawed OpenElections rows (66 of 92 counties; 26 all-zero counties; 11 split counties carrying only part of their vote;
## 2006: two counties classify the write-in Mantooth '(W-R)' as Republican). 2026-09-21.
## Gate: the panel's Indiana House rows for these years must be exactly the old elect_he_cty_in.rds rows (66 each); positive dem/rep medians; everything else unchanged. Backup: backup_in3/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_in3"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_in3.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_in3.rds"))
yrs <- c(2004, 2006, 2008); old_src <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_in.rds"))
is_blk <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 18 & panel$year %in% yrs
for (y in yrs) { a <- panel[is_blk & panel$year == y, ] %>% inner_join(old_src %>% filter(year == y), by = c("year", "cty_fips", "sample"), suffix = c("", ".o"))
  stopifnot(nrow(a) == 66, all(abs(a$demovote - a$demovote.o) < 1e-9 & abs(a$repuvote - a$repuvote.o) < 1e-9 & abs(a$totalvote - a$totalvote.o) < 0.5)) }
new <- purrr::map_dfr(sprintf("elect_he_cty_in_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 3 * 92)
sh <- new %>% group_by(year) %>% summarise(med_dem = median(demovote), med_rep = median(repuvote), .groups = "drop"); print(as.data.frame(sh)); stopifnot(all(sh$med_dem > 0.15), all(sh$med_rep > 0.15))
cmp <- panel[is_blk, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
message("old rows replaced: ", nrow(cmp), "; of which values change: ", sum(!(abs(cmp$demovote - cmp$demovote.n) < 1e-9 & abs(cmp$repuvote - cmp$repuvote.n) < 1e-9 & abs(cmp$totalvote - cmp$totalvote.n) < 0.5)), "; added: ", nrow(new) - nrow(cmp))
p2 <- bind_rows(panel[!is_blk, ], new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
blk <- function(d) d$sample == "HE" & d$cty_fips %/% 1000 == 18 & d$year %in% yrs
stopifnot(isTRUE(all.equal(panel[!blk(panel), ] %>% arrange(year, cty_fips, sample), p2[!blk(p2), ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
