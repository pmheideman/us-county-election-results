## Fold the official Indiana House county results into the panel: 1990, 1992, 1994 (02u_house_in_1990_1994.R), 1996, 1998, 2000 (02u_house_in_1996_2000.R; image-only Indiana Secretary of State
## election reports), 2002 (02u_house_in_2002.R; native-text PDF) and 2010 (02u_house_in_2010.R; scanned booklet). 2026-09-21.
##   1990-2000: rows ADDED (no Indiana House rows before 2002).
##   2002, 2010: REPLACED. The old OpenElections-based rows (66 of 92 counties; 26 counties were all-zero records; 11-12 split counties carried only one district's part of the vote) are superseded.
## Gate: the panel's Indiana House 2002 and 2010 rows must be exactly the old OpenElections rows (elect_he_cty_in.rds); no rows before 2002. New shares must have positive Democratic and Republican
## medians (guards against a party-mapping error, which check_long_vs_source cannot catch). Everything outside Indiana House 1990-2000/2002/2010 is checked unchanged. Backup in scratchpad backup_in/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_in"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_in.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_in.rds"))
is_in <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 18
stopifnot(all(panel$year[is_in] >= 2002))
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_in.rds"))
for (y in c(2002, 2010)) { a <- panel %>% filter(is_in, year == y) %>% inner_join(old %>% filter(year == y), by = c("year", "cty_fips", "sample"), suffix = c("", ".o"))
  stopifnot(nrow(a) == 66, all(abs(a$demovote - a$demovote.o) < 1e-9 & abs(a$repuvote - a$repuvote.o) < 1e-9 & abs(a$totalvote - a$totalvote.o) < 0.5)) }
yrs <- c(seq(1990, 2002, 2), 2010)
new <- purrr::map_dfr(sprintf("elect_he_cty_in_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$sample == "HE"), all(new$cty_fips %/% 1000 == 18), nrow(new) == length(yrs) * 92)
sh <- new %>% group_by(year) %>% summarise(med_dem = median(demovote), med_rep = median(repuvote), n_dem0 = sum(demovote == 0), n_rep0 = sum(repuvote == 0), .groups = "drop"); print(as.data.frame(sh))
stopifnot(all(sh$med_dem > 0.2), all(sh$med_rep > 0.2))
cmp <- panel %>% filter(is_in, year %in% c(2002, 2010)) %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
message("old rows replaced: 2002 ", sum(cmp$year == 2002), ", 2010 ", sum(cmp$year == 2010), "; of which values change: ", sum(!(abs(cmp$demovote - cmp$demovote.n) < 1e-9 & abs(cmp$repuvote - cmp$repuvote.n) < 1e-9 & abs(cmp$totalvote - cmp$totalvote.n) < 0.5)))
p2 <- panel %>% filter(!(is_in & year %in% c(2002, 2010))); p2 <- bind_rows(p2, new[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
in_blocks <- function(d) d$sample == "HE" & d$cty_fips %/% 1000 == 18 & (d$year <= 2002 | d$year == 2010)
stopifnot(isTRUE(all.equal(panel %>% filter(!in_blocks(panel)) %>% arrange(year, cty_fips, sample), p2 %>% filter(!in_blocks(p2)) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
