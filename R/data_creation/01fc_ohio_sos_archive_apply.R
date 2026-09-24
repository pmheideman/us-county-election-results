## Fold Ohio 1996 and 2002 (elect_he_cty_ohsosarc_<year>.rds; Ohio SOS results pages as archived by the Wayback Machine; 01fa_ohio_sos_archive_parse.py, 01fb_house_county_ohio_sos_archive.R) into the panel. 2026-09-23.
## 1996 is ADDED (no Ohio House rows before); 2002 is REPLACED: 87 of 88 counties are identical, Cuyahoga's old row (OpenElections, 46,100 votes) missed the Cuyahoga portions of CD10 and CD11
## (the SOS page has 374,372 House votes there). 2000 is identical to the archive and is left as built. Gate: only Ohio (39xxx) House rows of 1996 and 2002 change.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_oh_arc"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_oh_arc.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_oh_arc.rds"))
new <- bind_rows(readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ohsosarc_1996.rds")), readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ohsosarc_2002.rds"))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 176, !anyDuplicated(new[, c("year", "cty_fips", "sample")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == 39))
is_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 39 & panel$year %in% c(1996, 2002)
message("existing OH House rows 1996/2002 (replaced): ", sum(is_target), "; new rows: ", nrow(new)); stopifnot(sum(is_target) %in% c(88, 176))
old <- panel[is_target & panel$year == 2002, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n")); message("of the ", nrow(old), " old 2002 rows, ", sum(abs(old$demovote - old$demovote.n) > 1e-9 | abs(old$repuvote - old$repuvote.n) > 1e-9), " change")
p2 <- bind_rows(panel[!is_target, ], new[, names(panel)]) %>% arrange(year, cty_fips, sample); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_target) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 39 & year %in% c(1996, 2002))) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
