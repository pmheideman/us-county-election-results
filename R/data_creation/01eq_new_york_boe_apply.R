## Fold the New York State Board of Elections House build (elect_he_cty_nyboe_<year>.rds, 1996-2018; 01ep_house_county_new_york_boe.R) into the panel. 2026-09-23.
## Adds 1996, 1998, 2002, 2008, 2010 (no New York House rows before) and REPLACES the New York House rows of 2000, 2004, 2006, 2012, 2014, 2016, 2018 (OpenElections 2000-2014, MEDSL 2016-2018):
## the official database is complete (62 of 62 counties every year except 1996, where Oswego has no rows), its county slices tie to the printed district totals, and the old rows had
## errors (OpenElections 2000 Bronx/Queens and 2004 Kings; 2012 and 2016-2018 missing counties; MEDSL 2018 only 47 of 62 counties). Gate: only New York (36xxx) House rows of these years change.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_ny_boe"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ny_boe_house.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ny_boe_house.rds"))
yrs <- seq(1996, 2018, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_nyboe_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == 36), nrow(new) == 62 * length(yrs) - 1)
is_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 36 & panel$year %in% yrs
message("existing NY House rows in these years (replaced): ", sum(is_target), "; new rows: ", nrow(new))
p2 <- bind_rows(panel[!is_target, ], new[, names(panel)]) %>% arrange(year, cty_fips, sample)
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_target) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 36 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
