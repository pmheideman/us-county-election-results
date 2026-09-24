## Fold the Massachusetts Elections Statistics build (elect_he_cty_maelst_<year>.rds, 1990-2014; 01ey_house_county_massachusetts_electionstats.R) into the panel. 2026-09-23.
## Adds 1990-1998 (1996: 13 of 14 counties, Sutton has no rows) and REPLACES the OpenElections rows of 2000-2014 (2002-2008 and 2014 are identical; 2000 had 4 counties with a missing Republican
## column, 2010 two tiny differences, 2012 only 13 of 14 counties). Gate: only Massachusetts (25xxx) House rows of these years change.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_ma_elst"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ma_elst.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ma_elst.rds"))
yrs <- seq(1990, 2014, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_maelst_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == 25), nrow(new) == 14 * length(yrs) - 1)
is_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 25 & panel$year %in% yrs
message("existing MA House rows in these years (replaced): ", sum(is_target), "; new rows: ", nrow(new))
p2 <- bind_rows(panel[!is_target, ], new[, names(panel)]) %>% arrange(year, cty_fips, sample)
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_target) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 25 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
