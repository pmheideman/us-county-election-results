## Fold the Connecticut Secretary of the State Election History build (elect_he_cty_ctelh_<year>.rds, 1990-2014; 01es_house_county_connecticut_elstats.R) into the panel. 2026-09-23.
## Adds 1990-1998 (1998: only the 5 counties whose towns all have House rows), 2004 and 2014 (no Connecticut House rows before), and REPLACES the OpenElections rows of 2000, 2002, 2006, 2008, 2010 and 2012
## (2012 had only 3 of 8 counties; 2002 had a blank party label, 2010 differences of up to 1.6 points in share). Gate: only Connecticut (09xxx) House rows of these years change.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_ct_elh"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ct_elh.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ct_elh.rds"))
yrs <- seq(1990, 2014, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_ctelh_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == 9), nrow(new) == 8 * length(yrs) - 3)
is_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 9 & panel$year %in% yrs
message("existing CT House rows in these years (replaced): ", sum(is_target), "; new rows: ", nrow(new))
p2 <- bind_rows(panel[!is_target, ], new[, names(panel)]) %>% arrange(year, cty_fips, sample)
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_target) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 9 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
