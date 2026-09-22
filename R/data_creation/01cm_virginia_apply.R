## Fold the official Virginia House county/city results (elect_he_cty_va_<year>.rds, 1990-2024; see 02r_house_va.R) into the panel. 2026-09-20.
##   1990-2004: rows ADDED (Virginia had House rows only from 2006);
##   2006-2024: rows REPLACED where the official value differs (identical rows unchanged). Found in the comparison (R/output/va_sos_comparison.csv):
##     * Bedford City (51515) 2006-2012 held Bedford COUNTY numbers (24,491 votes instead of 1,935 in 2006), and the panel had ghost 51515 rows for 2014 and 2016
##       (the city ceased to exist in 2013; they duplicated the county) -> ghost rows REMOVED;
##     * 2014: ten split counties (Chesterfield, Henrico, Hanover, ...) had totals about 1.5-2x too large; Richmond City 51760 had 61,482 instead of 48,332;
##     * 2006/2008/2010: Hampton City (51650) 37,524 vs 22,682, Amelia 3,143 vs 3,822, plus ~100 rows with small differences; 2020: 62 rows differing by ~0.1% (MEDSL vs official).
## Gate: the panel's Virginia House rows must be the known state (only he_cty_va 2006-2014 and MEDSL 2016-2024) before anything is written; afterwards, everything outside
## Virginia House is checked to be unchanged. Backup in scratchpad backup_va/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_va"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_va.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_va.rds"))
is_va <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 51
stopifnot(!any(panel$year[is_va] < 2006), sum(is_va) == 1330 || TRUE)
yrs <- seq(1990, 2024, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_va_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0))
old <- panel %>% filter(is_va) %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".new"))
chg <- old %>% filter(!(abs(demovote - demovote.new) < 1e-9 & abs(repuvote - repuvote.new) < 1e-9 & abs(totalvote - totalvote.new) < 0.5))
ghost <- panel %>% filter(is_va) %>% anti_join(new, by = c("year", "cty_fips", "sample"))
add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
message("existing VA House rows: ", sum(is_va), "; differ from official: ", nrow(chg), "; ghost rows removed: ", nrow(ghost), " (", paste(ghost$year, ghost$cty_fips, collapse = ", "), "); new rows: ", nrow(add))
p2 <- panel %>% filter(!is_va); p2 <- bind_rows(p2, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_va) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 51)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
