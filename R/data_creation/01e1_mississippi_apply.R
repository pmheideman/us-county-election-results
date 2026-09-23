## Fold Mississippi U.S. House 1998/2000/2002/2004 (elect_he_cty_ms_<year>.rds; see 02an_house_ms_1998_2000.R and 02ao_house_ms_2002_2004.R,
## both from the Mississippi Official and Statistical Register -- clean, born-digital PDFs) into the panel. Every row here is ADDED, none
## replaced (Mississippi House had no rows before 2006; 2006-2014 via OpenElections, 2016+ via MEDSL).
## NOTE: 1990/1992 (built by a third script, 02am_house_ms_1990_1992.R, from an older/harder-to-read scanned edition of the same Register)
## are DELIBERATELY NOT folded in here -- real, unresolved district-level discrepancies (up to ~4,000-7,000 votes, concentrated in Mike
## Espy's Second District both years) survived extensive re-verification by two independent passes; see the coordinating session's notes
## and data_corrections_log.csv. Left out pending a decision on how to handle that data quality gap.
## 1994 and 1996 remain confirmed absent from every source file the user supplied.
## Gate: no existing MS House rows for 1998/2000/2002/2004; afterwards, everything outside them is checked to be unchanged.
## Backup in scratchpad backup_ms/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_ms"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ms_bluebook.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ms_bluebook.rds"))

yrs <- c(1998, 2000, 2002, 2004)
is_ms_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 28 & panel$year %in% yrs
stopifnot(sum(is_ms_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_ms_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 82 * length(yrs))

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))                                             # every row is genuinely new
message("new MS House rows to add: ", nrow(add), " (", length(yrs), " years x 82 counties)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 28 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
