## Fold Mississippi U.S. House 1990/1992 (elect_he_cty_ms_<year>.rds; see 02am_house_ms_1990_1992.R, built from the older/harder-to-read
## 1992-1996 edition of the Mississippi Official and Statistical Register, cross-checked against FEC's certified district totals) into
## the panel. Every row here is ADDED, none replaced (Mississippi House had no rows before 1998, per the earlier 1998-2004 fold-in).
##
## KNOWN DATA QUALITY CAVEAT, decided with the user 2026-09-22: three candidate/district/years could not be fully reconciled to FEC's
## certified totals despite two independent transcription passes and a dedicated FEC-reconciliation pass (three verification methods each) --
## 1990 District 2 (Mike Espy), 1992 District 2 (Mike Espy), 1992 District 5 (Gene Taylor) each show the SAME county-sum overcount of
## exactly +4,000 votes relative to the correct (FEC-certified, and in 2 of 3 cases book-confirmed) total. The precision and repetition of
## this figure across independent tables/years/candidates indicates a real defect in the source document itself, not a transcription error
## (see data_corrections_log.csv for the full FEC cross-check). Per the user's decision, this data is folded in as-is (it is the best
## available county-level breakdown, and the DISTRICT-level total is known/correctable even where the county-level allocation is not) and
## flagged `totals_possibly_inflated` in the release build (03a_house_release.R), the same mechanism already used for NJ Bergen 2024.
## 1990 is also missing Yalobusha County entirely (81/82) -- a separate, smaller, undiagnosed gap, also logged.
## 1990 District 1 has a smaller (~2%), write-in/scattered-vote-shaped gap vs FEC that was not chased further (see log).
##
## Gate: no existing MS House rows for 1990/1992; afterwards, everything outside them is checked to be unchanged. Backup in scratchpad backup_ms2/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_ms2"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ms_1990_1992.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ms_1990_1992.rds"))

yrs <- c(1990, 1992)
is_ms_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 28 & panel$year %in% yrs
stopifnot(sum(is_ms_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_ms_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 81 + 82)

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))                                             # every row is genuinely new
message("new MS House rows to add: ", nrow(add), " (1990: 81 counties, 1992: 82 counties)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 28 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
