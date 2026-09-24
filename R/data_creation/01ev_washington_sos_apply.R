## Fold Washington 2008 (elect_he_cty_wasos_2008.rds: SOS workbooks) and 2010 (elect_he_cty_wasos_2010.rds: the SOS county precinct files, 01ew_washington_2010_sos_precinct.R) into the panel;
## see 01eu_house_county_washington_sos.R. 2026-09-23. 2008 is ADDED (no Washington House rows before); 2010 is REPLACED (was 37 of 39 counties: Wahkiakum and Pend Oreille were dropped by a spelling mismatch, and the OpenElections rows had errors in King, Pierce, Skamania and Snohomish).
## Gate: only Washington (53xxx) House rows of 2008 and 2010 change.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_wa_sos"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_wa_sos.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_wa_sos.rds"))
new <- bind_rows(readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wasos_2008.rds")), readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wasos_2010.rds"))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 78, !anyDuplicated(new[, c("year", "cty_fips", "sample")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == 53))
is_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 53 & panel$year %in% c(2008, 2010)
message("existing WA House rows 2008/2010 (replaced): ", sum(is_target), "; new rows: ", nrow(new))
stopifnot(sum(is_target) %in% c(37, 78))                       # 37 = 2010 only (original panel); 78 = this script already ran once
old <- panel[is_target & panel$year == 2010, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
message("of the ", nrow(old), " old 2010 rows, ", sum(abs(old$demovote - old$demovote.n) > 1e-9 | abs(old$repuvote - old$repuvote.n) > 1e-9), " change (max Democratic-share change ", round(max(abs(old$demovote - old$demovote.n)), 4), ")")
p2 <- bind_rows(panel[!is_target, ], new[, names(panel)]) %>% arrange(year, cty_fips, sample)
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_target) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 53 & year %in% c(2008, 2010))) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
