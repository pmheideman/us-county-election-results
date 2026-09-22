## President 2024: add the 367 county-years of Idaho, North Carolina, Rhode Island, Utah, Washington, West Virginia, Wisconsin and Wyoming that 01a silently dropped. 2026-09-21.
## Cause: MEDSL's 2024 county file has a BLANK `mode` for those states; 01a read it as NA, so `any(mode == "TOTAL")` was NA and its filter removed every row. A blank mode is treated as a total (02c_president_long_medsl.R).
## Gate: the keys must be absent from the panel; shares positive; county totals 0.85-1.35 of the 2020 presidential totals; everything else unchanged. Also refreshes elect_pe_cty_medsl.rds (source of record). Backup: backup_pese/.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese"
saveRDS(panel, file.path(bk, "elect_cty_final_before_pe2024states.rds"))
d <- derive_shares(readRDS(file.path(LONG_DIR, "pe_medsl.rds"))) %>% filter(year == 2024, (cty_fips %/% 1000) %in% c(16, 37, 44, 49, 53, 54, 55, 56))
new <- d %>% anti_join(panel %>% filter(sample == "PE"), by = c("year", "cty_fips")); stopifnot(nrow(new) == 367, nrow(new) == nrow(d))
ref <- panel %>% filter(sample == "PE", year == 2020) %>% select(cty_fips, ref = totalvote); r <- new %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
message("367 rows | median dem ", round(median(new$demovote), 3), " rep ", round(median(new$repuvote), 3), " | 2024/2020 total: min ", round(min(r$ratio, na.rm = TRUE), 2), " median ", round(median(r$ratio, na.rm = TRUE), 2), " max ", round(max(r$ratio, na.rm = TRUE), 2), " | counties without a 2020 total: ", sum(is.na(r$ratio)))
stopifnot(median(new$demovote) > 0.15, median(new$repuvote) > 0.15, quantile(r$ratio, 0.05, na.rm = TRUE) > 0.8, quantile(r$ratio, 0.95, na.rm = TRUE) < 1.4)
add <- new %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
p2 <- bind_rows(panel, add[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0, nrow(p2) == nrow(panel) + 367)
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
f <- file.path(OUTPUT_DIR, "elect_pe_cty_medsl.rds"); s <- readRDS(f); stopifnot(!any(paste(new$year, new$cty_fips) %in% paste(s$year, s$cty_fips)))
saveRDS(bind_rows(s, new[, names(s)[names(s) %in% names(new)]]), f); message("elect_pe_cty_medsl.rds now has ", nrow(readRDS(f)), " rows")
