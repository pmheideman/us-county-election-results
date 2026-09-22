## Georgia 2022 U.S. Senate: MEDSL's file holds the November general AND the December runoff; 01a summed both (county totals about twice the House totals, 147 of 159 counties flagged by the QA sweep).
## Decisive-round rule (docs/DECISIONS.md; same as Louisiana): keep only the runoff rows (01a `decisive_round`; 02b sets stage 'runoff'). 159 Senate county-years replaced. 2026-09-21.
## Gate: only GA 2022 SE rows change, all MEDSL-sourced, totals go down by 40-60%; Democratic share changes by less than 0.05. MEDSL source file refreshed. Backup: backup_pese/.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese"; saveRDS(panel, file.path(bk, "elect_cty_final_before_ga_runoff.rds"))
f <- file.path(OUTPUT_DIR, "elect_se_cty_medsl.rds"); s <- readRDS(f); file.copy(f, file.path(bk, "elect_se_cty_medsl_before_ga_runoff.rds"), overwrite = FALSE)
d <- derive_shares(readRDS(file.path(LONG_DIR, "se_medsl.rds")))
j <- s %>% inner_join(d, by = c("year", "cty_fips", "sample"), suffix = c("", ".n")) %>% filter(!(abs(demovote - demovote.n) < 1e-9 & abs(repuvote - repuvote.n) < 1e-9 & abs(totalvote - totalvote.n) < 0.5))
stopifnot(nrow(j) == 159, all(j$year == 2022), all(j$cty_fips %/% 1000 == 13), all(j$totalvote.n / j$totalvote > 0.4 & j$totalvote.n / j$totalvote < 0.6), all(abs(j$demovote.n - j$demovote) < 0.05))
pv <- prov %>% filter(sample == "SE", year == 2022, cty_fips %/% 1000 == 13); stopifnot(all(pv$source == "se_cty_medsl"), nrow(pv) == 159)
s2 <- s %>% left_join(j %>% select(year, cty_fips, sample, demovote.n, repuvote.n, totalvote.n), by = c("year", "cty_fips", "sample")) %>%
  mutate(demovote = ifelse(!is.na(totalvote.n), demovote.n, demovote), repuvote = ifelse(!is.na(totalvote.n), repuvote.n, repuvote), totalvote = ifelse(!is.na(totalvote.n), totalvote.n, totalvote)) %>% select(names(s)); saveRDS(s2, f)
u <- j %>% transmute(year, cty_fips, sample, demovote = demovote.n, repuvote = repuvote.n, totalvote = totalvote.n, state = NA_character_)
p2 <- bind_rows(panel %>% anti_join(u, by = c("year", "cty_fips", "sample")), u[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0, nrow(p2) == nrow(panel))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("replaced ", nrow(u), " Georgia 2022 Senate rows")
