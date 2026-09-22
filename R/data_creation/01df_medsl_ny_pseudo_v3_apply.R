## Apply the extended pseudo-row list (01a PSEUDO_RE, 2026-09-21: 'Federal Votes', 'Public Counter', 'Manually Counted Emergency', 'Scattered Votes') to the MEDSL-sourced House and Senate rows. 2026-09-21.
## Found by comparing New York's 2018 Senate county table with America Votes 33: MEDSL's New York City rows carry a 'PUBLIC COUNTER' pseudo-candidate (the machine-count subtotal, 274,705 in the Bronx 2018)
## next to the real candidates, DOUBLING the total and halving every share (Bronx and Queens 2018, all five boroughs in 2022; smaller 'Manually Counted Emergency' / 'Scattered Votes' amounts in 2016 and elsewhere);
## Tompkins County 2018 had 38,211 'Federal Votes' in the Senate race. 22 House and 22 Senate county-years change, all in New York (2016, 2018, 2022).
## Gate: every changed row is MEDSL-sourced (provenance); the new totals are not larger; everything else unchanged; the MEDSL source files (elect_he/se_cty_medsl.rds) are refreshed so provenance stays clean. Backup: backup_pese/.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese"; saveRDS(panel, file.path(bk, "elect_cty_final_before_ny_pseudo.rds"))
upd <- list()
for (o in c("he", "se")) {
  f <- file.path(OUTPUT_DIR, sprintf("elect_%s_cty_medsl.rds", o)); s <- readRDS(f); file.copy(f, file.path(bk, sprintf("elect_%s_cty_medsl_before_ny_pseudo.rds", o)), overwrite = FALSE)
  d <- derive_shares(readRDS(file.path(LONG_DIR, sprintf("%s_medsl.rds", o))))
  j <- s %>% inner_join(d, by = c("year", "cty_fips", "sample"), suffix = c("", ".n")) %>% filter(!(abs(demovote - demovote.n) < 1e-9 & abs(repuvote - repuvote.n) < 1e-9 & abs(totalvote - totalvote.n) < 0.5))
  stopifnot(nrow(j) == 22, all(j$cty_fips %/% 1000 == 36), all(j$totalvote.n <= j$totalvote), all(j$year %in% c(2016, 2018, 2022)))
  pv <- prov %>% filter(sample == toupper(sub("he", "HE", sub("se", "SE", o)))) %>% semi_join(j, by = c("year", "cty_fips")); stopifnot(all(pv$source == sprintf("%s_cty_medsl", o)), nrow(pv) == 22)
  s2 <- s %>% left_join(j %>% select(year, cty_fips, sample, demovote.n, repuvote.n, totalvote.n), by = c("year", "cty_fips", "sample")) %>%
    mutate(demovote = ifelse(!is.na(totalvote.n), demovote.n, demovote), repuvote = ifelse(!is.na(totalvote.n), repuvote.n, repuvote), totalvote = ifelse(!is.na(totalvote.n), totalvote.n, totalvote)) %>% select(names(s)); saveRDS(s2, f)
  upd[[o]] <- j %>% transmute(year, cty_fips, sample, demovote = demovote.n, repuvote = repuvote.n, totalvote = totalvote.n, state = NA_character_) }
u <- bind_rows(upd); p2 <- bind_rows(panel %>% anti_join(u, by = c("year", "cty_fips", "sample")), u[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0, nrow(p2) == nrow(panel))
stopifnot(isTRUE(all.equal(panel %>% anti_join(u, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips, sample), p2 %>% anti_join(u, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("replaced ", nrow(u), " panel rows (House ", nrow(upd$he), ", Senate ", nrow(upd$se), ")")
