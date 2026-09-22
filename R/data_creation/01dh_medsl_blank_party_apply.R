## MEDSL rows whose PARTY field is blank were classified OTHER, so Democratic and/or Republican votes were counted as "other": Georgia, Kentucky and North Dakota 2016 (Senate: GA 158, KY 108, ND 53 county-years with
## Democratic AND Republican shares of 0; House: GA 16, KY 27), and smaller cases (IL, ME, NJ 2016; IN, NY, VT 2018/2020; AZ 2024). 2026-09-21. Found by the QA sweep (R/qa_medsl_sweep.R, test T3).
## Fix (already in 01a, but applied only to NJ/OR 2024): fill blank party labels from the same candidate's labelled rows (state + district + name), now for ALL state-years (02a/02b), plus two Wikipedia-nominee overrides for
## North Dakota 2016 Senate (Hoeven REP, Glassheim DEM-NPL: no labelled row anywhere) in candidate_party_overrides.csv. Totals do not change, only the party split.
## Gate: every changed row is MEDSL-sourced (rows since replaced by official builds are skipped), totals identical, dem + rep never decrease; MEDSL source files refreshed. Backup: backup_pese/.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese"; saveRDS(panel, file.path(bk, "elect_cty_final_before_blank_party.rds"))
upd <- list()
for (o in c("he", "se")) {
  f <- file.path(OUTPUT_DIR, sprintf("elect_%s_cty_medsl.rds", o)); s <- readRDS(f); file.copy(f, file.path(bk, sprintf("elect_%s_cty_medsl_before_blank_party.rds", o)), overwrite = FALSE)
  d <- derive_shares(readRDS(file.path(LONG_DIR, sprintf("%s_medsl.rds", o))))
  j <- s %>% inner_join(d, by = c("year", "cty_fips", "sample"), suffix = c("", ".n")) %>% filter(!(abs(demovote - demovote.n) < 1e-9 & abs(repuvote - repuvote.n) < 1e-9 & abs(totalvote - totalvote.n) < 0.5))
  stopifnot(all(abs(j$totalvote - j$totalvote.n) < 0.5), all(j$demovote.n + j$repuvote.n >= j$demovote + j$repuvote - 1e-9))
  s2 <- s %>% left_join(j %>% select(year, cty_fips, sample, demovote.n, repuvote.n), by = c("year", "cty_fips", "sample")) %>%
    mutate(demovote = ifelse(!is.na(demovote.n), demovote.n, demovote), repuvote = ifelse(!is.na(repuvote.n), repuvote.n, repuvote)) %>% select(names(s)); saveRDS(s2, f)
  pv <- prov %>% filter(sample == toupper(o)) %>% select(year, cty_fips, source)
  jj <- j %>% left_join(pv, by = c("year", "cty_fips")) %>% filter(source == sprintf("%s_cty_medsl", o))
  message(toupper(o), ": changed county-years in the MEDSL source ", nrow(j), "; MEDSL-sourced in the panel ", nrow(jj))
  upd[[o]] <- jj %>% transmute(year, cty_fips, sample, demovote = demovote.n, repuvote = repuvote.n, totalvote, state = NA_character_) }
u <- bind_rows(upd); p2 <- bind_rows(panel %>% anti_join(u, by = c("year", "cty_fips", "sample")), u[, names(panel)]); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0, nrow(p2) == nrow(panel))
stopifnot(isTRUE(all.equal(panel %>% anti_join(u, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips, sample), p2 %>% anti_join(u, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("replaced ", nrow(u), " panel rows (House ", nrow(upd$he), ", Senate ", nrow(upd$se), ")")
