## Bring the President and Senate rows of the panel in line with the candidate-level long tables (project rules: totals = votes for candidates only; special elections ignored). 2026-09-21.
##  (1) President 2024, Arizona / Iowa / Vermont (127 county-years): MEDSL's `totalvotes` column includes OVERVOTES and UNDERVOTES rows (about 0.4-1.8% of the total; 01a divided by it), so shares were
##      slightly low. Replaced by shares over the sum of candidate votes (the long table 02c_president_long_medsl.R removes the pseudo-candidate rows with 01a's PSEUDO_RE).
##  (2) Senate 1990-2014 (even years), 515 county-years (Indiana 1990, Oklahoma 1994, Georgia 2000, Missouri 2002, Delaware / Massachusetts / West Virginia 2010): the state-year's only Senate race was a
##      SPECIAL election (Algara & Amlani election_type "S", used by 01b when no general exists). Removed (special elections are excluded from the release).
## Gate: the panel rows must equal the source files first; positive dem/rep medians for the replaced rows; everything else unchanged. Backup: backup_pese/.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pese"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_pese.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_pese.rds"))
pe_long <- readRDS(file.path(LONG_DIR, "pe_medsl.rds")); d <- derive_shares(pe_long) %>% filter(year == 2024)
pp <- panel %>% filter(sample == "PE", year == 2024) %>% inner_join(d, by = c("year", "cty_fips", "sample"), suffix = c("", ".new"))
chg <- pp %>% filter(!(abs(demovote - demovote.new) < 1e-9 & abs(repuvote - repuvote.new) < 1e-9 & abs(totalvote - totalvote.new) < 0.5))
stopifnot(nrow(chg) == 127, all(chg$cty_fips %/% 1000 %in% c(4, 19, 50)), all(chg$totalvote > chg$totalvote.new), all(chg$demovote.new > 0.05))
message("President 2024 rows to replace: ", nrow(chg))
sp <- readRDS(file.path(LONG_DIR, "se_historical.rds")); dh <- derive_shares(sp)
scope_se <- panel %>% filter(sample == "SE", year >= 1990, year <= 2014, year %% 2 == 0, !((cty_fips %/% 1000) %in% c(2, 11, 15))); hs <- readRDS(file.path(OUTPUT_DIR, "elect_se_cty_historical.rds"))
rm_keys <- scope_se %>% anti_join(dh, by = c("year", "cty_fips")) %>% select(year, cty_fips); stopifnot(nrow(rm_keys) == 515, all(paste(rm_keys$year, rm_keys$cty_fips) %in% paste(hs$year, hs$cty_fips)))
message("Senate special-only rows to remove: ", nrow(rm_keys))
p2 <- panel %>% anti_join(rm_keys %>% mutate(sample = "SE"), by = c("year", "cty_fips", "sample"))
upd <- chg %>% transmute(year, cty_fips, sample, demovote = demovote.new, repuvote = repuvote.new, totalvote = totalvote.new, state = NA_character_)
p2 <- bind_rows(p2 %>% anti_join(upd, by = c("year", "cty_fips", "sample")), upd[, names(p2)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0, nrow(p2) == nrow(panel) - 515)
other0 <- panel %>% anti_join(rm_keys %>% mutate(sample = "SE"), by = c("year", "cty_fips", "sample")) %>% anti_join(chg, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% anti_join(chg, by = c("year", "cty_fips", "sample")) %>% arrange(year, cty_fips, sample); stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
