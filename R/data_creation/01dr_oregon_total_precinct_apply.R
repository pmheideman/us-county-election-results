## Fixes a doubling bug in Oregon House 2000/2002/2004: 01bc_house_county_oregon.R's precinct-level
## source files (OpenElections) carry an EXTRA row per (office,district,party,candidate) with
## precinct=="Total" in some counties -- a county-wide rollup disguised as an ordinary precinct row,
## using the REAL candidate name (not caught by the existing is_pseudo_row candidate-name filter).
## Confirmed: Multnomah 2002 CD-3, Blumenauer's precinct=="Total" row (133,810) exactly equals the
## sum of his ~110 real precinct rows. This is the cause of the Oregon House 2000s doubling flagged
## by qa_state_reconcile_house_1990s.R against the FEC (CD-3 sits entirely in Multnomah, so it came
## out ~2x; other districts span counties with and without the bug, so they were inflated 25-41%).
## Fix: 01bc now also excludes precinct=="Total" rows (see is_total_precinct()); not every county's
## file has this extra row (12/20 in 2000, 8/12 in 2002, 9/10 in 2004), so the effect is real but
## uneven across counties. Verified after the fix: Oregon CD-3 2002 (all counties) now totals
## 237,598 vs the FEC's certified 234,977 (1.1% off, in the normal range for this project, vs. ~2x
## before). Building the matching candidate-level long table (02w_house_long_oregon.R) and running
## check_long_vs_source also caught two more, unrelated, pre-existing pseudo-candidate rows that
## is_pseudo_row had been missing on this state's own OpenElections wording ("Overvotes"/
## "Undervotes" with no space, e.g. Gilliam 2004; "Blanks" as a bare candidate name, e.g. Clackamas
## 2002) -- both added to is_pseudo_row so the shares panel and the long table agree exactly (246/246).
## Gate: only county-years currently sourced from he_cty_or change; shares stay within [0,1]; backup
## in the session scratchpad.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds"))
new_or <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_or.rds")) %>% select(year, cty_fips, sample, demovote, repuvote, totalvote)

blk <- panel %>% mutate(.i = row_number()) %>%
  inner_join(new_or %>% transmute(year, cty_fips, sample, .flag = TRUE), by = c("year", "cty_fips", "sample"))
p_src <- prov %>% inner_join(new_or %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(all(p_src$source == "he_cty_or"))   # gate: only currently-Oregon-sourced rows are being touched
stopifnot(nrow(blk) == nrow(new_or))          # every OR key in the new build exists exactly once in the panel

changed <- blk %>%
  inner_join(new_or, by = c("year", "cty_fips", "sample"), suffix = c(".o", ".n")) %>%
  filter(abs(demovote.o - demovote.n) > 1e-9 | abs(repuvote.o - repuvote.n) > 1e-9 | abs(totalvote.o - totalvote.n) > 0.5)
message("OR House county-years changed by all three fixes: ", nrow(changed), " of ", nrow(new_or))
print(as.data.frame(changed %>% count(year) %>% arrange(year)))
stopifnot(nrow(changed) > 0)
## All three fixes (precinct=="Total", Overvotes/Undervotes without a space, bare "Blanks") only
## ever remove votes that were being over-counted, so every changed county-year's total should be
## <= the stale panel value.
stopifnot(all(changed$totalvote.n <= changed$totalvote.o + 0.5))
sh <- changed$demovote.n + changed$repuvote.n
stopifnot(all(sh >= 0 & sh <= 1.001))

bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1a14ce10-d028-4538-a07e-60e7ffd0a619/scratchpad/backup_or"
if (!file.exists(file.path(bk, "elect_cty_final_before_or.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_or.rds"))

panel2 <- panel
idx <- match(paste(blk$year, blk$cty_fips, blk$sample), paste(new_or$year, new_or$cty_fips, new_or$sample))
panel2[blk$.i, c("demovote", "repuvote", "totalvote")] <- new_or[idx, c("demovote", "repuvote", "totalvote")]
stopifnot(nrow(panel2) == nrow(panel))
unchanged <- setdiff(seq_len(nrow(panel)), blk$.i)
stopifnot(isTRUE(all.equal(panel[unchanged, ] %>% arrange(year, cty_fips, sample), panel2[unchanged, ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))

saveRDS(panel2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
message("panel rows ", nrow(panel), " -> ", nrow(panel2), " (", nrow(blk), " rows updated in place)")
