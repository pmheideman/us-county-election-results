## Rebuild elect_cty_final.rds FROM SCRATCH out of the per-source shares files (R/output/elect_{he,pe,se}_cty_*.rds), using the exact same
## priority rule as build_provenance.R (per-year official build > MEDSL > historical; elect_cty_medsl.rds skipped as a union file), and
## verify the result is IDENTICAL to the current panel. This is the reproducibility test for the whole pipeline: after
## R/build_provenance.R reports status=="as_source" for every panel row (no "modified" rows -- a row that was hand-patched in place, not
## backed by any source file), the panel is FULLY DETERMINED by (a) the current set of source files and (b) this priority rule, and
## should therefore be re-derivable byte-for-byte without replaying the ~50 apply scripts individually.
##
## What this DOES verify: the panel is exactly the union of its declared sources under a deterministic rule -- no manual patch is
## floating free of a source file, and the priority rule alone (not script-run ORDER) determines every value.
## What this does NOT verify: that each source .rds file itself is correctly derived from the original raw/scanned/downloaded source
## (that is what 02*_house_long_*.R's check_long_vs_source() acceptance tests are for, run per source) or that re-running the ~30
## download/parse scripts from raw inputs reproduces today's source files byte for byte (those depend on external sites and are not
## re-verified here). This script closes the gap BETWEEN "all source files" and "the released panel".
##
## Run after any change to a source file or to elect_cty_final.rds itself, right after R/build_provenance.R.
## Outputs: prints a diff (should be empty); R/output/elect_cty_final_REBUILT.rds (the rebuilt panel, for inspection).
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
stopifnot(setequal(names(panel), c("year", "cty_fips", "sample", "demovote", "repuvote", "totalvote", "state")))

files <- list.files(OUTPUT_DIR, pattern = "^elect_(he_cty_.*|pe_cty_.*|se_cty_.*)\\.rds$", full.names = TRUE)
files <- files[basename(files) != "elect_cty_medsl.rds"]   # union file, not an independent source
label <- sub("\\.rds$", "", basename(files))
prio <- dplyr::case_when(
  grepl("^elect_(he|pe|se)_cty_[a-z_]+_[0-9]{4}$", label) ~ 0L,
  grepl("^elect_he_cty_medsl$|^elect_pe_cty_medsl$|^elect_se_cty_medsl$", label) ~ 2L,
  grepl("historical$", label) & grepl("^elect_(pe|se)_", label) ~ 3L,
  TRUE ~ 1L)

src <- purrr::map_dfr(seq_along(files), function(i) {
  d <- readRDS(files[i])
  stopifnot(all(c("year", "cty_fips", "sample", "demovote", "repuvote", "totalvote") %in% names(d)))
  d %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, source = sub("^elect_", "", label[i]), prio = prio[i])
})
message(nrow(src), " source rows from ", length(files), " files")
## a handful of MEDSL rows are NaN (totalvote == 0, e.g. Indiana Senate 2022: the raw MEDSL file for a few counties is all-zero votes; the panel
## uses the official se_in_enr build instead, priority 0, so these never win): drop them here rather than let them win-by-default in a state/year
## that has no other source (they never do today, but the panel would then legitimately have no row for that key -- caught by the diff below, not by a crash)
na_src <- src %>% dplyr::filter(is.na(demovote) | is.na(repuvote) | is.na(totalvote))
if (nrow(na_src)) { message(nrow(na_src), " source rows have NA/NaN shares (zero-vote placeholder rows) and are dropped:"); print(as.data.frame(na_src %>% dplyr::count(source, sample))) }
src <- src %>% dplyr::filter(!is.na(demovote), !is.na(repuvote), !is.na(totalvote))

## one row per (year, cty_fips, sample): lowest prio wins; ties break alphabetically by source (same rule as build_provenance.R's pick())
rebuilt <- src %>% dplyr::arrange(year, cty_fips, sample, prio, source) %>%
  dplyr::group_by(year, cty_fips, sample) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
  dplyr::transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)

## a source key can legitimately win for MULTIPLE candidate rows tying at the SAME prio+value (already collapsed by group_by/slice); flag any where
## the top two contenders at the SAME prio DISAGREE in value (a genuine ambiguity, not just an alphabetical tiebreak among identical rows)
amb <- src %>% dplyr::group_by(year, cty_fips, sample) %>%
  dplyr::filter(prio == min(prio)) %>%
  dplyr::summarise(n_distinct_vals = dplyr::n_distinct(round(demovote, 9), round(repuvote, 9), round(totalvote, 9)), .groups = "drop") %>%
  dplyr::filter(n_distinct_vals > 1)
message("ambiguous keys (same top priority, different values): ", nrow(amb))
if (nrow(amb)) print(as.data.frame(amb %>% dplyr::mutate(st = cty_fips %/% 1000) %>% dplyr::count(sample, st, year) %>% dplyr::arrange(dplyr::desc(n)) %>% head(15)))

saveRDS(rebuilt, file.path(OUTPUT_DIR, "elect_cty_final_REBUILT.rds"))

## ---- compare rebuilt vs the live panel ------------------------------------------------------------------------------------------------
cmp <- panel %>% dplyr::select(year, cty_fips, sample, demovote, repuvote, totalvote) %>%
  dplyr::full_join(rebuilt %>% dplyr::select(-state), by = c("year", "cty_fips", "sample"), suffix = c(".panel", ".rebuilt"))
missing_from_rebuilt <- cmp %>% dplyr::filter(is.na(demovote.rebuilt))
extra_in_rebuilt <- cmp %>% dplyr::filter(is.na(demovote.panel))
diff_vals <- cmp %>% dplyr::filter(!is.na(demovote.panel), !is.na(demovote.rebuilt)) %>%
  dplyr::filter(abs(demovote.panel - demovote.rebuilt) > 1e-9 | abs(repuvote.panel - repuvote.rebuilt) > 1e-9 | abs(totalvote.panel - totalvote.rebuilt) > 0.5)

message("\n=== REBUILD vs LIVE PANEL ===")
message("panel rows: ", nrow(panel), " | rebuilt rows: ", nrow(rebuilt))
message("keys in panel but not reproducible from any source file: ", nrow(missing_from_rebuilt))
message("keys the sources would add that the panel doesn't have: ", nrow(extra_in_rebuilt))
message("keys where the panel's values differ from the rebuilt (highest-priority source) values: ", nrow(diff_vals))
if (nrow(missing_from_rebuilt)) print(as.data.frame(missing_from_rebuilt %>% dplyr::mutate(st = cty_fips %/% 1000) %>% dplyr::count(sample, st, year) %>% dplyr::arrange(dplyr::desc(n)) %>% head(15)))
if (nrow(extra_in_rebuilt)) print(as.data.frame(extra_in_rebuilt %>% dplyr::mutate(st = cty_fips %/% 1000) %>% dplyr::count(sample, st, year) %>% dplyr::arrange(dplyr::desc(n)) %>% head(15)))
if (nrow(diff_vals)) print(as.data.frame(diff_vals %>% dplyr::mutate(st = cty_fips %/% 1000) %>% dplyr::count(sample, st, year) %>% dplyr::arrange(dplyr::desc(n)) %>% head(15)))

ok <- nrow(missing_from_rebuilt) == 0 && nrow(extra_in_rebuilt) == 0 && nrow(diff_vals) == 0
message(if (ok) "PASS: the panel is reproduced exactly from its source files under the priority rule." else "FAIL: see above.")
