## Provenance sidecar for the master election panel R/output/elect_cty_final.rds.
##
## The panel itself has no source column (kept at 7 columns so downstream merges/Stata-port code are
## unaffected). This script reconstructs, for every panel row, WHERE it came from by matching it
## (key = year + cty_fips + sample, then exact values) against every per-source .rds in R/output.
##
## status:
##   as_source  values identical to a source file's row (source = highest-priority match)
##   modified   the key exists in a source file but the panel's values DIFFER (row was patched in the
##              panel after the source file was written -- e.g. a fix applied directly to the panel);
##              source = the source it was derived from, alt_sources lists any others
##   unmatched  key present in no source file (should be empty; investigate if not)
##
## Source priority when several files hold the identical row: state-specific House builds
## (elect_he_cty_<x>) > MEDSL (elect_{he,pe,se}_cty_medsl) > historical (elect_{pe,se}_cty_historical).
## elect_cty_medsl.rds is skipped (it is the union of the three MEDSL files).
##
## one_party (added 2026-09-20; motivated by Mayda et al.'s Stata rules, see memory): flags panel rows where a major
##   party's share is exactly 0 -- "no_dem" (demovote == 0), "no_rep" (repuvote == 0), "neither_major_party" (both 0),
##   "" otherwise. In House rows this is usually a race with no candidate from that party (independent-only opponent,
##   same-party runoff, unopposed-by-one-party), which the paper's code treats asymmetrically: Republican present but
##   Democrat missing -> demovote set to 0 and KEPT; Republican missing -> DROPPED (drop if repuvote==.). CAUTION: a zero
##   share can also be a party-classification failure (e.g. the MN DFL bug), so treat the flag as "worth checking",
##   not proof of a one-party race. Use it to re-run results with and without these rows.
## Also reports COMPLETENESS (source keys absent from the panel) -> R/output/elect_cty_final_missing_from_panel.csv
## Outputs: R/output/elect_cty_final_provenance.rds  (one row per panel row)
##          R/output/elect_cty_final_provenance_summary.csv  (source x sample x years x rows, human-readable)
##          R/output/elect_cty_final_one_party_summary.csv   (rows with a zero major-party share, by sample x state FIPS x year)
## Companion: R/write_corrections_log.R writes R/output/data_corrections_log.csv, the hand-maintained list of every
## data error found and how it was handled (fixed / replaced / flagged). Every 'modified' row reported here
## should be explainable by an entry in that log; add an entry (and re-run that script) when patching the panel.
## Re-run after ANY change to elect_cty_final.rds. Superseded files (SUPERSEDED_*) are ignored.

source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% select(year, cty_fips, sample, demovote, repuvote, totalvote)

files <- list.files(OUTPUT_DIR, pattern = "^elect_(he_cty_.*|pe_cty_.*|se_cty_.*)\\.rds$", full.names = TRUE)
label <- sub("\\.rds$", "", basename(files))
## Among sources holding the IDENTICAL row, the more specific (per-year) official build wins over a state-wide file: a build named elect_he_cty_<state>_<YYYY>
## (Texas SOS, Virginia DoE, Indiana/Kansas/... official reports) beats the older OpenElections-based elect_he_cty_<state>.rds, so the long table takes the candidate
## detail from the official document (added 2026-09-21; values are identical by construction, so the master check is unaffected).
prio <- dplyr::case_when(
  grepl("^elect_(he|pe|se)_cty_[a-z_]+_[0-9]{4}$", label) ~ 0L,
  grepl("^elect_he_cty_medsl$|^elect_pe_cty_medsl$|^elect_se_cty_medsl$", label) ~ 2L,
  grepl("historical$", label) & grepl("^elect_(pe|se)_", label) ~ 3L,
  TRUE ~ 1L)
src <- purrr::map_dfr(seq_along(files), function(i) {
  d <- readRDS(files[i])
  stopifnot(all(c("year", "cty_fips", "sample", "demovote", "repuvote", "totalvote") %in% names(d)))
  d %>% transmute(year, cty_fips, sample, s_dem = demovote, s_rep = repuvote, s_tot = totalvote,
                  source = sub("^elect_", "", label[i]), prio = prio[i])
})
message(nrow(src), " source rows from ", length(files), " files")

j <- panel %>% inner_join(src, by = c("year", "cty_fips", "sample"), relationship = "many-to-many") %>%
  mutate(same = abs(demovote - s_dem) < 1e-9 & abs(repuvote - s_rep) < 1e-9 & abs(totalvote - s_tot) < 0.5)

pick <- function(df) {
  m <- df[df$same, ]
  if (nrow(m) > 0) {
    m <- m[order(m$prio, m$source), ]
    tibble(source = m$source[1], status = "as_source",
           alt_sources = paste(setdiff(unique(m$source), m$source[1]), collapse = "; "))
  } else {
    a <- df[order(df$prio, df$source), ]
    tibble(source = a$source[1], status = "modified",
           alt_sources = paste(setdiff(unique(a$source), a$source[1]), collapse = "; "))
  }
}
prov <- j %>% group_by(year, cty_fips, sample) %>% group_modify(~ pick(.x)) %>% ungroup()

prov <- panel %>% select(year, cty_fips, sample) %>% left_join(prov, by = c("year", "cty_fips", "sample")) %>%
  mutate(status = ifelse(is.na(status), "unmatched", status), source = ifelse(is.na(source), NA_character_, source))
stopifnot(nrow(prov) == nrow(panel), !anyDuplicated(prov[, c("year", "cty_fips", "sample")]))

## one-party flag from the panel's own shares
flags <- panel %>% transmute(year, cty_fips, sample,
  no_dem = demovote < 1e-12, no_rep = repuvote < 1e-12,
  one_party = dplyr::case_when(no_dem & no_rep ~ "neither_major_party", no_dem ~ "no_dem", no_rep ~ "no_rep", TRUE ~ ""))
prov <- prov %>% left_join(flags %>% select(year, cty_fips, sample, one_party), by = c("year", "cty_fips", "sample"))
stopifnot(!anyNA(prov$one_party))
write.csv(flags %>% mutate(state_fips = cty_fips %/% 1000) %>% filter(one_party != "") %>%
            group_by(sample, state_fips, year, one_party) %>% summarise(rows = n(), .groups = "drop") %>%
            arrange(sample, state_fips, year, one_party),
          file.path(OUTPUT_DIR, "elect_cty_final_one_party_summary.csv"), row.names = FALSE)
save_step(prov, "elect_cty_final_provenance")

summ <- prov %>% group_by(sample, source, status) %>%
  summarise(rows = n(), first_year = min(year), last_year = max(year), .groups = "drop") %>% arrange(sample, source, status)
write.csv(summ, file.path(OUTPUT_DIR, "elect_cty_final_provenance_summary.csv"), row.names = FALSE)

## ---- COMPLETENESS: source rows whose key (year, county, sample) is missing from the panel altogether ----------------------
## (added 2026-09-20 after finding that WI/WV/AR/WA/WY 2016-2024 and VT 2018-2024 House rows had been dropped by an earlier
## fold-in while still sitting in elect_he_cty_medsl.rds; neither the coverage tracker nor the provenance check could see holes)
miss_src <- src %>% filter(is.finite(s_dem), is.finite(s_rep), s_tot > 0) %>%
  anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
write.csv(miss_src %>% mutate(state_fips = cty_fips %/% 1000) %>% group_by(source, sample, state_fips) %>%
            summarise(rows = n(), years = paste(sort(unique(year)), collapse = ","), .groups = "drop") %>% arrange(desc(rows)),
          file.path(OUTPUT_DIR, "elect_cty_final_missing_from_panel.csv"), row.names = FALSE)
message("\n== source rows missing from the panel: ", nrow(miss_src), " (should be 0 or explainable; see elect_cty_final_missing_from_panel.csv)")
if (nrow(miss_src) > 0) print(as.data.frame(miss_src %>% mutate(st = cty_fips %/% 1000) %>% group_by(source, sample, st) %>%
        summarise(rows = n(), years = paste(sort(unique(year)), collapse = ","), .groups = "drop") %>% arrange(desc(rows)) %>% head(25)))

message("\n== status counts =="); print(table(prov$status))
message("\n== one_party flag by sample ==")
print(prov %>% group_by(sample) %>% summarise(rows = n(), no_dem = sum(one_party == "no_dem"), no_rep = sum(one_party == "no_rep"),
                                                neither = sum(one_party == "neither_major_party"), pct_flagged = round(100 * mean(one_party != ""), 2)))
message("\n== modified / unmatched rows (should be explainable) ==")
print(prov %>% filter(status != "as_source") %>% group_by(sample, source, status) %>%
        summarise(rows = n(), years = paste(sort(unique(year)), collapse = ","), states = paste(sort(unique(cty_fips %/% 1000)), collapse = ","), .groups = "drop"),
      n = 60, width = 200)
