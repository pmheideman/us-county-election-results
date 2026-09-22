## Apply, to ALL MEDSL-sourced House and Senate rows in the panel, the two exclusions decided on 2026-09-20:
##   (1) pseudo-rows (over/under votes, blanks, voids, CONTEST TOTAL, TOTAL VOTES CAST, CAST VOTES, BALLOTS CAST, ...) so that
##       totalvote = votes for candidates. Previously done only for NJ and OR 2024 (01ca).
##   (2) special elections (MEDSL `special` flag): v1 covers regular general elections only. Also fixes a double-count: where a state
##       had a regular AND a special race on the same ballot, the county rows summed both (Senate AZ 2020, CA 2024, GA 2020, MN 2018,
##       MS 2018, NE 2024, OK 2022; House WI-8 2024, MD-7 2020, MI-13 2018, TX-18 2024, ...).
## Targets (both use 01a's exact read_precinct_file(), extracted by parsing the file):
##   F_std   party_fallback=T fill_blank_party=F exclude_pseudo=T candidate_level_party=F exclude_special=T   (every state-year)
##   F_full  party_fallback=T fill_blank_party=T exclude_pseudo=T candidate_level_party=T exclude_special=T   (NJ and OR 2024 only)
## Gate: before changing anything, the current panel's MEDSL-sourced rows must equal the previously applied targets (V1, and V2 for
## NJ/OR 2024) in medsl_corrected_variants.rds; any unexplained difference aborts.
## Rows whose race disappears entirely (e.g. AZ Senate 2020, a special-only year) are REMOVED from the panel and reported.
## Also refreshes elect_he_cty_medsl.rds / elect_se_cty_medsl.rds to the final targets (backed up by the caller), so a future rebuild
## of the panel from those files (01b) reproduces this state.
## Outputs: R/output/medsl_final_targets.rds, R/output/medsl_final_changes.csv, R/output/medsl_final_removed_rows.csv, updated panel.

source(file.path("R", "00_setup.R"))
library(readr)
exprs <- parse(file = file.path("R", "data_creation", "01a_election_data_medsl.R"))
keep <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                 as.character(e[[2]]) %in% c("RAW_DIR", "detect_delim", "PSEUDO_RE", "read_precinct_file"), NA)
eval(exprs[keep])

to_wide <- function(df, sample_code) {
  df %>% group_by(year, county_fips) %>%
    summarise(demovote_n = sum(votes[party_std == "DEMOCRAT"], na.rm = TRUE), repuvote_n = sum(votes[party_std == "REPUBLICAN"], na.rm = TRUE),
              totalvote = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, sample = sample_code) %>%
    select(year, cty_fips = county_fips, sample, demovote, repuvote, totalvote)
}
FINAL <- list(F_std  = list(fill = FALSE, cand = FALSE),
              F_full = list(fill = TRUE,  cand = TRUE))
tf <- file.path(OUTPUT_DIR, "medsl_final_targets.rds")
if (file.exists(tf) && !identical(Sys.getenv("RECOMPUTE"), "1")) { message("reusing saved final targets"); all_f <- readRDS(tf)
} else {
  all_f <- purrr::pmap_dfr(expand_grid(office = c("house", "senate"), year = c(2016, 2018, 2020, 2022, 2024), v = names(FINAL)), function(office, year, v) {
    message(office, " ", year, " ", v)
    read_precinct_file(file.path(RAW_DIR, paste0(office, "_", year, ".raw")), year, party_fallback = TRUE, fill_blank_party = FINAL[[v]]$fill,
                       exclude_pseudo = TRUE, candidate_level_party = FINAL[[v]]$cand, exclude_special = TRUE) %>%
      to_wide(ifelse(office == "house", "HE", "SE")) %>% mutate(variant = v)
  })
  save_step(all_f, "medsl_final_targets")
}
scope <- tibble(st = c(34, 41), year = c(2024, 2024))          # NJ, OR 2024 use the full variant
final <- bind_rows(
  all_f %>% filter(variant == "F_std")  %>% mutate(st = cty_fips %/% 1000) %>% anti_join(scope, by = c("st", "year")),
  all_f %>% filter(variant == "F_full") %>% mutate(st = cty_fips %/% 1000) %>% semi_join(scope, by = c("st", "year"))) %>%
  select(-variant, -st) %>% filter(is.finite(demovote), is.finite(repuvote), totalvote > 0)

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds")) %>% select(year, cty_fips, sample, source)
mk <- prov %>% filter(source %in% c("he_cty_medsl", "se_cty_medsl")) %>% select(year, cty_fips, sample)

## ---- gate: panel MEDSL rows == previously applied targets ------------------------------------------------------------------------
old <- readRDS(file.path(OUTPUT_DIR, "medsl_corrected_variants.rds"))
prev <- bind_rows(old %>% filter(variant == "V1") %>% mutate(st = cty_fips %/% 1000) %>% anti_join(scope, by = c("st", "year")),
                  old %>% filter(variant == "V2") %>% mutate(st = cty_fips %/% 1000) %>% semi_join(scope, by = c("st", "year"))) %>% select(-variant, -st)
g <- mk %>% inner_join(panel %>% select(year, cty_fips, sample, demovote, repuvote, totalvote), by = c("year", "cty_fips", "sample")) %>%
  inner_join(prev, by = c("year", "cty_fips", "sample"), suffix = c("", ".prev"))
bad <- g %>% filter(!(abs(demovote - demovote.prev) < 1e-9 & abs(repuvote - repuvote.prev) < 1e-9 & abs(totalvote - totalvote.prev) < 0.5))
message("GATE: ", nrow(g) - nrow(bad), " of ", nrow(g), " MEDSL-sourced panel rows equal the previously applied targets (",
        nrow(mk) - nrow(g), " MEDSL keys have no previous target)")
if (nrow(bad) > 0) { print(as.data.frame(bad %>% count(year, sample, st = cty_fips %/% 1000))); stop("gate failed: panel differs from previously applied targets") }

## ---- changes and removals ---------------------------------------------------------------------------------------------------------------
cur <- mk %>% inner_join(panel %>% select(year, cty_fips, sample, demovote, repuvote, totalvote), by = c("year", "cty_fips", "sample"))
j <- cur %>% left_join(final, by = c("year", "cty_fips", "sample"), suffix = c(".old", ".new"))
removed <- j %>% filter(is.na(totalvote.new))
changed <- j %>% filter(!is.na(totalvote.new)) %>%
  filter(!(abs(demovote.old - demovote.new) < 1e-9 & abs(repuvote.old - repuvote.new) < 1e-9 & abs(totalvote.old - totalvote.new) < 0.5))
new_only <- final %>% anti_join(cur, by = c("year", "cty_fips", "sample"))
message("MEDSL-sourced rows: ", nrow(cur), " | changed: ", nrow(changed), " | removed (race excluded entirely): ", nrow(removed), " | new keys: ", nrow(new_only))
summ <- changed %>% mutate(st = cty_fips %/% 1000) %>% group_by(sample, st, year) %>%
  summarise(rows = n(), mean_abs_rep_change = round(mean(abs(repuvote.new - repuvote.old)), 4), mean_abs_dem_change = round(mean(abs(demovote.new - demovote.old)), 4),
            median_total_ratio = round(median(totalvote.new / totalvote.old), 3), min_total_ratio = round(min(totalvote.new / totalvote.old), 3), .groups = "drop") %>%
  arrange(desc(mean_abs_rep_change))
write.csv(summ, file.path(OUTPUT_DIR, "medsl_final_changes.csv"), row.names = FALSE)
write.csv(removed %>% transmute(year, cty_fips, sample, state_fips = cty_fips %/% 1000, demovote = demovote.old, repuvote = repuvote.old, totalvote = totalvote.old),
          file.path(OUTPUT_DIR, "medsl_final_removed_rows.csv"), row.names = FALSE)
message("\nstate-years changed: ", nrow(summ), "; top 12 by mean |repuvote change|:"); print(as.data.frame(head(summ, 12)))
message("overall median total ratio: ", round(median(changed$totalvote.new / changed$totalvote.old), 3),
        "; state-years with mean |rep change| > 0.01: ", sum(summ$mean_abs_rep_change > 0.01))
if (nrow(removed)) { message("\nREMOVED rows by sample/state/year:"); print(as.data.frame(removed %>% count(sample, st = cty_fips %/% 1000, year))) }

## ---- apply to panel -------------------------------------------------------------------------------------------------------------------------------
upd <- changed %>% transmute(year, cty_fips, sample, demovote = demovote.new, repuvote = repuvote.new, totalvote = totalvote.new, state = NA_character_)
upd <- upd[, names(panel)]
panel2 <- panel %>% anti_join(removed %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample")) %>%
  anti_join(upd %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample")) %>% bind_rows(upd)
stopifnot(nrow(panel2) == nrow(panel) - nrow(removed), anyDuplicated(panel2[, c("year", "cty_fips", "sample")]) == 0,
          !anyNA(panel2[, c("demovote", "repuvote", "totalvote")]))
saveRDS(panel2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
message("\npanel rows ", nrow(panel), " -> ", nrow(panel2))

## ---- refresh the MEDSL source files to the final targets ---------------------------------------------------------------------------------
for (s in c("HE", "SE")) {
  f <- file.path(OUTPUT_DIR, ifelse(s == "HE", "elect_he_cty_medsl.rds", "elect_se_cty_medsl.rds"))
  saveRDS(final %>% filter(sample == s) %>% arrange(cty_fips, year), f)
}
message("refreshed elect_he_cty_medsl.rds and elect_se_cty_medsl.rds to the final targets")

## ---- sanity: House / Senate total vs presidential total, same county, presidential years ----------------------------------------------------------
pe <- panel2 %>% filter(sample == "PE", year %in% c(2016, 2020, 2024)) %>% select(year, cty_fips, pe = totalvote)
r <- panel2 %>% inner_join(mk, by = c("year", "cty_fips", "sample")) %>% filter(year %in% c(2016, 2020, 2024)) %>% inner_join(pe, by = c("year", "cty_fips")) %>%
  mutate(ratio = totalvote / pe)
message("\n== MEDSL House/Senate total / presidential total, presidential years (after) ==")
print(as.data.frame(r %>% group_by(sample, year) %>% summarise(n = n(), p05 = round(quantile(ratio, .05), 2), median = round(median(ratio), 2), p95 = round(quantile(ratio, .95), 2),
                                                                share_above_1.3 = round(mean(ratio > 1.3), 3), .groups = "drop")))
