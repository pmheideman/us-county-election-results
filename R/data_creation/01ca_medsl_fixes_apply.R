## Apply the 2026-09-20 MEDSL classification fixes to the master panel (standalone patch, NOT a re-run of 01a/01b, which
## would risk clobbering the accumulated panel -- same approach as the earlier Minnesota patch).
##
## Uses the EXACT read_precinct_file() from 01a (extracted by parsing the file, so there is one source of truth) in four
## modes and compares them:
##   V0 legacy      party_fallback=F fill_blank_party=F exclude_pseudo=F   -> must reproduce the current MEDSL-sourced panel rows
##   V1 party       party_fallback=T fill_blank_party=F exclude_pseudo=F   -> party-classification fixes only
##   V2 full        party_fallback=T fill_blank_party=T exclude_pseudo=T candidate_level_party=T -> + blank-party fill (+ external overrides) + pseudo-row exclusion + candidate-level party
##   V3 party+pseudo party_fallback=T fill_blank_party=F exclude_pseudo=T  -> only used to MEASURE the pseudo-row effect elsewhere
##
## What is applied to the panel (only rows whose provenance is MEDSL; rows from other sources are never touched):
##   * V2 (full) for the state-years the user asked to fix: New Jersey 2024 and Oregon 2024 (House and Senate).
##   * V1 (party fixes only) everywhere else it changes anything -- unambiguous classification corrections:
##     ND DEMOCRATIC-NPL 2020/2022, VT 2024 REPUBLICAN/LIBERTARIAN, NY 2024 REPUBLICAN/CONSERVATIVE (Oregon 2024 via V2).
##   * The pseudo-row exclusion for all OTHER state-years (V3 vs V1) is only REPORTED here, not applied -- it touches many more
##     rows and needs the user's decision.
## Outputs: R/output/medsl_corrected_variants.rds (all four variants, long), R/output/medsl_fix_changes_applied.csv,
##          R/output/medsl_pseudo_row_effect_not_applied.csv, and the updated R/output/elect_cty_final.rds.

source(file.path("R", "00_setup.R"))
library(readr)

exprs <- parse(file = file.path("R", "data_creation", "01a_election_data_medsl.R"))
keep <- vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
                 as.character(e[[2]]) %in% c("RAW_DIR", "detect_delim", "PSEUDO_RE", "read_precinct_file"), NA)
eval(exprs[keep])

VARIANTS <- list(V0 = c(FALSE, FALSE, FALSE, FALSE), V1 = c(TRUE, FALSE, FALSE, FALSE), V2 = c(TRUE, TRUE, TRUE, TRUE), V3 = c(TRUE, FALSE, TRUE, FALSE))
to_wide <- function(df, sample_code) {
  df %>% group_by(year, county_fips) %>%
    summarise(demovote_n = sum(votes[party_std == "DEMOCRAT"], na.rm = TRUE), repuvote_n = sum(votes[party_std == "REPUBLICAN"], na.rm = TRUE),
              totalvote = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, sample = sample_code) %>%
    select(year, cty_fips = county_fips, sample, demovote, repuvote, totalvote)
}

runs <- expand_grid(office = c("house", "senate"), year = c(2016, 2018, 2020, 2022, 2024), variant = names(VARIANTS))
if (file.exists(file.path(OUTPUT_DIR, "medsl_corrected_variants.rds")) && !identical(Sys.getenv("RECOMPUTE"), "1")) {
  message("reusing saved variants (set RECOMPUTE=1 to rebuild)"); all_v <- readRDS(file.path(OUTPUT_DIR, "medsl_corrected_variants.rds"))
} else all_v <- purrr::pmap_dfr(runs, function(office, year, variant) {
  o <- VARIANTS[[variant]]
  message(office, " ", year, " ", variant)
  read_precinct_file(file.path(RAW_DIR, paste0(office, "_", year, ".raw")), year,
                     party_fallback = o[1], fill_blank_party = o[2], exclude_pseudo = o[3], candidate_level_party = o[4]) %>%
    to_wide(ifelse(office == "house", "HE", "SE")) %>% mutate(variant = variant)
})
save_step(all_v, "medsl_corrected_variants")

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
prov <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final_provenance.rds")) %>% select(year, cty_fips, sample, source)
medsl_keys <- prov %>% filter(source %in% c("he_cty_medsl", "se_cty_medsl")) %>% select(year, cty_fips, sample)

same <- function(a, b) abs(a$demovote - b$demovote) < 1e-9 & abs(a$repuvote - b$repuvote) < 1e-9 & abs(a$totalvote - b$totalvote) < 0.5

## ---- 1. VALIDATION: V0 must reproduce the current MEDSL-sourced panel rows exactly ------------------------------------
v0 <- all_v %>% filter(variant == "V0") %>% select(-variant)
chk <- medsl_keys %>% inner_join(panel %>% select(year, cty_fips, sample, demovote, repuvote, totalvote), by = c("year", "cty_fips", "sample")) %>%
  inner_join(v0, by = c("year", "cty_fips", "sample"), suffix = c("", ".v0"))
chk$ok <- abs(chk$demovote - chk$demovote.v0) < 1e-9 & abs(chk$repuvote - chk$repuvote.v0) < 1e-9 & abs(chk$totalvote - chk$totalvote.v0) < 0.5
message("\n== VALIDATION: legacy re-computation vs current MEDSL-sourced panel rows: ", sum(chk$ok), " identical of ", nrow(chk),
        " (", nrow(medsl_keys) - nrow(chk), " panel MEDSL keys not recomputed)")
bad <- chk %>% filter(!ok)
## The ONLY expected disagreement: Minnesota 2020 SENATE. The earlier DFL patch replaced MN House 2016/2020/2022 but never the
## Senate, so the panel still has demovote = 0 there while the (already DFL-aware) legacy function gives the right value.
known <- bad %>% filter(sample == "SE", year == 2020, cty_fips %/% 1000 == 27)
if (nrow(bad) != nrow(known)) { print(as.data.frame(bad %>% count(year, sample, st = cty_fips %/% 1000))); stop("unexplained disagreement between legacy re-computation and panel: aborting before any change") }
message("NOTE: ", nrow(known), " Minnesota 2020 Senate rows still carry the DFL zero-Democrat bug in the panel; the corrected target replaces them")

## ---- 2. target values ------------------------------------------------------------------------------------------------------
FULL_SCOPE <- tibble(st = c(34, 41), year = c(2024, 2024))       # New Jersey, Oregon 2024
v1 <- all_v %>% filter(variant == "V1") %>% select(-variant)
v2 <- all_v %>% filter(variant == "V2") %>% select(-variant)
target <- bind_rows(
  v1 %>% mutate(st = cty_fips %/% 1000) %>% anti_join(FULL_SCOPE, by = c("st", "year")) %>% select(-st),
  v2 %>% mutate(st = cty_fips %/% 1000) %>% semi_join(FULL_SCOPE, by = c("st", "year")) %>% select(-st))
target <- target %>% filter(is.finite(demovote), is.finite(repuvote), totalvote > 0)

chg <- medsl_keys %>% inner_join(panel %>% select(year, cty_fips, sample, demovote, repuvote, totalvote), by = c("year", "cty_fips", "sample")) %>%
  inner_join(target, by = c("year", "cty_fips", "sample"), suffix = c(".old", ".new")) %>%
  filter(!(abs(demovote.old - demovote.new) < 1e-9 & abs(repuvote.old - repuvote.new) < 1e-9 & abs(totalvote.old - totalvote.new) < 0.5))
summ <- chg %>% mutate(st = cty_fips %/% 1000) %>% group_by(sample, st, year) %>%
  summarise(rows = n(), mean_dem_old = round(mean(demovote.old), 3), mean_dem_new = round(mean(demovote.new), 3),
            mean_rep_old = round(mean(repuvote.old), 3), mean_rep_new = round(mean(repuvote.new), 3),
            median_total_ratio = round(median(totalvote.new / totalvote.old), 3), .groups = "drop")
write.csv(summ, file.path(OUTPUT_DIR, "medsl_fix_changes_applied.csv"), row.names = FALSE)
message("\n== rows to change: ", nrow(chg))
print(as.data.frame(summ))

## ---- 3. apply --------------------------------------------------------------------------------------------------------------------
new_rows <- chg %>% transmute(year, cty_fips, sample, demovote = demovote.new, repuvote = repuvote.new, totalvote = totalvote.new, state = NA_character_)
new_rows <- new_rows[, names(panel)]
panel2 <- panel %>% anti_join(new_rows %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample")) %>% bind_rows(new_rows)
stopifnot(nrow(panel2) == nrow(panel), anyDuplicated(panel2[, c("year", "cty_fips", "sample")]) == 0,
          !anyNA(panel2[, c("demovote", "repuvote", "totalvote")]))
saveRDS(panel2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
message("panel updated: ", nrow(new_rows), " rows replaced; panel rows ", nrow(panel2))

## ---- 3b. RESTORE MEDSL rows that are missing from the panel altogether ------------------------------------------------------
## Found 2026-09-20: whole states' 2016-2024 House rows (WI, WV, AR, WA, WY; VT 2018-2024) exist in elect_he_cty_medsl.rds but were
## dropped from the panel by an earlier fold-in (it removed each state's House rows and re-added only its own years).
panel2 <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
miss <- target %>% anti_join(panel2 %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample")) %>%
  mutate(state = NA_character_)
miss <- miss[, names(panel2)]
message("\n== MEDSL rows missing from the panel, restored: ", nrow(miss))
print(as.data.frame(miss %>% mutate(st = cty_fips %/% 1000) %>% group_by(sample, st) %>% summarise(rows = n(), years = paste(sort(unique(year)), collapse = ","), .groups = "drop") %>% arrange(desc(rows))))
panel3 <- bind_rows(panel2, miss)
stopifnot(anyDuplicated(panel3[, c("year", "cty_fips", "sample")]) == 0, !anyNA(panel3[, c("demovote", "repuvote", "totalvote")]))
saveRDS(panel3, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
message("panel rows now ", nrow(panel3))

## ---- 4. REPORT (not applied): pseudo-row exclusion elsewhere (V3 vs V1) and blank-fill elsewhere (V2 vs V3) ------------------------------
v3 <- all_v %>% filter(variant == "V3") %>% select(-variant)
eff <- medsl_keys %>% inner_join(v1, by = c("year", "cty_fips", "sample")) %>%
  inner_join(v3, by = c("year", "cty_fips", "sample"), suffix = c(".v1", ".v3")) %>% mutate(st = cty_fips %/% 1000) %>%
  anti_join(FULL_SCOPE, by = c("st", "year")) %>%
  filter(!(abs(demovote.v1 - demovote.v3) < 1e-9 & abs(repuvote.v1 - repuvote.v3) < 1e-9 & abs(totalvote.v1 - totalvote.v3) < 0.5)) %>%
  group_by(sample, st, year) %>%
  summarise(rows = n(), mean_abs_dem_change = round(mean(abs(demovote.v3 - demovote.v1)), 4), mean_abs_rep_change = round(mean(abs(repuvote.v3 - repuvote.v1)), 4),
            median_total_ratio = round(median(totalvote.v3 / totalvote.v1), 3), min_total_ratio = round(min(totalvote.v3 / totalvote.v1), 3), .groups = "drop") %>%
  arrange(desc(mean_abs_rep_change))
write.csv(eff, file.path(OUTPUT_DIR, "medsl_pseudo_row_effect_not_applied.csv"), row.names = FALSE)
message("\n== NOT APPLIED: exclude-pseudo-rows effect in other state-years: ", sum(eff$rows), " rows in ", nrow(eff), " state-years (top 15 by mean |repuvote change|)")
print(as.data.frame(head(eff, 15)))
message("\nNOT APPLIED overall: median total ratio ", round(median(eff$median_total_ratio), 3), "; state-years where mean |rep change| > 0.01: ", sum(eff$mean_abs_rep_change > 0.01))
fill_else <- medsl_keys %>% inner_join(v3, by = c("year", "cty_fips", "sample")) %>% inner_join(v2, by = c("year", "cty_fips", "sample"), suffix = c(".v3", ".v2")) %>%
  mutate(st = cty_fips %/% 1000) %>% anti_join(FULL_SCOPE, by = c("st", "year")) %>%
  filter(!(abs(demovote.v3 - demovote.v2) < 1e-9 & abs(repuvote.v3 - repuvote.v2) < 1e-9)) %>% count(sample, st, year, name = "rows_changed_by_blank_fill")
message("blank-party fill would also change (not applied): ", if (nrow(fill_else)) "" else "nothing")
if (nrow(fill_else)) print(as.data.frame(fill_else))
