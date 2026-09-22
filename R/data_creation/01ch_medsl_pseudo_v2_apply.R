## Apply the EXTENDED pseudo-row list (01a PSEUDO_RE, 2026-09-20) to ALL MEDSL-sourced House and Senate rows in the panel. New patterns: "Total Ballots Cast"
## (Maine 2024, 835,904 votes: the cause of Maine's doubled totals), "Affidavit", "Absentee / Military", "Over" (New York), "Blank Ballots", "Blank/void",
## "Undervotes-Voids", "Federal (Ballots)", "Not Qualified", "Special Votes", "Contest Totals", "Spoiled", "Invalid", ... Write-ins and "scattering" are real votes and stay.
## Same structure as 01cg: gate (the panel's MEDSL rows must equal the previous targets in medsl_final_targets_v2.rds), then replace/remove and refresh the MEDSL source files.
## Outputs: R/output/medsl_final_targets_v3.rds, medsl_pseudo_v2_changes.csv, medsl_pseudo_v2_removed_rows.csv, updated panel.

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
tf <- file.path(OUTPUT_DIR, "medsl_final_targets_v3.rds")
if (file.exists(tf) && !identical(Sys.getenv("RECOMPUTE"), "1")) { message("reusing saved final targets"); all_f <- readRDS(tf)
} else {
  all_f <- purrr::pmap_dfr(expand_grid(office = c("house", "senate"), year = c(2016, 2018, 2020, 2022, 2024), v = names(FINAL)), function(office, year, v) {
    message(office, " ", year, " ", v)
    read_precinct_file(file.path(RAW_DIR, paste0(office, "_", year, ".raw")), year, party_fallback = TRUE, fill_blank_party = FINAL[[v]]$fill,
                       exclude_pseudo = TRUE, candidate_level_party = FINAL[[v]]$cand, exclude_special = TRUE, exclude_other_offices = TRUE) %>%
      to_wide(ifelse(office == "house", "HE", "SE")) %>% mutate(variant = v)
  })
  save_step(all_f, "medsl_final_targets_v3")
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
oldf <- readRDS(file.path(OUTPUT_DIR, "medsl_final_targets_v2.rds"))          # the previous final targets (01cg)
prev <- bind_rows(oldf %>% filter(variant == "F_std") %>% mutate(st = cty_fips %/% 1000) %>% anti_join(scope, by = c("st", "year")),
                  oldf %>% filter(variant == "F_full") %>% mutate(st = cty_fips %/% 1000) %>% semi_join(scope, by = c("st", "year"))) %>% select(-variant, -st)
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
write.csv(summ, file.path(OUTPUT_DIR, "medsl_pseudo_v2_changes.csv"), row.names = FALSE)
write.csv(removed %>% transmute(year, cty_fips, sample, state_fips = cty_fips %/% 1000, demovote = demovote.old, repuvote = repuvote.old, totalvote = totalvote.old),
          file.path(OUTPUT_DIR, "medsl_pseudo_v2_removed_rows.csv"), row.names = FALSE)
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
