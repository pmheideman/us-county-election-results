## Restore House rows that exist in a per-state source file (elect_he_cty_<x>.rds) but are absent from the panel (keyed by
## year + county + sample). Found by the completeness check in build_provenance.R on 2026-09-20: Arkansas 2002-2014 (230 rows),
## Texas 2016 (7) and Virginia 2016 (1) had been dropped by earlier fold-ins. Only keys with NO panel row are added, so nothing
## already in the panel is overwritten (2016+ MEDSL rows keep priority by convention). Source values are used as-is.
## Deliberately NOT restored: elect_se_cty_historical rows (2017 Alabama special, Indiana 2018 Senate) and pre-1916 presidential rows
## without a county FIPS -- different provenance / out of scope, listed in R/output/elect_cty_final_missing_from_panel.csv.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
files <- list.files(OUTPUT_DIR, pattern = "^elect_he_cty_.*\\.rds$", full.names = TRUE)
files <- files[!grepl("elect_he_cty_medsl|la_sos_2024", files)]     # MEDSL restored separately in 01ca; la_sos_2024 already in the panel
src <- purrr::map_dfr(files, function(f) readRDS(f) %>% mutate(source = sub("^elect_", "", sub("\\.rds$", "", basename(f)))) %>%
                        select(year, cty_fips, sample, demovote, repuvote, totalvote, source))
miss <- src %>% filter(is.finite(demovote), is.finite(repuvote), totalvote > 0, !is.na(cty_fips)) %>%
  anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(!anyDuplicated(miss[, c("year", "cty_fips", "sample")]))
message("restoring ", nrow(miss), " House rows missing from the panel:")
print(as.data.frame(miss %>% mutate(st = cty_fips %/% 1000) %>% group_by(source, st) %>% summarise(rows = n(), years = paste(sort(unique(year)), collapse = ","), .groups = "drop")))
new <- miss %>% select(-source) %>% mutate(state = NA_character_); new <- new[, names(panel)]
panel2 <- bind_rows(panel, new)
stopifnot(anyDuplicated(panel2[, c("year", "cty_fips", "sample")]) == 0, !anyNA(panel2[, c("demovote", "repuvote", "totalvote")]))
saveRDS(panel2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
message("panel rows ", nrow(panel), " -> ", nrow(panel2))
