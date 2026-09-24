## Shared panel-fold helper for the state apply scripts written from 2026-09-24 on. apply_state_years(): adds (or replaces) one state's House county rows for the given years from
## R/output/elect_he_cty_<label>_<year>.rds files. Gate: only that state's HE rows of those years change; every other panel row is identical afterwards; the old rows (if any) are reported.
apply_state_years <- function(label, state_fips, years, expected_rows = NULL, allow_replace = TRUE) {
  panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
  bk <- file.path(tempdir(), paste0("backup_", label)); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(file.path(bk, "before.rds"))) saveRDS(panel, file.path(bk, "before.rds"))
  new <- purrr::map_dfr(sprintf("elect_he_cty_%s_%d.rds", label, years), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
  stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), all(new$cty_fips %/% 1000 == state_fips))
  if (!is.null(expected_rows)) stopifnot(nrow(new) == expected_rows)
  is_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == state_fips & panel$year %in% years
  old <- panel[is_target, ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
  message(label, ": existing state House rows in these years: ", sum(is_target), " (", nrow(old), " re-covered, ", sum(abs(old$demovote - old$demovote.n) > 1e-9 | abs(old$repuvote - old$repuvote.n) > 1e-9), " change); new rows: ", nrow(new))
  if (!allow_replace) stopifnot(sum(is_target) == 0)
  p2 <- dplyr::bind_rows(panel[!is_target, ], new[, names(panel)]) %>% dplyr::arrange(year, cty_fips, sample); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
  other0 <- panel %>% dplyr::filter(!is_target) %>% dplyr::arrange(year, cty_fips, sample)
  other1 <- p2 %>% dplyr::filter(!(sample == "HE" & cty_fips %/% 1000 == state_fips & year %in% years)) %>% dplyr::arrange(year, cty_fips, sample)
  stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
  saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
  invisible(p2)
}
