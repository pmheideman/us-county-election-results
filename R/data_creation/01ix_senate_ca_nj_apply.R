## Fold U.S. Senate California 2022 (added: MEDSL has no California 2022 Senate rows) and New Jersey 2024 (replaced: MEDSL lacks Sussex County) from
## elect_se_cty_ca_sos_2022.rds / elect_se_cty_nj_2024.rds (02b3_senate_ca_nj_official.R) into the panel. 2026-09-24.
## Gate: only these SE state-years change; positive dem/rep medians; every other panel row identical afterwards.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
new <- bind_rows(readRDS(file.path(OUTPUT_DIR, "elect_se_cty_ca_sos_2022.rds")), readRDS(file.path(OUTPUT_DIR, "elect_se_cty_nj_2024.rds"))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 58 + 21, !anyDuplicated(new[, c("year", "cty_fips", "sample")]), all(new$totalvote > 0), median(new$demovote) > 0.2, median(new$repuvote) > 0.2)
blk <- function(d) d$sample == "SE" & ((d$year == 2022 & d$cty_fips %/% 1000 == 6) | (d$year == 2024 & d$cty_fips %/% 1000 == 34))
old <- panel[blk(panel), ] %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".n"))
message("existing rows in these state-years: ", sum(blk(panel)), " (changed: ", sum(abs(old$demovote - old$demovote.n) > 1e-9 | abs(old$repuvote - old$repuvote.n) > 1e-9), "); new rows: ", nrow(new))
p2 <- bind_rows(panel[!blk(panel), ], new[, names(panel)]) %>% arrange(year, cty_fips, sample); stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
stopifnot(isTRUE(all.equal(panel[!blk(panel), ] %>% arrange(year, cty_fips, sample), p2[!blk(p2), ] %>% arrange(year, cty_fips, sample), check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
