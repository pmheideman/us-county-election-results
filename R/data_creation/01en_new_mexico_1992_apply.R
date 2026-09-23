## Fold New Mexico House 1992 (33/33; see 01em_house_county_new_mexico_1992_canvass.R) into the
## panel -- the last remaining New Mexico House gap, closing NM House to a complete 1990-2024 record.
## Gate: no existing NM House rows for 1992; afterwards, everything outside it is unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_nm_1992"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_nm_1992.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_nm_1992.rds"))

new <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_nm_1992.rds")) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 33, !anyDuplicated(new[, c("year", "cty_fips", "sample")]))

is_nm_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 35 & panel$year == 1992
stopifnot(sum(is_nm_target) == 0)

p2 <- panel %>% filter(!is_nm_target) %>% bind_rows(new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

other0 <- panel %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 35 & year == 1992)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (NM House 1992: +33 keys)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
