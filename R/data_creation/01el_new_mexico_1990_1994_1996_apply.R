## Fold New Mexico House 1990 (33/33), 1994 (31/33, Harding+Hidalgo missing from the source), and
## 1996 (32/33, Harding missing from the source) into the panel -- see
## 01ek_house_county_new_mexico_scanned_canvass.R. Every row here is ADDED, none replaced.
## Gate: no existing NM House rows for 1990/1994/1996; afterwards, everything outside them is unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_nm_1990_1994_1996"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_nm_1990_1994_1996.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_nm_1990_1994_1996.rds"))

yrs <- c(1990, 1994, 1996)
new <- purrr::map_dfr(sprintf("elect_he_cty_nm_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 33 + 31 + 32, !anyDuplicated(new[, c("year", "cty_fips", "sample")]))

is_nm_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 35 & panel$year %in% yrs
stopifnot(sum(is_nm_target) == 0)

p2 <- panel %>% filter(!is_nm_target) %>% bind_rows(new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

other0 <- panel %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 35 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (NM House 1990: +33, 1994: +31, 1996: +32)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
