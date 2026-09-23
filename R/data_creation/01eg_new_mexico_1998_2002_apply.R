## Fold New Mexico House 1998 (new) and 2002 (replaces OpenElections' 32/33) into the panel -- see
## 01ee_house_county_new_mexico_sos_archive.R. 1998: 33 new keys, none previously in the panel.
## 2002: 32 existing keys replaced (values unchanged -- verified exact match against OpenElections
## in 01ee) + 1 new key (Cibola, closing the known OpenElections gap).
## Gate: exact key counts asserted below; everything outside NM House 1998/2002 checked unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_nm_1998_2002"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_nm_1998_2002.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_nm_1998_2002.rds"))

nm_1998 <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_nm_1998.rds")) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
nm_2002 <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_nm_2002.rds")) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(nm_1998) == 33, nrow(nm_2002) == 33)

is_nm_1998 <- panel$sample == "HE" & panel$year == 1998 & panel$cty_fips %/% 1000 == 35
is_nm_2002 <- panel$sample == "HE" & panel$year == 2002 & panel$cty_fips %/% 1000 == 35
stopifnot(sum(is_nm_1998) == 0, sum(is_nm_2002) == 32, all(panel$cty_fips[is_nm_2002] %in% nm_2002$cty_fips))

p2 <- panel %>% filter(!is_nm_1998, !is_nm_2002) %>% bind_rows(nm_1998[, names(panel)], nm_2002[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

other0 <- panel %>% filter(!is_nm_1998, !is_nm_2002) %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & year %in% c(1998, 2002) & cty_fips %/% 1000 == 35)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (NM House 1998: +33 new keys; NM House 2002: 32 -> 33 keys)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
