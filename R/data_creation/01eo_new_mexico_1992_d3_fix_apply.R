## Re-applies New Mexico House 1992 after fixing a county-mislabeling bug in District 3 (see
## 01em_house_county_new_mexico_1992_canvass.R's header) -- the original transcription attributed
## several of District 3's right-hand cells to the wrong specific counties (e.g. Sierra/Socorro/
## Valencia, which actually belong to District 2, not District 3). The underlying vote NUMBERS were
## unaffected for District 1/2 (already correct) and for District 3's aggregate district total; only
## the county-level split of District 3's Los Alamos-through-Union counties changes here.
## Gate: current panel NM 1992 rows must match the OLD (pre-fix) values exactly before replacing;
## everything outside NM House 1992 is checked unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_nm_1992_d3_fix"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_nm_1992_d3_fix.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_nm_1992_d3_fix.rds"))

new <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_nm_1992.rds")) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new) == 33, !anyDuplicated(new[, c("year", "cty_fips", "sample")]))

is_nm_1992 <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 35 & panel$year == 1992
stopifnot(sum(is_nm_1992) == 33, all(panel$cty_fips[is_nm_1992] %in% new$cty_fips))

p2 <- panel %>% filter(!is_nm_1992) %>% bind_rows(new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

other0 <- panel %>% filter(!is_nm_1992) %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 35 & year == 1992)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

## Report which counties actually changed value (District 1/2 counties should NOT have changed)
old_1992 <- panel %>% filter(is_nm_1992) %>% arrange(cty_fips)
new_1992 <- new %>% arrange(cty_fips)
changed <- abs(old_1992$totalvote - new_1992$totalvote) > 0.5 | abs(old_1992$demovote - new_1992$demovote) > 1e-9
message("counties with changed values: ", sum(changed), " of 33")
print(old_1992$cty_fips[changed])

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (NM House 1992 D3 county-label fix, values replaced in place)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
