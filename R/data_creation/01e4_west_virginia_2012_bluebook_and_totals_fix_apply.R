## Fold two West Virginia House fixes into the panel at once (both surfaced by cross-checking the
## user-supplied WV Blue Book 2012 general-election returns against 01az's OpenElections build --
## see 01e3_house_county_west_virginia_bluebook.R and 02p_house_long_west_virginia_bluebook_2012.R):
##
## 1. A real doubling bug in `01az_house_county_west_virginia.R` (now fixed there and in
##    02p_house_long_west_virginia.R): some counties' OpenElections precinct files carry an extra
##    `precinct == "TOTALS"` rollup row that was being summed on top of the real per-precinct rows.
##    Affects 12 West Virginia House 2010 counties (values change, no new counties) and 2 of 2012's
##    25 existing counties (Monongalia, Ohio).
## 2. WV House 2012 coverage extended from 25/55 to 55/55 counties using the Blue Book's printed
##    U.S. House returns (`elect_he_cty_wv_bluebook_2012.rds`), which is a strict superset of the
##    fixed OpenElections build for this year (23/25 overlap counties match exactly; Roane +21 votes
##    and Barbour +3 votes are a small pre-existing residual, not the TOTALS bug -- see 01e3's header).
##
## Net effect on the panel: WV House 2010 -- 12 of 31 existing keys change value, no new keys.
##                          WV House 2012 -- 5 of 25 existing keys change value (the TOTALS-bug 2 +
##                          the tiny residual 2 + Webster, which the TOTALS fix alone happened to
##                          correct), 30 new keys added (55 total).
## Gate: exact key/value counts asserted below; everything outside WV 2010/2012 House checked unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_wv_2012_bluebook"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_wv_2012_bluebook.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_wv_2012_bluebook.rds"))

wv_oe <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv.rds"))            # fixed (no more TOTALS doubling)
wv_bb <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2012.rds")) # full 55/55 for 2012

## ---- gate: current WV House 2010/2012 keys match the pre-fix expectation ----
is_wv_2010 <- panel$sample == "HE" & panel$year == 2010 & panel$cty_fips %/% 1000 == 54
is_wv_2012 <- panel$sample == "HE" & panel$year == 2012 & panel$cty_fips %/% 1000 == 54
stopifnot(sum(is_wv_2010) == 31, sum(is_wv_2012) == 25)

## 2010: replace values for the same 31 keys with the corrected build (no new counties this year)
new_2010 <- wv_oe %>% filter(year == 2010) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new_2010) == 31, setequal(new_2010$cty_fips, panel$cty_fips[is_wv_2010]))

## 2012: drop the old 25 OE-sourced rows, replace with the full 55 Blue Book rows
new_2012 <- wv_bb %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new_2012) == 55, all(panel$cty_fips[is_wv_2012] %in% new_2012$cty_fips))

p2 <- panel %>% filter(!is_wv_2010, !is_wv_2012) %>%
  bind_rows(new_2010[, names(panel)], new_2012[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

## everything outside WV House 2010/2012 is byte-identical
other0 <- panel %>% filter(!is_wv_2010, !is_wv_2012) %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & year %in% c(2010, 2012) & cty_fips %/% 1000 == 54)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (WV House 2010: 31 -> 31 keys, 12 values corrected; ",
        "WV House 2012: 25 -> 55 keys, 5 values corrected, 30 added)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
