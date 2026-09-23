## Fold the WV Blue Book 1990/1992/1994/1996/1998 (brand new) and 2010 (extends 31->55 counties)
## House builds into the panel -- see 01e5/01e6/02p_..._2010/02p_..._1990_1998 earlier this session.
## 2012 was already folded in by 01e4; this closes the rest of the WV House gap.
##
## Net effect: WV House 1990/1992/1994/1996/1998 -- 55 new keys each (275 total), none previously
## in the panel (OpenElections' WV repo starts at 2000). WV House 2010 -- 31 existing keys replaced
## (all differ; see 01e5's header for why), 24 new keys added (55 total).
## Gate: exact key counts asserted below; everything outside WV House 1990/92/94/96/98/2010 checked unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_wv_1990s_2010"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_wv_1990s_2010.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_wv_1990s_2010.rds"))

new_years <- c(1990, 1992, 1994, 1996, 1998)
new_rows <- purrr::map_dfr(new_years, function(y) {
  readRDS(file.path(OUTPUT_DIR, paste0("elect_he_cty_wv_bluebook_", y, ".rds")))
}) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(new_rows) == 55 * length(new_years), !anyDuplicated(new_rows[, c("year", "cty_fips", "sample")]))

is_wv_new_years <- panel$sample == "HE" & panel$year %in% new_years & panel$cty_fips %/% 1000 == 54
stopifnot(sum(is_wv_new_years) == 0)   # genuinely new, nothing to replace

wv_2010_bb <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2010.rds")) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(wv_2010_bb) == 55)
is_wv_2010 <- panel$sample == "HE" & panel$year == 2010 & panel$cty_fips %/% 1000 == 54
stopifnot(sum(is_wv_2010) == 31, all(panel$cty_fips[is_wv_2010] %in% wv_2010_bb$cty_fips))

p2 <- panel %>% filter(!is_wv_new_years, !is_wv_2010) %>%
  bind_rows(new_rows[, names(panel)], wv_2010_bb[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

other0 <- panel %>% filter(!is_wv_new_years, !is_wv_2010) %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & (year %in% new_years | year == 2010) & cty_fips %/% 1000 == 54)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (WV House 1990/92/94/96/98: +", nrow(new_rows), " new keys; ",
        "WV House 2010: 31 -> 55 keys)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
