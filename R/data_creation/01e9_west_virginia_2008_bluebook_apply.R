## Fold WV Blue Book 2008 (extends 48->55 counties) into the panel -- see 01e8's header for why the
## Blue Book supersedes OpenElections' 2008 build (same "OpenElections' WV precinct files are
## themselves incomplete for a number of counties" issue already fixed for 2010 in 01e5/01e7).
## This is the LAST remaining WV House gap; after this fold, WV House is complete 1990-2024.
## Gate: exact key counts asserted below; everything outside WV House 2008 checked unchanged.
source(file.path("R", "00_setup.R"))

panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- file.path(tempdir(), "backup_wv_2008"); dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_wv_2008.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_wv_2008.rds"))

wv_2008_bb <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_wv_bluebook_2008.rds")) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(nrow(wv_2008_bb) == 55)

is_wv_2008 <- panel$sample == "HE" & panel$year == 2008 & panel$cty_fips %/% 1000 == 54
stopifnot(sum(is_wv_2008) == 48, all(panel$cty_fips[is_wv_2008] %in% wv_2008_bb$cty_fips))

p2 <- panel %>% filter(!is_wv_2008) %>% bind_rows(wv_2008_bb[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)

other0 <- panel %>% filter(!is_wv_2008) %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & year == 2008 & cty_fips %/% 1000 == 54)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))

message("panel rows ", nrow(panel), " -> ", nrow(p2), " (WV House 2008: 48 -> 55 keys)")
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
