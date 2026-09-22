## Fold Utah U.S. House 1990-2006 (elect_he_cty_ut_<year>.rds; see 02ad_house_ut_2000_2006.R for the born-digital 2000/2002/2004/2006 canvass PDFs and
## 02ae_house_ut_1990_1998.R for the scanned 1990/1992/1994/1996/1998 canvass PDFs, both from vote.utah.gov/historical-election-results/) into the panel.
## Utah House had NO rows before 2008 (2008/2010 folded earlier via 01bm; 2012+ via OpenElections/MEDSL) -- every row here is ADDED, none replaced.
## Gate: no existing Utah House rows before 2008; afterwards, everything outside these new rows is checked to be unchanged. Backup in scratchpad backup_ut/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_ut"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ut_1990_2006.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ut_1990_2006.rds"))

is_ut_pre2008 <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 49 & panel$year < 2008
stopifnot(sum(is_ut_pre2008) == 0)

yrs <- c(seq(1990, 1998, 2), seq(2000, 2006, 2))
new <- purrr::map_dfr(sprintf("elect_he_cty_ut_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 29 * length(yrs))

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))                                             # every row is genuinely new
message("new UT House rows to add: ", nrow(add), " (", length(yrs), " years x 29 counties)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 49 & year < 2008)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
