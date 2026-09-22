## Replace Utah U.S. House 2012/2014 (elect_he_cty_ut_2012.rds / elect_he_cty_ut_2014.rds; see 02af_house_ut_2012_2014.R, the state's own
## official canvass workbooks) in the panel. The existing rows came from OpenElections (01aw/02w_house_long_utah.R) and were genuinely
## partial -- 15 of 29 counties in 2012, 25 of 29 in 2014 (missing per-county files in that repo, not a parsing issue). The new source has
## full 29/29 both years, every district tying exactly to its own printed total.
## Gate: existing UT 2012/2014 rows must be <=29 (the known partial state) before anything is written; afterwards, everything outside UT
## 2012/2014 House is checked to be unchanged. Backup in scratchpad backup_ut2.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_ut2"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ut_2012_2014.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ut_2012_2014.rds"))

is_ut_1214 <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 49 & panel$year %in% c(2012, 2014)
stopifnot(sum(is_ut_1214) <= 58, sum(is_ut_1214) > 0)
old_counts <- panel %>% filter(is_ut_1214) %>% count(year)
message("existing UT 2012/2014 rows (to be replaced): ", sum(is_ut_1214)); print(old_counts)

new <- purrr::map_dfr(sprintf("elect_he_cty_ut_%d.rds", c(2012, 2014)), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 29 * 2)

p2 <- panel %>% filter(!is_ut_1214); p2 <- bind_rows(p2, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!is_ut_1214) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 49 & year %in% c(2012, 2014))) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
