## Fold Vermont U.S. House 1990-2010 (elect_he_cty_vt_<year>.rds, from electionarchive.vermont.gov; see 02ac_house_vt_1990_2010.R) into the panel.
## VT House had NO rows before 2012 (OpenElections only covers 2012/2014, folded earlier via 01ax) -- every row here is ADDED, none replaced.
## Gate: no existing VT House rows before 2012; afterwards, everything outside these new rows is checked to be unchanged. Backup in scratchpad backup_vt/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_vt"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_vt_1990_2010.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_vt_1990_2010.rds"))

is_vt_pre2012 <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 50 & panel$year < 2012
stopifnot(sum(is_vt_pre2012) == 0)

yrs <- seq(1990, 2010, 2)
new <- purrr::map_dfr(sprintf("elect_he_cty_vt_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 14 * 11)

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))                                             # every row is genuinely new
message("new VT House rows to add: ", nrow(add), " (", length(yrs), " years x 14 counties)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 50 & year < 2012)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
