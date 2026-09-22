## Fold Wisconsin U.S. House 1990/1992/1994/1996/1998/2004 (elect_he_cty_wi_<year>.rds; see 02ag_house_wi_1990_1996.R and
## 02ah_house_wi_1998_2004.R, both from Wisconsin Blue Book scanned pages the user supplied) into the panel. Wisconsin House had NO rows
## for these 6 years (2000/2002/2006-2014 via OpenElections, 01ba; 2016+ via MEDSL) -- every row here is ADDED, none replaced.
## Gate: no existing Wisconsin House rows for these years; afterwards, everything outside them is checked to be unchanged.
## Backup in scratchpad backup_wi/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_wi"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_wi_bluebook.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_wi_bluebook.rds"))

yrs <- c(1990, 1992, 1994, 1996, 1998, 2004)
is_wi_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 55 & panel$year %in% yrs
stopifnot(sum(is_wi_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_wi_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 72 * length(yrs))

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))                                             # every row is genuinely new
message("new WI House rows to add: ", nrow(add), " (", length(yrs), " years x 72 counties)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 55 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
