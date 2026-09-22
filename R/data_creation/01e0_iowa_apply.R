## Fold Iowa U.S. House 1990/1992/1994/1996/1998 (elect_he_cty_ia_<year>.rds; see 02aj_house_ia_1990_1996_1998.R and 02ak_house_ia_1992_1994.R,
## both from scanned Iowa SOS canvass PDFs the user supplied) into the panel -- every row here is ADDED, none replaced (Iowa House had no rows
## before 2000; 2000-2014 via OpenElections, 2016+ via MEDSL). ALSO merges elect_he_cty_ia_2008_supp.rds (02al_house_ia_2008_supplement.R),
## the 10 southwest-Iowa counties missing from the existing 2008 OpenElections-sourced rows (89/99) -- this is an ADD to 2008, not a replace.
## Gate: no existing IA House rows for 1990/1992/1994/1996/1998; existing 2008 rows are exactly 89 and share no county_fips with the supplement.
## Backup in scratchpad backup_ia/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_ia"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ia_bluebook.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ia_bluebook.rds"))

yrs <- c(1990, 1992, 1994, 1996, 1998)
is_ia_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 19 & panel$year %in% yrs
stopifnot(sum(is_ia_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_ia_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 99 * length(yrs))

## -- 2008 supplement: 10 new counties added to the existing 89 -----------------------------------------------------------------------------
supp <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ia_2008_supp.rds")) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
existing_2008 <- panel %>% filter(sample == "HE", cty_fips %/% 1000 == 19, year == 2008)
stopifnot(nrow(existing_2008) == 89, nrow(supp) == 10, length(intersect(existing_2008$cty_fips, supp$cty_fips)) == 0)
message("2008: existing ", nrow(existing_2008), " + supplement ", nrow(supp), " = ", nrow(existing_2008) + nrow(supp))

add <- bind_rows(new, supp) %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new) + nrow(supp))                                # every row is genuinely new
message("new IA House rows to add: ", nrow(add), " (", length(yrs), " years x 99 counties + 10 2008 supplement counties)")

p2 <- bind_rows(panel, add[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample)
other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 19 & (year %in% yrs | (year == 2008 & cty_fips %in% supp$cty_fips)))) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
