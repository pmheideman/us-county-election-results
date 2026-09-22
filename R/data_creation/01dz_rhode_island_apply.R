## Fold Rhode Island U.S. House 1990/1992/1994/1996/1998/2000/2002/2004/2006/2010 (elect_he_cty_ri_<year>.rds; see 02ai_house_ri_1990_2010.R,
## built from elections.ri.gov + www.ri.gov data cached via a browser-automation session that cleared their Cloudflare challenge, cross-checked
## against the RI Board of Elections' own Count Book PDFs) into the panel. Rhode Island House had NO rows for these 10 years (2008/2012/2014
## via OpenElections; 2016+ via MEDSL) -- every row here is ADDED, none replaced. This closes RI's entire remaining House gap.
## Gate: no existing RI House rows for these years; afterwards, everything outside them is checked to be unchanged. Backup in scratchpad backup_ri/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/1d842c35-d902-4a63-85f7-6e0a2e549bad/scratchpad/backup_ri"; dir.create(bk, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(file.path(bk, "elect_cty_final_before_ri_1990_2010.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_ri_1990_2010.rds"))

yrs <- c(1990, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2006, 2010)
is_ri_target <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 44 & panel$year %in% yrs
stopifnot(sum(is_ri_target) == 0)

new <- purrr::map_dfr(sprintf("elect_he_cty_ri_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>%
  transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0), nrow(new) == 5 * length(yrs))

add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample"))
stopifnot(nrow(add) == nrow(new))                                             # every row is genuinely new
message("new RI House rows to add: ", nrow(add), " (", length(yrs), " years x 5 counties)")

p2 <- bind_rows(panel, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 44 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
