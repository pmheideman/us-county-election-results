## Fold the official Texas Secretary of State House county results (elect_he_cty_tx_sos_<year>.rds; see 02r_house_tx_sos.R) into the panel. 2026-09-20.
##   1992, 1994, 1996, 1998: rows ADDED (Texas had no House county data before 2000).
##   2006: the 5 districts redrawn after LULAC v. Perry (15, 21, 23, 25, 28) were on the November special-election ballot / December runoff, missing from the general-election pages: 49 counties added, split counties' totals change.
##   2014 (2 rows: El Paso 48141 / Ellis 48139 were swapped), 2016 (45 MEDSL rows), 2018 (43 MEDSL rows): rows REPLACED where the SOS value differs; identical rows are unchanged.
## Gate: the panel's Texas House rows must be the known state (2000-2018 as listed in the provenance: he_cty_tx and he_cty_medsl only) before anything is written;
## afterwards, everything outside Texas House and outside these 7 years is checked to be unchanged. Backup in scratchpad backup_tx/.
source(file.path("R", "00_setup.R"))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_tx"; dir.create(bk, showWarnings = FALSE)
if (!file.exists(file.path(bk, "elect_cty_final_before_tx.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_tx.rds"))
is_tx <- panel$sample == "HE" & panel$cty_fips %/% 1000 == 48
stopifnot(!any(panel$year[is_tx] %in% 1990:1998))
yrs <- c(1992, 1994, 1996, 1998, 2006, 2014, 2016, 2018)
new <- purrr::map_dfr(sprintf("elect_he_cty_tx_sos_%d.rds", yrs), function(f) readRDS(file.path(OUTPUT_DIR, f))) %>% transmute(year, cty_fips, sample, demovote, repuvote, totalvote, state = NA_character_)
stopifnot(!anyDuplicated(new[, c("year", "cty_fips", "sample")]), !anyNA(new[, c("demovote", "repuvote", "totalvote")]), all(new$totalvote > 0))
old <- panel %>% filter(is_tx, year %in% yrs) %>% inner_join(new, by = c("year", "cty_fips", "sample"), suffix = c("", ".new"))
chg <- old %>% filter(!(abs(demovote - demovote.new) < 1e-9 & abs(repuvote - repuvote.new) < 1e-9 & abs(totalvote - totalvote.new) < 0.5))
message("existing rows in these years: ", nrow(old), "; of which differ from SOS: ", nrow(chg), " (", paste(names(table(chg$year)), table(chg$year), sep = ": ", collapse = ", "), ")")
add <- new %>% anti_join(panel %>% select(year, cty_fips, sample), by = c("year", "cty_fips", "sample")); message("new rows added: ", nrow(add), " (", paste(names(table(add$year)), table(add$year), sep = ": ", collapse = ", "), ")")
p2 <- panel %>% filter(!(is_tx & year %in% yrs & paste(year, cty_fips) %in% paste(new$year, new$cty_fips)))
p2 <- bind_rows(p2, new[, names(panel)])
stopifnot(anyDuplicated(p2[, c("year", "cty_fips", "sample")]) == 0)
other0 <- panel %>% filter(!(is_tx & year %in% yrs)) %>% arrange(year, cty_fips, sample); other1 <- p2 %>% filter(!(sample == "HE" & cty_fips %/% 1000 == 48 & year %in% yrs)) %>% arrange(year, cty_fips, sample)
stopifnot(isTRUE(all.equal(other0, other1, check.attributes = FALSE)))
saveRDS(p2, file.path(OUTPUT_DIR, "elect_cty_final.rds")); message("panel rows ", nrow(panel), " -> ", nrow(p2))
