## Remove non-vote pseudo-candidate rows (blank / void / over- / under-votes, "Blank, Void & Scattering", "Bvs Subtotal", "Times Blank Voted", rejected write-ins) from the
## long tables, the per-source shares files and the panel. 2026-09-21. Found by the FEC reconciliation: New York House totals were 9-19% too high (3.46M votes) while Democratic and
## Republican votes matched the FEC exactly. Every shares row that equals the value derived from the UNFILTERED long rows is replaced by the value derived from the filtered rows
## (shares are votes / county total, so removing the pseudo votes changes the shares too). Gate: nothing but those rows changes. Backup: backup_pseudo/.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
bk <- "/tmp/claude-1000/-home-paul-Stats-substack-projects-immigration/831fbe11-26b0-4ba2-9f03-261a9fbdf053/scratchpad/backup_pseudo"; dir.create(bk, showWarnings = FALSE)
lf <- list.files(LONG_DIR, "^(he|se)_.*\\.rds$", full.names = TRUE); lf <- lf[!grepl("_long_all|_county_summary|_long_raw", lf)]; lf <- union(lf, list.files(bk, "^(he|se)_.*\\.rds$", full.names = TRUE) %>% sub(bk, LONG_DIR, ., fixed = TRUE))
sf <- list.files(OUTPUT_DIR, "^elect_(he|se)_cty_.*\\.rds$", full.names = TRUE)
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); if (!file.exists(file.path(bk, "elect_cty_final_before_pseudo.rds"))) saveRDS(panel, file.path(bk, "elect_cty_final_before_pseudo.rds"))
same <- function(a, b) abs(a$demovote - b$demovote) < 1e-9 & abs(a$repuvote - b$repuvote) < 1e-9 & abs(a$totalvote - b$totalvote) < 0.5
upd <- function(tbl, old_d, new_d) {   # replace rows of tbl (year, cty_fips, sample, values) that equal old_d by new_d; returns list(table, n_replaced)
  j <- tbl %>% dplyr::mutate(.row = dplyr::row_number()) %>% dplyr::inner_join(old_d, by = c("year", "cty_fips", "sample"), suffix = c("", ".o")) %>% dplyr::filter(abs(demovote - demovote.o) < 1e-9, abs(repuvote - repuvote.o) < 1e-9, abs(totalvote - totalvote.o) < 0.5)
  if (!nrow(j)) return(list(tbl, 0L)); nw <- new_d %>% dplyr::inner_join(j %>% dplyr::select(year, cty_fips, sample, .row), by = c("year", "cty_fips", "sample"))
  tbl$demovote[nw$.row] <- nw$demovote; tbl$repuvote[nw$.row] <- nw$repuvote; tbl$totalvote[nw$.row] <- nw$totalvote; list(tbl, nrow(nw)) }
report <- list(); shares_cache <- list()
for (f in lf) {
  ## the unfiltered original is read from the backup when a previous (interrupted) run already filtered the live file, so the script can be re-run safely
  L <- readRDS(if (file.exists(file.path(bk, basename(f)))) file.path(bk, basename(f)) else f); ps <- grepl(PSEUDO_NAME_RE, trimws(L$candidate), ignore.case = TRUE); if (!any(ps)) next
  keys <- L[ps, ] %>% dplyr::distinct(year, county_fips); LK <- L %>% dplyr::inner_join(keys, by = c("year", "county_fips")); LKf <- LK[!grepl(PSEUDO_NAME_RE, trimws(LK$candidate), ignore.case = TRUE), ]
  off <- unique(L$office); stopifnot(length(off) == 1); old_d <- derive_shares(LK); new_d <- derive_shares(LKf)
  stopifnot(nrow(new_d) == nrow(old_d), all(new_d$totalvote <= old_d$totalvote[match(paste(new_d$year, new_d$cty_fips), paste(old_d$year, old_d$cty_fips))]))   # every affected county keeps some votes and loses none-or-some (a pseudo row can carry 0 votes)
  n_sh <- 0L
  for (s in sf) { smp <- unname(OFFICE_SAMPLE[off]); if (is.null(shares_cache[[s]])) shares_cache[[s]] <- readRDS(s)
    if (!any(shares_cache[[s]]$sample == smp)) next
    r <- upd(shares_cache[[s]], old_d, new_d); if (r[[2]] > 0) { if (!file.exists(file.path(bk, basename(s)))) saveRDS(shares_cache[[s]], file.path(bk, basename(s))); shares_cache[[s]] <- r[[1]]; saveRDS(r[[1]], s); n_sh <- n_sh + r[[2]] } }
  rp <- upd(panel, old_d, new_d); panel <- rp[[1]]
  if (!file.exists(file.path(bk, basename(f)))) saveRDS(L, file.path(bk, basename(f))); saveRDS(L[!ps, ], f)
  report[[basename(f)]] <- data.frame(long_file = basename(f), pseudo_rows = sum(ps), pseudo_votes = sum(L$votes[ps]), county_years = nrow(keys), shares_rows_updated = n_sh, panel_rows_updated = rp[[2]])
}
rep <- dplyr::bind_rows(report); print(as.data.frame(rep)); message("pseudo rows removed: ", sum(rep$pseudo_rows), " (", format(sum(rep$pseudo_votes), big.mark = ","), " votes); panel rows updated: ", sum(rep$panel_rows_updated))
saveRDS(panel, file.path(OUTPUT_DIR, "elect_cty_final.rds"))
