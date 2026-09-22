## Virginia U.S. House, county/city (locality) level, November general elections 1990-2024, from the Department of Elections' Historical Elections Database
## (contest CSVs cached by 01cl_virginia_download.R). One CSV per congressional district. Layout: header row 1 = candidate names, row 2 = parties, then a
## "Congressional District" total row, one "Locality" row per county / independent city (the part of a split locality that lies in this district), and "Precinct" rows (unused).
## Candidate columns run up to "Total Votes Cast"; "Undervotes" (through 2022) is a pseudo column and is dropped. "Write-Ins" is one aggregated candidate (kept, OTHER).
## Checks: per contest, locality sums == the printed district row for every candidate, and each row's candidates add up to "Total Votes Cast".
## Outputs: R/output/long/he_va_<year>.rds (all years), R/output/va_sos_comparison.csv (vs the panel's current Virginia rows) and, for the years folded into the panel,
## R/output/elect_he_cty_va_<year>.rds.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "virginia_elections")
idx <- read.csv(file.path(DIR, "contests_index.csv"), stringsAsFactors = FALSE) %>% filter(keep) %>% mutate(district = sprintf("%02d", as.integer(sub("Congressional District ", "", division))))
stopifnot(!anyDuplicated(idx[, c("year", "district")]))

## ---- locality name -> FIPS: counties (fips < 510) by name, independent cities (>= 510) by name without "City"; names come from the MEDSL table, plus the 2 cities that no longer exist
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state_po == "VA", !is.na(county_fips)) %>% distinct(county_fips, county_name) %>%
  mutate(kind = ifelse(county_fips < 51510, "County", "City"), nm = toupper(county_name),
         base = gsub("[^A-Z]", "", ifelse(kind == "County", sub(" COUNTY$", "", nm), sub(" CITY$", "", nm)))) %>% distinct(county_fips, base, kind)   # NB "Charles City" is a county
xw <- bind_rows(xw, tibble(county_fips = c(51560L, 51780L), base = c("CLIFTONFORGE", "SOUTHBOSTON"), kind = "City"))     # Clifton Forge (merged into Alleghany 2001), South Boston (into Halifax 1995)
stopifnot(!anyDuplicated(xw[, c("base", "kind")]))
fips_of <- function(z) { kind <- ifelse(grepl(" City$", z), "City", "County"); base <- gsub("[^A-Z]", "", toupper(sub(" (City|County)$", "", z))); xw$county_fips[match(paste(base, kind), paste(xw$base, xw$kind))] }

pseudo_cols <- c("Total Votes Cast", "Undervotes")
res <- list(); tie <- list()
for (i in seq_len(nrow(idx))) {
  d <- read.csv(file.path(DIR, sprintf("contest_%d.csv", idx$contest_id[i])), header = FALSE, colClasses = "character", check.names = FALSE, fill = TRUE)
  nm <- unlist(d[1, -(1:2)]); pt <- unlist(d[2, -(1:2)]); it <- which(nm == "Total Votes Cast"); stopifnot(length(it) == 1)
  cc <- seq_len(it - 1)                                                       # candidate columns (relative to the -(1:2) offset)
  body <- d[-(1:2), ]; num <- function(m) suppressWarnings(as.numeric(gsub(",", "", m)))
  V <- sapply(seq_len(ncol(d) - 2), function(k) num(body[[k + 2]]))
  if (is.null(dim(V))) V <- matrix(V, nrow = nrow(body))
  is_loc <- body$V1 == "Locality"; is_dist <- body$V1 == "Congressional District"
  loc <- V[is_loc, , drop = FALSE]; dist <- V[is_dist, ]
  ok_rows <- all(rowSums(loc[, cc, drop = FALSE]) == loc[, it]); ok_tie <- all(colSums(loc[, c(cc, it)]) == dist[c(cc, it)])
  ## A locality row whose candidate cells do not add up to its own "Total Votes Cast" (2022 district 8, Alexandria: cells look like an earlier count): use the precinct sums of that
  ## locality if THEY add up to the row's total exactly; the contest is still listed as a tie failure below (the district row is built from the stale cells).
  bad <- which(rowSums(loc[, cc, drop = FALSE]) != loc[, it]); loc_id <- cumsum(is_loc)
  for (b in bad) { ps <- colSums(V[body$V1 == "Precinct" & loc_id == b, , drop = FALSE], na.rm = TRUE)
    if (sum(ps[cc]) == loc[b, it]) { message("  ", idx$year[i], " D", idx$district[i], " ", body$V2[is_loc][b], ": candidate cells replaced by precinct sums (", sum(loc[b, cc]), " -> ", sum(ps[cc]), ")"); loc[b, cc] <- ps[cc] } else message("  ", idx$year[i], " D", idx$district[i], " ", body$V2[is_loc][b], ": cells do not add to the total and precincts do not fix it (kept cells)") }
  tie[[i]] <- data.frame(year = idx$year[i], district = idx$district[i], n_loc = nrow(loc), rows_ok = ok_rows, tie_ok = ok_tie)
  fp <- fips_of(body$V2[is_loc]); stopifnot(!anyNA(fp))
  ## The database's own locality labels are unreliable for two pairs: it calls the independent cities of Richmond and Franklin "Richmond County" / "Franklin County" in most years.
  ## Geography decides (Richmond County lies only in district 1; Franklin County only in district 5, from 2022 district 9; the cities are in other districts):
  lab <- sub(" (City|County)$", "", body$V2[is_loc]); dn <- as.integer(idx$district[i])
  for (b in c("Richmond", "Franklin")) { w <- which(lab == b); if (!length(w)) next
    if (length(w) == 2) next                                   # both the city and the county are listed in this contest: their labels are taken as printed
    fp[w] <- if (b == "Richmond") ifelse(dn == 1, 51159L, 51760L) else ifelse(dn %in% c(5, 9), 51067L, 51620L) }
  keep_loc <- loc[, it] > 0; loc <- loc[keep_loc, , drop = FALSE]; fp <- fp[keep_loc]                 # a few rows are all zero (Bedford City in district 6, 1996/98): dropped
  res[[i]] <- do.call(rbind, lapply(seq_along(cc), function(k) data.frame(year = idx$year[i], county_fips = fp, district = idx$district[i], candidate = nm[cc[k]], party = pt[cc[k]], votes = loc[, cc[k]], stringsAsFactors = FALSE)))
}
tie <- bind_rows(tie); cat("contests:", nrow(tie), "| every locality row adds up to Total Votes Cast:", sum(tie$rows_ok), "| locality sums equal the printed district row:", sum(tie$tie_ok), "\n"); print(as.data.frame(tie %>% filter(!rows_ok | !tie_ok)))
raw <- bind_rows(res) %>% mutate(party = ifelse(party == "", "Write-In", party), party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"))
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")
for (y in sort(unique(raw$year))) save_long(finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("va_", y)), paste0("he_va_", y))
saveRDS(raw, file.path(OUTPUT_DIR, "va_raw_candidates.rds"))

## ---- compare with the panel ------------------------------------------------------------------------------------------------------------------------
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 51)
L <- purrr::map_dfr(list.files(LONG_DIR, "^he_va_[0-9]{4}\\.rds$", full.names = TRUE), readRDS)
j <- derive_shares(L) %>% full_join(panel, by = c("year", "cty_fips", "sample"), suffix = c(".va", ".panel")) %>%
  mutate(status = case_when(is.na(totalvote.va) ~ "panel only", is.na(totalvote.panel) ~ "VA only",
    abs(demovote.va - demovote.panel) < 1e-9 & abs(repuvote.va - repuvote.panel) < 1e-9 & abs(totalvote.va - totalvote.panel) < 0.5 ~ "identical", TRUE ~ "differs"))
print(as.data.frame(j %>% count(year, status) %>% tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0) %>% arrange(year)))
write.csv(j %>% filter(status %in% c("differs", "panel only")) %>% arrange(year, cty_fips), file.path(OUTPUT_DIR, "va_sos_comparison.csv"), row.names = FALSE)

## ---- shares files for every year (all are folded into the panel by 01cm_virginia_apply.R) + acceptance test ----------------------------------------
for (y in seq(1990, 2024, 2)) {
  long <- readRDS(file.path(LONG_DIR, sprintf("he_va_%d.rds", y)))
  shares <- derive_shares(long) %>% transmute(state = "VIRGINIA", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_va_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_va_%d.rds", y))); stopifnot(all(r$pass))
}
