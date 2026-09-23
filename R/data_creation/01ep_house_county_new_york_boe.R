## New York U.S. House, county level, November general elections 1996-2018 (346 distinct contests: one per congressional district and year; 347 listed), from the New York State Board of Elections'
## Elections Database (https://results.elections.ny.gov/, the same Elstats platform as Virginia/Vermont/Colorado, tenant "ny"). Database history starts in 1996 (nothing earlier).
## HOW OBTAINED: the site returns HTTP 403 to curl/scripted clients, so both calls were made from a browser session on the site itself (javascript in the page):
##   1. POST /api/graphql_pr (header X-Elstats-Tenant: ny), query SearchContests with offices=[{id:13}] ("Representative in Congress"), one call per year 1996..2018 -> contest ids
##      (keep: eventTypeDisplayName == "General", not special, not runoff, event date in November).
##   2. GET /api/download_contest/<contest id>_table.csv?split_party=true for each contest (one column per candidate x party line, so fusion candidates keep every line).
##   The 347 listed tables were saved as R/data/raw_house_county_open_states/ny_elections/house_by_party/contest_<id>.csv (+ contests_index.csv). A "merged" variant (split_party=false)
##   shows only one party label per candidate (e.g. Paxon 1996 as "Conservative") and was NOT used.
## Layout: header row 1 = candidate names, row 2 = party line, then one "Congressional District" total row and one "County" row per county slice of that district; last column
##   "Total Votes". Non-candidate columns "Blank", "Void", "Scattering" and "Blank/Void/Scattering" are not votes for anybody and are left out (the FEC district totals exclude them;
##   this matches the earlier New York builds and MEDSL clean-up). Write-in ("Scattering") votes therefore are not in the county totals.
## Fusion voting: a candidate's columns (Democratic, Working Families, Independence, ...) are summed per (county, district, candidate) FIRST; the candidate is DEM if any of their
##   party lines starts with "Democratic", else REP if any starts with "Republican" (incl. "Republican + TRP"), else OTHER (same rule as 02n_house_long_new_york.R). Display party =
##   "Democratic"/"Republican" for those, otherwise the candidate's first line label.
## Source defect handled: in 2010 CD24 and 2012-2018 CD22 and CD24 the database's county table contains an extra "Otsego" row that is a cell-for-cell copy of that district's "Oswego"
##   row (Otsego is entirely in CD19 in 2018, and the 3 Otsego rows add up to 2.8x the county's Senate vote); the district row and the FEC total equal the county rows WITHOUT it.
##   The Otsego row is dropped in exactly those contests (asserted: identical to Oswego and the remaining rows tie to the district row in every column).
## Checks (stop on failure): every county row's candidate cells add up to its Total Votes; for every contest the county rows add up to the printed district row in every column.
## Outputs: R/output/long/he_nyboe_<year>.rds and R/output/elect_he_cty_nyboe_<year>.rds for the APPLY_YEARS below (folded into the panel by 01eq_new_york_boe_apply.R);
##   R/output/ny_boe_house_raw_candidates.rds (all years, for comparison). Also prints a comparison with the existing panel rows for every other year.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "ny_elections", "house_by_party")
APPLY_YEARS <- seq(1996, 2018, 2)   # every year in the database: fills the gaps (1996, 1998, 2002, 2008, 2010) and replaces the OpenElections / MEDSL rows of 2000-2006 and 2012-2018 (see the comparison printed at the end and 01eq_new_york_boe_apply.R)
idx <- read.csv(file.path(DIR, "contests_index.csv"), stringsAsFactors = FALSE, colClasses = c(contest_id = "character")) %>%
  mutate(district = sprintf("%02d", as.integer(sub("Congressional District ", "", division))))
## the database lists 2000 CD2 twice (contest ids 4359 and 5531, identical except one party label, "GRN" vs "Green"): keep the first
dup <- idx[duplicated(idx[, c("year", "district")]) | duplicated(idx[, c("year", "district")], fromLast = TRUE), ]
for (k in unique(paste(dup$year, dup$district))) { ids <- dup$contest_id[paste(dup$year, dup$district) == k]; stopifnot(length(ids) == 2, identical(readLines(file.path(DIR, sprintf("contest_%s.csv", ids[1])))[-2], readLines(file.path(DIR, sprintf("contest_%s.csv", ids[2])))[-2])); idx <- idx[idx$contest_id != ids[2], ] }
stopifnot(nrow(idx) == 346, !anyDuplicated(idx[, c("year", "district")]))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "NEW YORK", !is.na(county_fips)) %>% distinct(county_fips, county_name) %>% mutate(nm = toupper(trimws(county_name)))
fips_of <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$nm)]
NONCAND <- c("Blank", "Void", "Scattering", "Blank/Void/Scattering")
num <- function(m) suppressWarnings(as.numeric(gsub(",", "", m)))
res <- list(); log <- list()
for (i in seq_len(nrow(idx))) {
  d <- read.csv(file.path(DIR, sprintf("contest_%s.csv", idx$contest_id[i])), header = FALSE, colClasses = "character", check.names = FALSE, fill = TRUE)
  nm <- unlist(d[1, -(1:2)]); pt <- unlist(d[2, -(1:2)]); it <- which(nm == "Total Votes"); stopifnot(length(it) == 1, it == length(nm))
  cc <- which(!nm %in% c(NONCAND, "Total Votes"))
  body <- d[-(1:2), ]; V <- sapply(seq_along(nm), function(k) num(body[[k + 2]])); if (is.null(dim(V))) V <- matrix(V, nrow = nrow(body))
  is_cty <- body$V1 == "County"; is_dist <- body$V1 == "Congressional District"; stopifnot(sum(is_dist) == 1, sum(is_cty) >= 1)
  cty <- V[is_cty, , drop = FALSE]; cname <- trimws(body$V2[is_cty]); dist <- V[is_dist, ]
  stopifnot(all(rowSums(cty[, -it, drop = FALSE]) == cty[, it]))                 # candidate + blank/void/scattering cells add up to Total Votes in every county row
  ties <- function(m) all(colSums(m) == dist)
  dropped <- NA_character_
  if (!ties(cty)) {
    oi <- which(cname == "Otsego"); wi <- which(cname == "Oswego")
    stopifnot(length(oi) == 1, length(wi) == 1, all(cty[oi, ] == cty[wi, ]), ties(cty[-oi, , drop = FALSE]))   # the documented duplicate-row defect, nothing else
    cty <- cty[-oi, , drop = FALSE]; cname <- cname[-oi]; dropped <- "Otsego (copy of Oswego)"
  }
  log[[i]] <- data.frame(year = idx$year[i], district = idx$district[i], n_cty = nrow(cty), dropped = dropped, tie_ok = ties(cty))
  fp <- fips_of(cname); stopifnot(!anyNA(fp))
  keep <- cty[, it] > 0; cty <- cty[keep, , drop = FALSE]; fp <- fp[keep]
  cand <- tibble(col = cc, candidate = nm[cc], line = pt[cc])
  for (k in seq_len(nrow(cand))) res[[length(res) + 1]] <- data.frame(year = idx$year[i], county_fips = fp, district = idx$district[i], candidate = cand$candidate[k], line = cand$line[k], votes = unname(cty[, cand$col[k]]), stringsAsFactors = FALSE)
}
log <- bind_rows(log); cat("contests:", nrow(log), "| county rows tie to the district row in all columns:", sum(log$tie_ok), "| Otsego duplicate dropped in", sum(!is.na(log$dropped)), "\n"); stopifnot(all(log$tie_ok))
lines <- bind_rows(res) %>% mutate(candidate = gsub("\\s+", " ", trimws(candidate)))
## the database sometimes spells the same candidate differently on different party lines (2000 CD13: "Vito  Fossella" on the Republican line, "Vito J. Fossella" on Conservative / Right to Life);
## merge names within a (year, district) that share first initial + last name (suffix Jr/Sr/II/III ignored) into the spelling with the most votes, BEFORE lines are summed and classified
nm_key <- function(x) { w <- strsplit(gsub("[.,]", "", toupper(iconv(x, to = "ASCII//TRANSLIT"))), " ")[[1]]; w <- w[!w %in% c("JR", "SR", "II", "III", "IV")]; paste0(substr(w[1], 1, 1), "|", w[length(w)]) }
lines$nkey <- vapply(lines$candidate, nm_key, "")
canon <- lines %>% group_by(year, district, nkey, candidate) %>% summarise(v = sum(votes), .groups = "drop") %>% group_by(year, district, nkey) %>% arrange(desc(v), .by_group = TRUE) %>%
  mutate(canon = first(candidate), n_var = n()) %>% ungroup()
cat("candidate spellings merged within a district-year:\n"); print(as.data.frame(canon %>% filter(n_var > 1) %>% select(year, district, candidate, canon, v)))
lines <- lines %>% left_join(canon %>% select(year, district, nkey, candidate, canon), by = c("year", "district", "nkey", "candidate")) %>% mutate(candidate = canon) %>% select(-nkey, -canon)
## fusion: sum a candidate's lines per (county, district, candidate); classify from the set of lines
cand_tab <- lines %>% group_by(year, district, candidate) %>% summarise(labels = paste(unique(line), collapse = " | "), first_label = first(line), .groups = "drop") %>%
  mutate(is_dem = grepl("^Democratic|\\| Democratic", labels), is_rep = grepl("^Republican|\\| Republican", labels),
         party_group = ifelse(is_dem, "DEM", ifelse(is_rep, "REP", "OTHER")), party = ifelse(is_dem, "Democratic", ifelse(is_rep, "Republican", first_label)))
cat("candidates whose lines include BOTH Democratic and Republican (counted DEM):\n"); print(as.data.frame(cand_tab %>% filter(is_dem & is_rep) %>% select(year, district, candidate, labels)))
raw <- lines %>% group_by(year, county_fips, district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(cand_tab %>% select(year, district, candidate, party, party_group), by = c("year", "district", "candidate")) %>% filter(votes > 0)
stopifnot(!anyNA(raw$party_group), !anyDuplicated(raw[, c("year", "county_fips", "district", "candidate")]))
saveRDS(raw, file.path(OUTPUT_DIR, "ny_boe_house_raw_candidates.rds"))

## every district has at most one DEM and one REP candidate (a second D or R candidate would be a mis-labelled line)
chk <- raw %>% distinct(year, district, candidate, party_group) %>% count(year, district, party_group) %>% filter(party_group != "OTHER", n > 1)
cat("district-years with more than one DEM or REP candidate:", nrow(chk), "\n"); print(as.data.frame(chk)); stopifnot(nrow(chk) == 0)

for (y in APPLY_YEARS) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("nyboe_", y))
  save_long(long, paste0("he_nyboe_", y))
  shares <- derive_shares(long) %>% transmute(state = "NEW YORK", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nyboe_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nyboe_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}

## ---- comparison with the panel rows that already exist (all years 1996-2018) ----
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 36)
sh_all <- derive_shares(raw %>% transmute(year, county_fips, votes, party_group, office = "house")) %>% rename(b_dem = demovote, b_rep = repuvote, b_tot = totalvote)
cmp <- sh_all %>% left_join(panel %>% select(year, cty_fips, demovote, repuvote, totalvote), by = c("year", "cty_fips")) %>%
  group_by(year) %>% summarise(boe_counties = n(), in_panel = sum(!is.na(totalvote)), identical = sum(!is.na(totalvote) & abs(b_dem - demovote) < 1e-9 & abs(b_rep - repuvote) < 1e-9 & abs(b_tot - totalvote) < 0.5),
                                within_1pct_total = sum(!is.na(totalvote) & abs(b_tot / totalvote - 1) < 0.01), max_dem_diff = round(max(abs(b_dem - demovote), na.rm = TRUE), 4), .groups = "drop")
cat("\nBOE county-year rows vs existing NY panel rows:\n"); print(as.data.frame(cmp))
