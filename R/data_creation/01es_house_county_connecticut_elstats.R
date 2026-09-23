## Connecticut U.S. House, county level (the 8 counties, from town results), November general elections 1990-2018 (80 contests, downloaded by 01er_connecticut_download.R from the Secretary of the
## State's Election History database, electionhistory.ct.gov, Elstats platform tenant "ct").
## Layout: header row 1 = candidate names, row 2 = ballot line, a "Representative in Congress District" total row, one "City/Town" row per town slice of the district (a town split between two
## districts appears in each), optional "Polling Place" rows (ignored: they are a finer breakdown of the town rows, and most years have none), last columns "Total Votes Cast" and "Total Ballots Cast".
## Town -> county: R/data/raw_house_county_open_states/connecticut/ct_town_county_crosswalk.csv (169 towns, each wholly inside one of the 8 counties). Every town name in the database matches it.
## FUSION: literal ballot-line classification, NO cross-line consolidation (the convention of the earlier Connecticut build 01x / 02n, which reproduces MEDSL's CT county numbers exactly): a line is DEM
## if its label starts with "Democratic", REP if it starts with "Republican", otherwise OTHER (Working Families, A Connecticut, Independent Party, Green, Write In, ...). One long row per
## (county, district, candidate, ballot line).
## Checks (stop on failure): every town row's candidate cells add up to Total Votes Cast, except three town rows where the database's own total disagrees with its cells (1994 CD5 Woodbridge: cells
##   exceed the total by 200, and the district row's cells exceed its total by the same 200; 2008 CD2 Groton and Sprague: total exceeds the cells by 6 and 1, i.e. votes not itemised). The candidate
##   cells are used everywhere (they tie to the district row's cells), so the long-table total there is the sum of the cells. For every contest the town rows add up to the printed district row in every column, EXCEPT
##   (a) write-in columns ("Write In" lines) that the database prints only on the district row (2004 CD2 Lyon 130, CD4 Vassar 4; 2008 CD2 Vachon 11 vs 12 in one town): the town rows are used, and
##   (b) 2008 CD3 Republican Bo Itshaky, whose printed district-row cell (26,432) contradicts his town rows (58,583) while the printed district TOTAL (297,368) equals the town rows: the town rows are used.
## 1998: the CD1 contest does not exist in the database (uncontested seats are not counted in Connecticut), so 19 towns have no House rows; only counties with EVERY town present are kept (a partial
##   county total would be wrong).
## Party-label overrides: see PARTY_OVERRIDE below (checked against FEC official results).
## Outputs: R/output/long/he_ctelh_<year>.rds and R/output/elect_he_cty_ctelh_<year>.rds for APPLY_YEARS (folded into the panel by 01et_connecticut_elstats_apply.R); prints a comparison with the
##   panel rows that already exist for every year.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "connecticut_elections")
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "connecticut")
idx <- read.csv(file.path(DIR, "contests_index.csv"), stringsAsFactors = FALSE) %>% filter(keep) %>%
  mutate(district = sprintf("%02d", as.integer(sub("Representative in Congress District ", "", division))))
stopifnot(nrow(idx) == 80, !anyDuplicated(idx[, c("year", "district")]))
xw <- read_csv(file.path(RAW, "ct_town_county_crosswalk.csv"), show_col_types = FALSE) %>% mutate(town = toupper(trimws(town)), county = toupper(trimws(sub(" County$", "", county))))
OBSOLETE <- c("ROCKVILLE", "SOUTH NORWALK", "WILLIMANTIC", "WINSTED")             # old boroughs/cities listed in the crosswalk that are not towns (no election rows)
towns_all <- setdiff(xw$town, OBSOLETE); stopifnot(length(towns_all) == 169)
ct_fips <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "CONNECTICUT", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(county = toupper(trimws(county_name))) %>% filter(county %in% unique(xw$county))
stopifnot(nrow(ct_fips) == 8)
num <- function(m) suppressWarnings(as.numeric(gsub(",", "", m)))
PSEUDO <- c("Total Votes Cast", "Total Ballots Cast")
res <- list(); log <- list()
for (i in seq_len(nrow(idx))) {
  d <- read.csv(file.path(DIR, sprintf("contest_%d.csv", idx$contest_id[i])), header = FALSE, colClasses = "character", check.names = FALSE, fill = TRUE)
  nm <- unlist(d[1, -(1:2)]); pt <- unlist(d[2, -(1:2)]); it <- which(nm == "Total Votes Cast"); stopifnot(length(it) == 1, all(nm[(it):length(nm)] %in% PSEUDO))
  cc <- which(!nm %in% PSEUDO)
  body <- d[-(1:2), ]; V <- sapply(seq_along(nm), function(k) num(body[[k + 2]])); if (is.null(dim(V))) V <- matrix(V, nrow = nrow(body)); V[is.na(V)] <- 0
  is_town <- body$V1 == "City/Town"; is_dist <- body$V1 == "Representative in Congress District"; stopifnot(sum(is_dist) == 1, sum(is_town) >= 1)
  tw <- V[is_town, , drop = FALSE]; tname <- toupper(trimws(body$V2[is_town])); dist <- V[is_dist, ]
  stopifnot(all(tname %in% xw$town), !anyDuplicated(tname))
  rs <- rowSums(tw[, cc, drop = FALSE]) - tw[, it]                                              # candidate cells minus the printed town total: 0 except 3 documented town rows
  KNOWN_ROW_DIFF <- c("1994|05|WOODBRIDGE" = 200, "2008|02|GROTON" = -6, "2008|02|SPRAGUE" = -1)
  stopifnot(all(rs[rs != 0] == KNOWN_ROW_DIFF[paste(idx$year[i], idx$district[i], tname[rs != 0], sep = "|")]))
  diff <- colSums(tw) - dist
  ok_wi <- pt == "Write In" & colSums(tw) <= dist                                              # (a) write-in columns printed only (or more fully) on the district row
  ok_it <- nm == "Bo Itshaky" & idx$year[i] == 2008 & idx$district[i] == "03"                  # (b) documented district-row typo
  bad <- which(diff != 0 & !ok_wi & !ok_it); if (idx$year[i] == 2008 && idx$district[i] == "02") bad <- setdiff(bad, which(nm == "Todd Vachon" | nm %in% PSEUDO))
  if (idx$year[i] == 2004 || (idx$year[i] == 2008 && idx$district[i] == "02")) bad <- setdiff(bad, which(nm %in% PSEUDO))                    # totals differ by the write-in amounts only
  if (idx$year[i] == 2008 && idx$district[i] == "03") stopifnot(diff[it] == 0)
  stopifnot(length(bad) == 0)
  log[[i]] <- data.frame(year = idx$year[i], district = idx$district[i], n_towns = nrow(tw), exceptions = sum(diff != 0))
  cand <- tibble(col = cc, candidate = trimws(nm[cc]), line = pt[cc])
  for (k in seq_len(nrow(cand))) res[[length(res) + 1]] <- data.frame(year = idx$year[i], town = tname, district = idx$district[i], candidate = cand$candidate[k], line = cand$line[k], votes = unname(tw[, cand$col[k]]), stringsAsFactors = FALSE)
}
log <- bind_rows(log); cat("contests:", nrow(log), "| contests with a documented tie exception:", sum(log$exceptions > 0), "\n"); print(as.data.frame(log %>% filter(exceptions > 0)))
lines <- bind_rows(res) %>% mutate(candidate = gsub("\\s+", " ", candidate))
## the database sometimes spells one candidate two ways within a district-year: merge on first initial + last name into the spelling with the most votes
nm_key <- function(x) { w <- strsplit(gsub("[.,]", "", toupper(iconv(x, to = "ASCII//TRANSLIT"))), " ")[[1]]; w <- w[!w %in% c("JR", "SR", "II", "III", "IV")]; paste0(substr(w[1], 1, 1), "|", w[length(w)]) }
lines$nkey <- vapply(lines$candidate, nm_key, "")
canon <- lines %>% group_by(year, district, nkey, candidate) %>% summarise(v = sum(votes), .groups = "drop") %>% group_by(year, district, nkey) %>% arrange(desc(v), .by_group = TRUE) %>% mutate(canon = first(candidate), n_var = n()) %>% ungroup()
cat("candidate spellings merged within a district-year:\n"); print(as.data.frame(canon %>% filter(n_var > 1) %>% select(year, district, candidate, canon, v)))
## Two party labels in the database are wrong / missing and are corrected against the FEC's official results (R/data/fec_official/federalelections98.pdf; fec2002_house.csv):
##   1998 CD3 David P. Cole is labelled "Republican" (the Republican was Martin T. Reust): FEC party REF (Reform), 676 votes;  2002 CD2 Joe Courtney (the Democratic nominee, FEC party D) has a BLANK label.
PARTY_OVERRIDE <- tribble(~year, ~district, ~candidate, ~party,
                          1998, "03", "David P. Cole", "Reform",
                          2002, "02", "Joe Courtney", "Democratic")
for (k in seq_len(nrow(PARTY_OVERRIDE))) { hit <- lines$year == PARTY_OVERRIDE$year[k] & lines$district == PARTY_OVERRIDE$district[k] & lines$candidate == PARTY_OVERRIDE$candidate[k]; stopifnot(any(hit)); lines$line[hit] <- PARTY_OVERRIDE$party[k] }
lines <- lines %>% left_join(canon %>% select(year, district, nkey, candidate, canon), by = c("year", "district", "nkey", "candidate")) %>% mutate(candidate = canon) %>% select(-nkey, -canon)
lines <- lines %>% left_join(xw %>% select(town, county), by = "town") %>% inner_join(ct_fips %>% select(county, county_fips), by = "county")
## keep only counties whose every town has House rows in that year (1998: CD1 does not exist)
present <- lines %>% distinct(year, town) %>% count(year, name = "n_towns")
full_cty <- xw %>% filter(!town %in% OBSOLETE) %>% crossing(year = sort(unique(lines$year))) %>% left_join(lines %>% distinct(year, town) %>% mutate(has = TRUE), by = c("year", "town")) %>%
  group_by(year, county) %>% summarise(complete = all(!is.na(has)), .groups = "drop")
cat("counties dropped for missing towns:\n"); print(as.data.frame(full_cty %>% filter(!complete)))
raw <- lines %>% inner_join(full_cty %>% filter(complete) %>% select(year, county), by = c("year", "county")) %>%
  mutate(party_group = case_when(startsWith(toupper(line), "DEMOCRAT") ~ "DEM", startsWith(toupper(line), "REPUBLICAN") ~ "REP", TRUE ~ "OTHER")) %>%
  group_by(year, county_fips, district, candidate, party = line, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
stopifnot(!anyNA(raw$county_fips))
saveRDS(raw, file.path(OUTPUT_DIR, "ct_elstats_house_raw_candidates.rds"))
chk <- raw %>% distinct(year, district, candidate, party_group) %>% count(year, district, party_group) %>% filter(party_group != "OTHER", n > 1)
cat("district-years with more than one DEM or REP candidate:", nrow(chk), "\n"); print(as.data.frame(chk))

APPLY_YEARS <- seq(1990, 2014, 2)   # fills 1990-1998, 2004, 2012 (OpenElections had 3 of 8 counties), 2014 and replaces the OpenElections rows of 2000-2010; 2016-2018 stay MEDSL (identical to the database to 4 decimals, only compared below)
for (y in APPLY_YEARS) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ctelh_", y))
  save_long(long, paste0("he_ctelh_", y))
  shares <- derive_shares(long) %>% transmute(state = "CONNECTICUT", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ctelh_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ctelh_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}

## ---- comparison with the panel rows that already exist ----
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 9)
sh_all <- derive_shares(raw %>% transmute(year, county_fips, votes, party_group, office = "house")) %>% rename(b_dem = demovote, b_rep = repuvote, b_tot = totalvote)
cmp <- sh_all %>% left_join(panel %>% select(year, cty_fips, demovote, repuvote, totalvote), by = c("year", "cty_fips")) %>%
  group_by(year) %>% summarise(ct_counties = n(), in_panel = sum(!is.na(totalvote)), identical = sum(!is.na(totalvote) & abs(b_dem - demovote) < 1e-9 & abs(b_rep - repuvote) < 1e-9 & abs(b_tot - totalvote) < 0.5),
                                within_1pct_total = sum(!is.na(totalvote) & abs(b_tot / totalvote - 1) < 0.01), max_dem_diff = suppressWarnings(round(max(abs(b_dem - demovote), na.rm = TRUE), 4)), .groups = "drop")
cat("\nCT county-year rows vs existing panel rows:\n"); print(as.data.frame(cmp))
