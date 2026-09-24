## Massachusetts U.S. House, county level (the 14 counties, from town results), November general elections 1990-2014 (129 contests: 9-11 districts x 13 years), downloaded by
## 01ex_massachusetts_download.R from the Secretary of the Commonwealth's Elections Statistics site (electionstats.state.ma.us, PD43+ ElectionStats platform).
## Layout (precincts_include:0): header row 1 = City/Town, Ward, Pct, one column per candidate, "All Others", "Blanks", "Total Votes Cast"; row 2 = party under each candidate; one row per town
##   (a town split between districts appears in each district's table) plus a "TOTALS" row (statewide district total).
## Town -> county: R/data/raw_house_county_open_states/massachusetts/ma_town_county_crosswalk.csv (351 towns, each wholly inside one of the 14 counties); "E./N./S./W." prefixes expanded as in 01ag.
## Conventions (the earlier MA build 01ag / 02n): "Blanks" and "Total Votes Cast" are not candidates and are dropped; "All Others" (aggregated write-ins) IS kept as an OTHER candidate; party is the
##   literal label under each candidate (Democratic / Republican / anything else = OTHER). Massachusetts has no ballot-line fusion in this table (one party per candidate).
## Checks (stop on failure): every town row's candidate cells + All Others + Blanks add up to Total Votes Cast, and the town rows add up to the TOTALS row in every column -- except two documented
##   defects of the source tables, each repaired from the table's own totals and confirmed by the FEC's official results (R/data/fec_official/federalelections00.pdf and federalelections96.pdf):
##   (a) 2000 CD4: the candidate column of Martin D. Travis (Republican, FEC 56,553 votes) is missing from the table although Total Votes Cast includes it; in every town Travis = Total Votes Cast - the
##       listed cells (these residuals add up to exactly 56,553).
##   (b) 1996 CD10 Sandwich: Edward B. Teague III's cell is 180 short of the TOTALS row and the row total (4,573 printed; 4,753 needed; his FEC total 123,523 equals the TOTALS row).
## Counties with a town that has no rows in a year are dropped for that year (a partial county total would be wrong).
## Outputs: R/output/long/he_maelst_<year>.rds and R/output/elect_he_cty_maelst_<year>.rds for APPLY_YEARS (folded into the panel by 01ez_massachusetts_electionstats_apply.R); prints a comparison with the
##   panel rows that already exist.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "massachusetts_elections")
RAW <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "massachusetts")
idx <- read.csv(file.path(DIR, "elections_index.csv"), stringsAsFactors = FALSE) %>% filter(election_type == "General Election", office == "U.S. House") %>%
  mutate(district = sprintf("%02d", as.integer(sub("^([0-9]+).*$", "\\1", district))))
stopifnot(nrow(idx) == 129, !anyDuplicated(idx[, c("year", "district")]))
xw <- read_csv(file.path(RAW, "ma_town_county_crosswalk.csv"), show_col_types = FALSE) %>% mutate(town = toupper(trimws(town)), county = toupper(trimws(county))); stopifnot(nrow(xw) == 351)
expand_abbrev <- function(x) { x <- toupper(trimws(x)); x <- sub("^E\\.\\s+", "EAST ", x); x <- sub("^N\\.\\s+", "NORTH ", x); x <- sub("^S\\.\\s+", "SOUTH ", x); sub("^W\\.\\s+", "WEST ", x) }
ma_fips <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MASSACHUSETTS", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(county = toupper(trimws(county_name))) %>% filter(county %in% unique(xw$county)); stopifnot(nrow(ma_fips) == 14)
num <- function(m) suppressWarnings(as.numeric(gsub(",", "", m)))
res <- list(); log <- list()
for (i in seq_len(nrow(idx))) {
  d <- read.csv(file.path(DIR, sprintf("election_%s.csv", idx$election_id[i])), header = FALSE, colClasses = "character", check.names = FALSE, fill = TRUE, na.strings = character(0))
  nm <- trimws(unlist(d[1, -(1:3)])); pt <- trimws(unlist(d[2, -(1:3)])); pt <- c(pt, rep("", length(nm) - length(pt))); it <- which(nm == "Total Votes Cast"); stopifnot(length(it) == 1, it == length(nm), nm[it - 1] == "Blanks", nm[it - 2] == "All Others")
  body <- d[-(1:2), ]; body <- body[trimws(body$V1) != "", ]; V <- sapply(seq_along(nm), function(k) num(body[[k + 3]])); if (is.null(dim(V))) V <- matrix(V, nrow = nrow(body)); V[is.na(V)] <- 0
  is_tot <- trimws(body$V1) == "TOTALS"; stopifnot(sum(is_tot) == 1); tot <- V[is_tot, ]; tw <- V[!is_tot, , drop = FALSE]; tname <- expand_abbrev(body$V1[!is_tot])
  stopifnot(all(tname %in% xw$town), !anyDuplicated(tname))
  y <- idx$year[i]; dd <- idx$district[i]; note <- ""
  if (y == 2000 && dd == "04") {                                                            # (a) missing Republican column (Travis)
    resid <- tw[, it] - rowSums(tw[, -it, drop = FALSE]); stopifnot(sum(resid) == 56553, all(resid >= 0))
    nm <- c(nm[1:(it - 3)], "Martin D. Travis", nm[(it - 2):it]); pt <- c(pt[1:(it - 3)], "Republican", pt[(it - 2):it]); tw <- cbind(tw[, 1:(it - 3), drop = FALSE], resid, tw[, (it - 2):it, drop = FALSE]); tot <- c(tot[1:(it - 3)], 56553, tot[(it - 2):it]); it <- length(nm); note <- "Travis reconstructed" }
  if (y == 1996 && dd == "10") { s <- which(tname == "SANDWICH"); k <- which(nm == "Edward B. Teague, III"); stopifnot(length(s) == 1, length(k) == 1, tw[s, k] == 4573, sum(tw[, k]) + 180 == tot[k], sum(tw[s, -it]) + 180 == tw[s, it]); tw[s, k] <- 4753; note <- "Sandwich Teague +180" }
  stopifnot(all(rowSums(tw[, -it, drop = FALSE]) == tw[, it]), all(colSums(tw) == tot))
  log[[i]] <- data.frame(year = y, district = dd, n_towns = nrow(tw), repaired = note)
  cand <- which(!nm %in% c("Blanks", "Total Votes Cast"))
  for (k in cand) res[[length(res) + 1]] <- data.frame(year = y, town = tname, district = dd, candidate = nm[k], party = pt[k], votes = unname(tw[, k]), stringsAsFactors = FALSE)
}
log <- bind_rows(log); cat("contests:", nrow(log), "| every table ties to its TOTALS row (after the documented repairs):", nrow(log), "\n"); print(as.data.frame(log %>% filter(repaired != "")))
lines <- bind_rows(res) %>% left_join(xw %>% select(town, county), by = "town") %>% inner_join(ma_fips %>% select(county, county_fips), by = "county")
full_cty <- xw %>% crossing(year = sort(unique(lines$year))) %>% left_join(lines %>% distinct(year, town) %>% mutate(has = TRUE), by = c("year", "town")) %>% group_by(year, county) %>% summarise(complete = all(!is.na(has)), missing = paste(town[is.na(has)], collapse = ", "), .groups = "drop")
cat("counties dropped for a town without House rows:\n"); print(as.data.frame(full_cty %>% filter(!complete)))
raw <- lines %>% inner_join(full_cty %>% filter(complete) %>% select(year, county), by = c("year", "county")) %>%
  mutate(party_group = case_when(startsWith(toupper(party), "DEMOCRAT") ~ "DEM", startsWith(toupper(party), "REPUBLICAN") ~ "REP", TRUE ~ "OTHER")) %>%
  group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
saveRDS(raw, file.path(OUTPUT_DIR, "ma_electionstats_house_raw_candidates.rds"))
chk <- raw %>% distinct(year, district, candidate, party_group) %>% count(year, district, party_group) %>% filter(party_group != "OTHER", n > 1)
cat("district-years with more than one DEM or REP candidate:", nrow(chk), "\n"); print(as.data.frame(chk))

APPLY_YEARS <- seq(1990, 2014, 2)
for (y in APPLY_YEARS) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("maelst_", y))
  save_long(long, paste0("he_maelst_", y))
  shares <- derive_shares(long) %>% transmute(state = "MASSACHUSETTS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_maelst_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_maelst_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", cty_fips %/% 1000 == 25)
sh_all <- derive_shares(raw %>% transmute(year, county_fips, votes, party_group, office = "house")) %>% rename(b_dem = demovote, b_rep = repuvote, b_tot = totalvote)
cmp <- sh_all %>% left_join(panel %>% select(year, cty_fips, demovote, repuvote, totalvote), by = c("year", "cty_fips")) %>%
  group_by(year) %>% summarise(ma_counties = n(), in_panel = sum(!is.na(totalvote)), identical = sum(!is.na(totalvote) & abs(b_dem - demovote) < 1e-9 & abs(b_rep - repuvote) < 1e-9 & abs(b_tot - totalvote) < 0.5),
                                within_1pct_total = sum(!is.na(totalvote) & abs(b_tot / totalvote - 1) < 0.01), max_dem_diff = suppressWarnings(round(max(abs(b_dem - demovote), na.rm = TRUE), 4)), .groups = "drop")
cat("\nMA county-year rows vs existing panel rows:\n"); print(as.data.frame(cmp))
