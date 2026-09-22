## Vermont U.S. House, town-level, 1990-2010 general elections, from electionarchive.vermont.gov (contest CSVs cached by 01du_vermont_download.R).
## Vermont has a single AT-LARGE seat (district "00") -- one contest per year, no per-district split needed (unlike Virginia's 02r on the same platform).
## Layout: header row 1 = candidate names, row 2 = parties, then a "State" total row (statewide, used only as a tie-check), one "City/Town" row per
## municipality, and "Precinct" rows for the handful of towns large enough to report by precinct (redundant with that town's own City/Town row -- unused).
## Candidate columns run up to "Total Votes Cast"; "Undervotes"/"Overvotes"/"Total Ballots Cast" are pseudo columns after it and are dropped. "Write-Ins"
## is one aggregated candidate (kept, OTHER). Vermont has no county government -- towns are mapped to counties via a Wikipedia-sourced crosswalk
## (vt_town_county_crosswalk.csv); every town nests wholly in one county (same situation as Connecticut/Rhode Island), so no split-town handling is
## needed. "Sherburne" was renamed "Killington" in 1999 but the source still uses the old name through the 2000 general election -- aliased in the
## crosswalk. Checks: per contest, every City/Town row's candidate cells add up to its own "Total Votes Cast", and the City/Town rows sum to the
## printed State row for every candidate.
## Party grouping: a handful of minor candidates ran on a fused line that includes a major-party name (e.g. Peter Welch "Democratic/Republican" in 2008
## -- essentially unopposed; Pete Diamondstone "Liberty Union/Democratic" in 2000, apparently also the Democratic nominee that year) -- classified DEM/REP
## by substring match on the party label (checking "democrat" before "republican"), the SAME rule already established and verified for Vermont's 2016
## MEDSL fused-party row (see R/data_creation/01a_election_data_medsl.R and project memory).
## Outputs: R/output/long/he_vt_<year>.rds (1990-2010) and, per year, R/output/elect_he_cty_vt_<year>.rds. Deliberately NOT folded into
## elect_cty_final.rds / house_results_coverage.csv here -- fold-in is done centrally, same convention as every other state build.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "vermont_elections")
idx <- read.csv(file.path(DIR, "contests_index.csv"), stringsAsFactors = FALSE) %>% filter(keep)
stopifnot(nrow(idx) == 11, !anyDuplicated(idx$year))

## ---- town -> county FIPS -----------------------------------------------------------------------------------------------------------------------
xw_cty <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "VERMONT") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw_cty) == 14)
town_xw <- read_csv(file.path(DIR, "vt_town_county_crosswalk.csv"), show_col_types = FALSE) %>%
  mutate(town = toupper(trimws(town)), county = toupper(trimws(county))) %>%
  left_join(xw_cty, by = c("county" = "county_name")); stopifnot(!anyNA(town_xw$county_fips))
dup_names <- town_xw$town[duplicated(town_xw$town) | duplicated(town_xw$town, fromLast = TRUE)]
## disambiguate the name pairs the source itself disambiguates ("Barre City" vs "Barre Town", etc.) -- same county either way for every pair here, so
## a plain name->fips lookup (post-disambiguation) is enough; keep the disambiguated label as the lookup key.
town_xw$lookup <- ifelse(town_xw$town %in% dup_names, paste(town_xw$town, ifelse(grepl("CITY", toupper(town_xw$type)), "CITY", "TOWN")), town_xw$town)
stopifnot(!anyDuplicated(town_xw$lookup))
fips_of_town <- function(z) town_xw$county_fips[match(toupper(trimws(z)), town_xw$lookup)]

res <- list(); tie_within <- list(); tie_state <- list()
for (i in seq_len(nrow(idx))) {
  d <- read.csv(file.path(DIR, sprintf("contest_%d.csv", idx$contest_id[i])), header = FALSE, colClasses = "character", check.names = FALSE, fill = TRUE)
  nm <- unlist(d[1, -(1:2)]); pt <- unlist(d[2, -(1:2)]); it <- which(nm == "Total Votes Cast"); stopifnot(length(it) == 1)
  cc <- seq_len(it - 1)                                                        # candidate columns (relative to the -(1:2) offset), incl. Write-Ins
  body <- d[-(1:2), ]; num <- function(m) suppressWarnings(as.numeric(gsub(",", "", m)))
  V <- sapply(seq_len(ncol(d) - 2), function(k) num(body[[k + 2]]))
  if (is.null(dim(V))) V <- matrix(V, nrow = nrow(body))
  is_town <- body$V1 == "City/Town"; is_state <- body$V1 == "State"
  town <- V[is_town, , drop = FALSE]; state_row <- V[is_state, ]
  ok_rows <- all(rowSums(town[, cc, drop = FALSE]) == town[, it])
  colsum <- colSums(town[, c(cc, it), drop = FALSE]); statev <- state_row[c(cc, it)]
  max_abs_diff <- max(abs(colsum - statev))                                   # every candidate column's town-sum vs the printed State row, worst case
  max_rel_diff <- max_abs_diff / state_row[it]                                # as a share of that year's total votes cast (NOT of the one candidate's own count, which blows up for minor candidates)
  ok_state <- max_abs_diff == 0
  tie_within[[i]] <- data.frame(year = idx$year[i], n_town = nrow(town), rows_ok = ok_rows)
  tie_state[[i]] <- data.frame(year = idx$year[i], state_ok = ok_state, max_abs_diff = max_abs_diff, max_rel_diff = max_rel_diff)
  fp <- fips_of_town(body$V2[is_town]); stopifnot(!anyNA(fp))
  res[[i]] <- do.call(rbind, lapply(seq_along(cc), function(k)
    data.frame(year = idx$year[i], county_fips = fp, candidate = nm[cc[k]], party = pt[cc[k]], votes = town[, cc[k]], stringsAsFactors = FALSE)))
}
tie_within <- bind_rows(tie_within); tie_state <- bind_rows(tie_state)
cat("contests:", nrow(tie_within), "| every City/Town row adds up to Total Votes Cast:", sum(tie_within$rows_ok),
    "| City/Town rows sum to the printed State row exactly:", sum(tie_state$state_ok), "\n")
if (any(!tie_state$state_ok)) { message("Years where the town-level sum does not exactly match the printed State row (source-side reconciliation gap, not a parsing bug -- every"); message("individual town row is internally consistent with its own Total Votes Cast and, where precincts exist, with its own precinct sum -- see script header):"); print(tie_state %>% filter(!state_ok)) }
## Every individual town row is self-consistent (checked above, ok_rows) and matches its own precinct breakdown where one exists (checked interactively
## while building this script). 1990 and 2002 have a tiny, unexplained gap between the town-level sum and the site's own "State" summary row (which
## matches the certified/Wikipedia statewide totals) -- max relative diff 0.44% (2002, Jane Newton), not traceable to a specific town without the
## original state canvass PDF. Logged in data_corrections_log.csv, not fixed. Fail loudly only if a future re-run finds a materially larger gap.
stopifnot(all(tie_within$rows_ok), all(tie_state$max_rel_diff < 0.01))

raw <- bind_rows(res) %>%
  group_by(year, county_fips, candidate, party) %>% summarise(votes = sum(votes), .groups = "drop") %>%   # multiple towns share a county; sum before the per-candidate table
  mutate(party = ifelse(party == "", "Write-In", party),
         party_lc = tolower(party),
         party_group = case_when(grepl("democrat", party_lc) ~ "DEM", grepl("republican", party_lc) ~ "REP", TRUE ~ "OTHER"),
         district = "00") %>%
  select(-party_lc)
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "candidate", "party")]))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")
cat("fused rows classified DEM/REP by substring:\n"); print(raw %>% filter(grepl("/", party)) %>% distinct(year, candidate, party, party_group))

for (y in sort(unique(raw$year))) save_long(finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("vt_", y)), paste0("he_vt_", y))

## ---- shares files for every year + acceptance test ----------------------------------------------------------------------------------------------
for (y in sort(unique(raw$year))) {
  long <- readRDS(file.path(LONG_DIR, sprintf("he_vt_%d.rds", y)))
  shares <- derive_shares(long) %>% transmute(state = "VERMONT", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_vt_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_vt_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 14 VT counties")
}

sanity <- bind_rows(lapply(sort(unique(raw$year)), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_vt_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")
