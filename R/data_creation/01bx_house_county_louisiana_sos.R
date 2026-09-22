## Louisiana parish-level U.S. House results, 1990-2014 (and 2016-2024 for cross-checking), from the
## Louisiana SOS Graphical Election Results app. Raw files fetched by 01bw_louisiana_results_download.R.
##
## Source layout is uniform across every year: one CSV per race, columns Office, Parish, then one
## column per candidate named "Name (PARTY)" with tags DEM/REP/OTH/IND/LBT/GRN/NOPTY. 64 parishes,
## no total/statewide pseudo-rows (checked in the audit), no non-numeric votes.
##
## ---- Decisive-round rule (same idea as 01ae, made explicit) ----------------------------------------
## Louisiana's congressional system changed several times, so one district-year can have several race
## files (open/jungle primary, closed party primaries, general, runoff). Rule, per (year, district):
##   1. Drop CLOSED PARTY PRIMARIES (2008-10-04, 2010-08-28, 2010-10-02): asserted below to contain only
##      one party's candidates per race. They are nominations, not the election.
##   2. Of the remaining races, take the LATEST date: the runoff/general if one was held, otherwise the
##      first-round open primary that decided the seat outright.
## Same-party runoffs (e.g. 2006 LA-2 Jefferson v Carter, both Democrats) therefore give parishes with
## zero votes for the other major party, exactly as in 01ae; the winner table printed below is checked
## against known outcomes. The full all-rounds long table is saved so this rule can be changed later.
##
## Districts with NO race file are unopposed seats: Louisiana elects unopposed candidates without a
## ballot, so no votes exist anywhere. That is a structural gap (parishes wholly inside such a district
## get no row; split parishes get their contested portion only) and is reported, never filled.
##
## Outputs: R/output/la_sos_house_parish_long.rds   (all rounds, long)
##          R/output/elect_he_cty_la_sos.rds        (decisive round, YEARS 1990-2014 only, panel schema)
## 2016-2024 stays MEDSL by project convention; here they are only cross-checked (first round, since
## that is what MEDSL carries). Does NOT touch elect_cty_final.rds / the coverage tracker.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "louisiana_sos")
manifest <- read.csv(file.path(RAW_DIR, "_manifest.csv"), stringsAsFactors = FALSE)
stopifnot(all(manifest$downloaded))

## ---- Read every race file into one long table ---------------------------------------------------
long <- purrr::map_dfr(seq_len(nrow(manifest)), function(i) {
  x <- suppressMessages(read_csv(file.path(RAW_DIR, manifest$date[i], paste0("ByParish_", manifest$race_id[i], ".csv")),
                                 col_types = cols(.default = "c"), show_col_types = FALSE))
  stopifnot(names(x)[1] == "Office", names(x)[2] == "Parish")
  x %>% pivot_longer(-(1:2), names_to = "cand_raw", values_to = "votes_chr") %>%
    transmute(date = manifest$date[i], race_id = manifest$race_id[i], office = Office, parish = Parish,
              cand_raw, votes = as.numeric(gsub(",", "", votes_chr)))
}) %>%
  mutate(year = as.integer(substr(date, 1, 4)),
         district = as.integer(str_match(office, "-- *([0-9]+)")[, 2]),
         party = str_match(cand_raw, "\\(([^()]*)\\)\\s*$")[, 2],
         candidate = trimws(sub("\\s*\\([^()]*\\)\\s*$", "", cand_raw)))
stopifnot(!anyNA(long$votes), !anyNA(long$district), !anyNA(long$party),
          all(long$party %in% c("DEM", "REP", "OTH", "IND", "LBT", "GRN", "NOPTY")))
save_step(long, "la_sos_house_parish_long")

## ---- Parish -> FIPS (join on normalized name; crosswalk carries dual spellings for some parishes) ----
norm <- function(x) gsub("[^A-Z]", "", toupper(x))
me_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "LOUISIANA", !is.na(county_fips)) %>%
  distinct(county_name, county_fips) %>% mutate(key = norm(county_name)) %>%
  distinct(key, county_fips)
stopifnot(!anyDuplicated(me_fips$key), nrow(me_fips) == 64)
long <- long %>% mutate(key = norm(parish)) %>% left_join(me_fips, by = "key")
if (anyNA(long$county_fips)) { print(unique(long$parish[is.na(long$county_fips)])); stop("unmatched parish names") }

## ---- Round selection --------------------------------------------------------------------------------
races <- long %>% group_by(date, race_id, year, district) %>%
  summarise(n_cand = n_distinct(candidate), parties = paste(sort(unique(party)), collapse = "/"), .groups = "drop")

CLOSED_PRIMARY_DATES <- c("20081004", "20100828", "20101002")
closed <- races %>% filter(date %in% CLOSED_PRIMARY_DATES)
## assert these really are single-party nominating primaries
stopifnot(all(!grepl("/", closed$parties)))
message("closed primaries dropped: ", nrow(closed), " races (all single-party)")

decisive <- races %>% filter(!date %in% CLOSED_PRIMARY_DATES) %>%
  group_by(year, district) %>% filter(date == max(date)) %>% ungroup()
stopifnot(!anyDuplicated(decisive[, c("year", "district")]))       # exactly one race per district-year
message("decisive races: ", nrow(decisive))

first_round <- races %>% filter(!date %in% CLOSED_PRIMARY_DATES) %>%
  group_by(year, district) %>% filter(date == min(date)) %>% ungroup()    # for the MEDSL cross-check

## ---- Winners table (verification against known outcomes) ----------------------------------------------
winners <- long %>% semi_join(decisive, by = c("race_id")) %>%
  group_by(year, district, date, candidate, party) %>% summarise(v = sum(votes), .groups = "drop") %>%
  group_by(year, district) %>% arrange(desc(v), .by_group = TRUE) %>%
  summarise(date = first(date), winner = first(candidate), wparty = first(party), wshare = round(first(v) / sum(v), 3),
            runner_up = nth(candidate, 2), rparty = nth(party, 2), .groups = "drop")
message("\n== decisive-round winners (check against known results) ==")
print(winners %>% filter(year <= 2014), n = 100, width = 200)

## ---- District coverage per year (unopposed seats = no ballot = no data) --------------------------------------
n_seats <- function(y) ifelse(y < 1992, 8, ifelse(y < 2012, 7, 6))
cover <- decisive %>% count(year, name = "districts_with_race") %>% mutate(seats = n_seats(year),
  missing_districts = purrr::map_chr(year, function(y) {
    m <- setdiff(seq_len(n_seats(y)), decisive$district[decisive$year == y]); paste(m, collapse = ",") }))
message("\n== districts with a race vs seats (missing = unopposed, no ballot) ==")
print(cover, n = 40)

## ---- Aggregate to parish-year, summing split parishes across districts ------------------------------------
agg <- function(sel) {
  long %>% semi_join(sel, by = "race_id") %>%
    group_by(year, county_fips) %>%
    summarise(totalvote = sum(votes),
              demovote_n = sum(votes[party == "DEM"]),
              repuvote_n = sum(votes[party == "REP"]), .groups = "drop") %>%
    filter(totalvote > 0)
}
la_all <- agg(decisive)

elect_he_cty_la_sos <- la_all %>% filter(year <= 2014) %>%
  transmute(state = "LOUISIANA", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(year, cty_fips) %>%
  save_step("elect_he_cty_la_sos")

## 2024 is saved SEPARATELY: MEDSL's own raw 2024 file carries only ~957K Louisiana House votes (roughly half;
## e.g. Mike Johnson 137,756 vs the SOS District 4 total of 306,248) whereas the SOS total is 1,905,718, close to the
## 2,006,975 presidential total. 2016-2022 first-round matches MEDSL exactly in every parish, so SOS is the reliable side.
## Replacing MEDSL's 2024 LA rows in the panel is a separate, explicit decision (not done by this script).
save_step(la_all %>% filter(year == 2024) %>%
  transmute(state = "LOUISIANA", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>% arrange(cty_fips),
  "elect_he_cty_la_sos_2024")

message("\n== parishes covered per year (of 64) ==")
print(table(elect_he_cty_la_sos$year))
message("two-party sum range by year:")
print(elect_he_cty_la_sos %>% group_by(year) %>% summarise(min = round(min(demovote + repuvote), 3), max = round(max(demovote + repuvote), 3)), n = 40)

## ---- Cross-check: FIRST-ROUND 2016-2024 vs MEDSL (what MEDSL carries) ------------------------------------------
medsl <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds")) %>%
  filter(cty_fips %/% 1000 == 22, year >= 2016) %>% select(year, cty_fips, m_dem = demovote, m_rep = repuvote, m_tot = totalvote)
mine <- agg(first_round) %>% filter(year >= 2016) %>%
  transmute(year, cty_fips = county_fips, dem = demovote_n / totalvote, rep = repuvote_n / totalvote, tot = totalvote)
xc <- inner_join(mine, medsl, by = c("year", "cty_fips")) %>%
  mutate(d_dem = abs(dem - m_dem), d_rep = abs(rep - m_rep), d_tot = abs(tot - m_tot) / m_tot)
message("\n== first-round vs MEDSL, LA 2016-2024 ==")
print(xc %>% group_by(year) %>% summarise(matched = n(), medsl_only = NA_integer_, max_dem = round(max(d_dem), 4),
      max_rep = round(max(d_rep), 4), median_tot_reldiff = round(median(d_tot), 4), max_tot_reldiff = round(max(d_tot), 4)), n = 10)
message("parishes in MEDSL but not in first-round SOS build, and vice versa:")
print(bind_rows(anti_join(medsl, mine, by = c("year", "cty_fips")) %>% count(year) %>% mutate(what = "MEDSL only"),
                anti_join(mine, medsl, by = c("year", "cty_fips")) %>% count(year) %>% mutate(what = "SOS only")))
