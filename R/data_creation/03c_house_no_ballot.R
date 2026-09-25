## House seats with NO county returns because the candidate was unopposed and the state does not put (or does not count) unopposed
## candidates on the ballot: Florida, Louisiana and Oklahoma leave them off the ballot; Arkansas prints them but does not tabulate their votes.
## These are structural "no ballot" gaps, distinct from data we have not found.
##
## Input:  R/data/house_unopposed_no_ballot.csv -- hand-curated, one row per (year, state, district) with the unopposed winner and the source that
##         says so: FEC "Federal Elections" books (1990: text "Unopposed"; 2004-2022: GENERAL VOTES = "Unopposed"), House Clerk "Statistics of the
##         Congressional Election" (1992-2002, 2024: footnoted "(1)" in place of votes, with the state-law footnote). Checked 2026-09-25 against the
##         districts absent from house_long_all.rds: every absent FL/LA/OK/AR district-year is in the list except Florida 1996 districts 17, 18 and 21,
##         which were contested (Clerk 1996 prints their votes) and are genuinely missing data. Only these four states appear with no-ballot seats in
##         either source. Louisiana 1992/1994 (every seat decided outright in the October open primary, Clerk footnote) are not gaps: the release
##         holds those primary returns.
## Counties of each district:
##   same_map_year -- the counties that report that district in the nearest year drawn on the same map (MAP_PERIODS below), from our own returns
##   boundary_overlay -- 1990 (1980s map, only one election year in scope): UCLA historical district shapefiles (Lewis, DeVine, Pitcher & Martis,
##     cdmaps.polisci.ucla.edu, districts102.zip); Louisiana 2022 (map used only in 2022): Census TIGER 118th Congress districts. A county belongs to a
##     district when at least 1% of its area lies in it (drops boundary slivers from generalized geometry).
## Output: R/output/house_no_ballot_counties.csv -- one row per (year, state, district, county), with whole_county = TRUE when the county has no
##         other House returns that year (it is blank on the map), FALSE when other districts in the county were contested (its totals omit this seat).

source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
suppressMessages({ library(readr); library(sf); library(tigris) })
options(tigris_use_cache = TRUE)

nb <- read_csv(file.path(PROJECT_ROOT, "R", "data", "house_unopposed_no_ballot.csv"), col_types = cols(.default = col_character())) %>% mutate(year = as.integer(year))
STATE_RULE <- c(
  FL = "Florida law does not print the names of candidates with no opposition on the ballot.",
  LA = "Under Louisiana law (R.S. 18:511) an unopposed candidate is declared elected and does not appear on the ballot.",
  OK = "Oklahoma law does not print the names of candidates with no opposition on the ballot.",
  AR = "Arkansas law does not require votes for unopposed candidates to be counted, so no county returns exist.")
stopifnot(all(nb$state_po %in% names(STATE_RULE)))

## redistricting periods (years sharing one map) for the four states
MAP_PERIODS <- list(
  FL = list(1990, c(1992, 1994), c(1996, 1998, 2000), seq(2002, 2010, 2), c(2012, 2014), c(2016, 2018, 2020), c(2022, 2024)),
  LA = list(1990, 1992, 1994, c(1996, 1998, 2000), seq(2002, 2010, 2), seq(2012, 2020, 2), 2022, 2024),
  OK = list(1990, seq(1992, 2000, 2), seq(2002, 2010, 2), seq(2012, 2020, 2), c(2022, 2024)),
  AR = list(1990, seq(1992, 2000, 2), seq(2002, 2010, 2), seq(2012, 2020, 2), c(2022, 2024)))
period_of <- function(st, y) { p <- Filter(function(p) y %in% p, MAP_PERIODS[[st]]); stopifnot(length(p) == 1); p[[1]] }

long <- readRDS(file.path(LONG_DIR, "house_long_all.rds")) %>% mutate(county_fips = ifelse(county_fips == 12025, 12086, county_fips))
st_fips <- c(FL = 12, LA = 22, OK = 40, AR = 5)
hl <- long %>% filter(state_fips %in% st_fips) %>% distinct(year, state_fips, county_fips, district)

## sanity: every listed seat is absent from our returns (a seat with returns would contradict "no ballot")
clash <- nb %>% mutate(state_fips = st_fips[state_po]) %>% semi_join(hl, by = c("year", "state_fips", "district"))
if (nrow(clash)) stop("listed as no-ballot but returns exist: ", paste(clash$year, clash$state_po, clash$district, collapse = "; "))

## ---- counties from the same map in another year -------------------------------------------------------------------------------------------------------
from_same_map <- function(st, y, d) {
  yrs <- setdiff(period_of(st, y), y); yrs <- yrs[order(abs(yrs - y))]
  for (yy in yrs) { k <- hl %>% filter(year == yy, state_fips == st_fips[[st]], district == d) %>% pull(county_fips)
    if (length(k)) return(tibble(county_fips = sort(unique(k)), county_method = paste0("same_map_year:", yy))) }
  NULL
}

## ---- counties from district boundaries --------------------------------------------------------------------------------------------------------------
cty_sf <- tigris::counties(cb = TRUE, resolution = "500k", year = 2020, progress_bar = FALSE) %>% filter(STATEFP %in% sprintf("%02d", st_fips)) %>%
  st_transform(5070) %>% transmute(county_fips = as.numeric(GEOID)) %>% mutate(county_area = as.numeric(st_area(geometry)))
overlay <- function(cd_sf, d) {
  x <- suppressWarnings(st_intersection(cty_sf, st_make_valid(cd_sf %>% filter(district == d)))) %>% mutate(share = as.numeric(st_area(geometry)) / county_area)
  x %>% st_drop_geometry() %>% filter(share >= 0.01) %>% distinct(county_fips)
}
cd102 <- st_read(file.path(PROJECT_ROOT, "R", "data", "cd_boundaries", "districts102", "districts102.shp"), quiet = TRUE) %>%
  filter(STATENAME %in% c("Florida", "Louisiana", "Oklahoma", "Arkansas")) %>% mutate(state_po = c(Florida = "FL", Louisiana = "LA", Oklahoma = "OK", Arkansas = "AR")[STATENAME],
  district = sprintf("%02d", as.integer(DISTRICT))) %>% st_transform(5070)
cd118_la <- tigris::congressional_districts(state = "LA", year = 2022, cb = TRUE, progress_bar = FALSE) %>% mutate(state_po = "LA", district = sprintf("%02d", as.integer(CD118FP))) %>% st_transform(5070)
from_boundaries <- function(st, y, d) {
  if (y == 1990) return(overlay(cd102 %>% filter(state_po == st), d) %>% mutate(county_method = "boundary_overlay:UCLA districts102 (1990 election)"))
  if (st == "LA" && y == 2022) return(overlay(cd118_la, d) %>% mutate(county_method = "boundary_overlay:Census TIGER CD118"))
  NULL
}

rows <- lapply(seq_len(nrow(nb)), function(i) { r <- nb[i, ]
  k <- from_same_map(r$state_po, r$year, r$district); if (is.null(k)) k <- from_boundaries(r$state_po, r$year, r$district)
  if (is.null(k) || !nrow(k)) stop("no county set for ", r$year, " ", r$state_po, "-", r$district)
  bind_cols(r[rep(1, nrow(k)), ], k) })
out <- bind_rows(rows) %>% mutate(state_fips = unname(st_fips[state_po]))

## whole_county: the county has no House returns at all that year
has_returns <- hl %>% distinct(year, county_fips) %>% mutate(has = TRUE)
out <- out %>% left_join(has_returns, by = c("year", "county_fips")) %>% mutate(whole_county = is.na(has)) %>% select(-has)

## ---- check: every county with no returns in these state-years is explained, except the known genuinely-missing Florida 1996 seats -------------------
all_cty <- cty_sf %>% st_drop_geometry() %>% mutate(state_fips = county_fips %/% 1000)
sy <- nb %>% distinct(year, state_po) %>% mutate(state_fips = unname(st_fips[state_po]))
blank <- sy %>% inner_join(all_cty, by = "state_fips", relationship = "many-to-many") %>% anti_join(has_returns, by = c("year", "county_fips"))
unexplained <- blank %>% anti_join(out, by = c("year", "county_fips"))
message("counties with no House returns in the affected state-years: ", nrow(blank), " | explained by a no-ballot seat: ", nrow(blank) - nrow(unexplained))
if (nrow(unexplained)) { message("NOT explained (genuinely missing data):"); print(as.data.frame(unexplained %>% count(year, state_po))) }

out <- out %>% transmute(year, state_po, district, county_fips = sprintf("%05d", as.integer(county_fips)), candidate, party, whole_county,
                         state_rule = unname(STATE_RULE[state_po]), source, county_method) %>% arrange(year, state_po, district, county_fips)
write_csv(out, file.path(OUTPUT_DIR, "house_no_ballot_counties.csv"), na = "")
message("house_no_ballot_counties.csv: ", nrow(out), " rows, ", nrow(nb), " seats, ", sum(out$whole_county), " whole-county blanks, ", sum(!out$whole_county), " split counties")
print(as.data.frame(out %>% count(method = sub(":.*", "", county_method))))
