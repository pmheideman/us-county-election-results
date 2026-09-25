## Builds the Shiny app's data files from the v0.2.0 release CSVs + county geometry. Run once (and any time the
## release is rebuilt); the app itself only reads the .rds files this script writes to shiny_app/data/.
##
## Outputs (all in shiny_app/data/):
##   counties_sf.rds   -- one row per county (sf polygons, EPSG:4326), 48 states (AK/HI excluded for now, see below)
##   results.rds       -- one row per (office, year, county_fips): shares, vote counts, gap_reason/status, n_districts
##   candidates.rds    -- one row per (office, year, county_fips, district, candidate): for the click/hover detail panel
##   no_ballot.rds     -- one row per (year, House district, county) where the unopposed winner was not on the ballot / not tabulated
##   meta.rds          -- small lookup: valid years per office, state list, etc.
##
## AK/HI note: the user plans to add Alaska and Hawaii later. Geometry is kept in real lat/lon (no Albers/USA
## projection, no manual insetting of AK/HI) specifically so that adding them later is just "add more rows to
## counties_sf and results" -- no reprojection or layout rework needed. tigris::counties() already returns all
## states; this script filters to the 48 in scope today via EXCLUDED_FIPS below, not by hardcoding a state list,
## so re-enabling AK/HI is a one-line change.
##
## Known non-county / historical-FIPS wrinkles handled here (see docs/DECISIONS.md section 5):
##  - Shannon County SD (46113, pre-2015) -> displayed on Oglala Lakota County's current polygon (46102)
##  - Bedford City VA (51515, pre-2013) -> Bedford County (51019)
##  - Clifton Forge City VA (51560, pre-2001) -> Alleghany County (51005)
##  - South Boston City VA (51780, pre-1995) -> Halifax County (51083)
##  - Two Kansas City MO non-county buckets ("36000", "2938000") used to leak through 03a/03d's own exclusion filter
##    because they were baked into the county crosswalk itself; fixed upstream 2026-09-22 (see data_corrections_log.csv)
##    by excluding them before the crosswalk is built, so the release CSVs this script reads no longer contain them.

source(file.path("R", "00_setup.R"))
suppressMessages({
  library(sf)
  library(tigris)
  library(readr)
})

APP_DATA_DIR <- file.path(PROJECT_ROOT, "shiny_app", "data")
dir.create(APP_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
options(tigris_use_cache = TRUE)
Sys.setenv(TIGRIS_CACHE_DIR = file.path(APP_DATA_DIR, "tigris_cache"))
dir.create(Sys.getenv("TIGRIS_CACHE_DIR"), showWarnings = FALSE, recursive = TRUE)

REL <- file.path(PROJECT_ROOT, "release", "v0.2.0")

## ---- 1. County geometry --------------------------------------------------------------------------------------
## Cartographic boundary (cb=TRUE), 1:20,000,000 -- already generalized/lightweight, no extra simplification needed.
counties_raw <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020, progress_bar = FALSE) %>%
  st_transform(4326) %>%
  transmute(county_fips = GEOID, county_name_geo = NAME, state_fips_geo = STATEFP)

## Territories/AK/HI excluded FOR NOW (see header note) -- identified by state FIPS, not hardcoded state names,
## so this is the only line to touch when AK/HI are added.
EXCLUDED_STATE_FIPS <- c("02", "15", "72", "60", "66", "69", "78")   # AK, HI, PR, American Samoa, Guam, N. Mariana Is., US Virgin Is.
counties_sf <- counties_raw %>% filter(!state_fips_geo %in% EXCLUDED_STATE_FIPS)
message("counties_sf: ", nrow(counties_sf), " counties (48 states)")

## State outlines, drawn over the counties. The same 1:20m cartographic file, so the lines match the county edges.
states_sf <- tigris::states(cb = TRUE, resolution = "20m", year = 2020, progress_bar = FALSE) %>%
  st_transform(4326) %>%
  transmute(state_fips_geo = STATEFP) %>%
  filter(state_fips_geo %in% counties_sf$state_fips_geo)

## ---- 2. Results summary (drives map fill) --------------------------------------------------------------------
summ <- read_csv(file.path(REL, "us_county_results_summary.csv"), show_col_types = FALSE, col_types = cols(county_fips = col_character(), state_fips = col_character()))

## Historical-FIPS -> current-polygon crosswalk (docs/DECISIONS.md section 5). Remap, then re-aggregate in case a
## year has BOTH the historical and current code (shouldn't happen for these four, but summing is safe either way).
FIPS_CROSSWALK <- c("46113" = "46102", "51515" = "51019", "51560" = "51005", "51780" = "51083")
summ <- summ %>%
  mutate(county_fips = recode(county_fips, !!!FIPS_CROSSWALK)) %>%
  group_by(office, year, county_fips) %>%
  summarise(state_fips = first(state_fips), state = first(state), state_po = first(state_po), county_name = first(county_name),
            n_districts = if (all(is.na(n_districts))) NA_real_ else max(n_districts, na.rm = TRUE), dem_votes = sum(dem_votes), rep_votes = sum(rep_votes),
            other_votes = sum(other_votes), total_votes = sum(total_votes), status = first(status),
            quality_flag = paste(unique(na.omit(quality_flag)), collapse = ";"), .groups = "drop") %>%
  mutate(dem_two_party_share = ifelse(dem_votes + rep_votes > 0, dem_votes / (dem_votes + rep_votes), NA_real_),
         rep_share_of_total = ifelse(total_votes > 0, rep_votes / total_votes, NA_real_),
         quality_flag = na_if(quality_flag, ""))

not_in_geo <- setdiff(unique(summ$county_fips), counties_sf$county_fips)
if (length(not_in_geo) > 0) message("NOTE: ", length(not_in_geo), " result county_fips still unmatched to geometry after crosswalk (dropped from map, kept in CSV): ", paste(not_in_geo, collapse = ", "))
summ <- summ %>% filter(county_fips %in% counties_sf$county_fips)

## ---- 3. Gaps (state-year level; used to label grey counties with a reason) ------------------------------------
gaps <- read_csv(file.path(REL, "us_county_results_gaps.csv"), show_col_types = FALSE) %>%
  mutate(state = toupper(state))
## state fips<->name/po lookup, built from the summary file itself (already has all three)
state_lookup <- summ %>% distinct(state_fips, state, state_po) %>% mutate(state_upper = toupper(state))
gaps <- gaps %>% left_join(state_lookup %>% select(state_upper, state_po, state_fips), by = c("state" = "state_upper"))

## ---- 4. Candidate-level long table (click/hover detail) -------------------------------------------------------
long <- read_csv(file.path(REL, "us_county_results_long.csv"), show_col_types = FALSE,
                  col_types = cols(county_fips = col_character(), state_fips = col_character(), district = col_character()))
long <- long %>% mutate(county_fips = recode(county_fips, !!!FIPS_CROSSWALK)) %>%
  filter(county_fips %in% counties_sf$county_fips) %>%
  select(year, office, county_fips, district, candidate, party, party_group, votes, quality_flag)

## ---- 4b. House seats with no ballot (unopposed): explains grey counties that are NOT missing data ------------------------------------------
no_ballot <- read_csv(file.path(REL, "us_county_results_no_ballot.csv"), show_col_types = FALSE, col_types = cols(.default = col_character())) %>%
  transmute(office, year = as.integer(year), county_fips, state, state_po, district, candidate, party_group, whole_county = whole_county == "yes", state_rule)

## ---- 5. Meta: valid years per office (drives the year selector) ------------------------------------------------
meta <- list(
  years_by_office = summ %>% distinct(office, year) %>% arrange(office, year) %>% group_by(office) %>% summarise(years = list(sort(year))) %>% tibble::deframe(),
  bounds = st_bbox(counties_sf)   # for the initial map view; AK/HI addition later will just widen this
)

## ---- write -------------------------------------------------------------------------------------------------
saveRDS(counties_sf, file.path(APP_DATA_DIR, "counties_sf.rds"))
saveRDS(states_sf, file.path(APP_DATA_DIR, "states_sf.rds"))
saveRDS(summ, file.path(APP_DATA_DIR, "results.rds"))
saveRDS(gaps, file.path(APP_DATA_DIR, "gaps.rds"))
saveRDS(long, file.path(APP_DATA_DIR, "candidates.rds"))
saveRDS(no_ballot, file.path(APP_DATA_DIR, "no_ballot.rds"))
saveRDS(meta, file.path(APP_DATA_DIR, "meta.rds"))

message("Wrote: counties_sf (", nrow(counties_sf), " rows), results (", nrow(summ), " rows), gaps (", nrow(gaps), " rows), candidates (", nrow(long), " rows)")
message("Years by office:"); print(meta$years_by_office)
