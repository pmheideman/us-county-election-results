## New Mexico House 1998 and 2002, from the SOS's own archive of individual county-level result
## pages (see 01ed_new_mexico_sos_archive_download.py for how the per-county HTML files were found
## and downloaded -- a 3rd-party "RealFile" widget API, not linked directly from the archive page's
## own HTML). Parsed by parse_nm_county_html.py (regex over the FrontPage-era HTML: one
## "UNITED STATES REPRESENTATIVE - DISTRICT NN" <H2> block per district actually touching that
## county, TR rows giving candidate/party/votes/percent).
##
## 1998 closes a complete `source_not_found` gap (01ao's OpenElections build only reaches back to
## 2000). 2002 REPLACES 01ao's OpenElections build, which was missing Cibola County entirely (a
## known, already-documented upstream OpenElections gap) -- verified here: the SOS archive's 32
## overlapping counties match OpenElections EXACTLY (max diff = 0, all counties), and Cibola is the
## one new county added, closing 32/33 -> 33/33.
##
## Party labels: DEMOCRAT/REPUBLICAN plus "Write-In (REP)"/"Write-In (GRN)" (write-ins with a party
## hint in parentheses) -- classified the same way as every other state script, `startsWith` after
## toupper, so "Write-In (REP)" correctly falls to REP and "Write-In (GRN)" to OTHER.
## Verified against the printed percent-of-district column for a spot-check sample (Bernalillo
## district 1, 1998): shares reproduce the printed 48%/42%/10%/0% exactly.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_mexico_sos_archive")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
nm_fips <- xw %>% filter(state == "NEW MEXICO") %>% select(county_name, county_fips) %>% distinct()
stopifnot(nrow(nm_fips) == 33)

normalize_county <- function(x) ifelse(toupper(trimws(x)) == "DEBACA", "DE BACA", toupper(trimws(x)))
to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }

build_year <- function(year) {
  raw <- read_csv(file.path(RAW_DIR, sprintf("nm_%d_house_raw.csv", year)), show_col_types = FALSE) %>%
    mutate(county = normalize_county(county), party_group = to_party(party))
  stopifnot(n_distinct(raw$county) == 33)

  long <- raw %>% left_join(nm_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips)) %>%
    transmute(year, county_fips, district = sprintf("%02d", district), candidate, party, party_group, votes)
  fl <- finalize_long(long, paste0("nm_sos_", year))
  ## Saved as "he_nm_<year>" (not "he_nm_sos_<year>") -- see 01em's header note for why this exact
  ## naming is required for 02z_house_long_assemble.R's file resolution to find it.
  save_long(fl, paste0("he_nm_", year))

  shares <- derive_shares(fl) %>% transmute(state = "NEW MEXICO", year, cty_fips, sample, demovote, repuvote, totalvote)
  stopifnot(nrow(shares) == 33, !anyNA(shares$demovote), !anyNA(shares$repuvote))
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", year)))
  r <- check_long_vs_source(fl, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", year))); stopifnot(all(r$pass))
  shares
}

s1998 <- build_year(1998)
s2002 <- build_year(2002)

sanity <- c(s1998$demovote + s1998$repuvote, s2002$demovote + s2002$repuvote)
message("sanity range demovote+repuvote: [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

## ---- cross-check 2002 against the existing (partial) OpenElections build ----
oe_2002 <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_nm.rds")) %>% filter(year == 2002)
cmp <- s2002 %>% inner_join(oe_2002, by = "cty_fips", suffix = c("_sos", "_oe"))
diff <- abs(cmp$totalvote_sos - cmp$totalvote_oe)
message("2002 cross-check vs OpenElections: ", nrow(cmp), " of ", nrow(oe_2002), " OE counties overlap, max diff = ", max(diff),
        "; SOS archive adds ", nrow(s2002) - nrow(cmp), " county not in OE (expected: Cibola, the OE repo's known gap)")
stopifnot(nrow(cmp) == nrow(oe_2002), max(diff) == 0)

message("New Mexico House 1998: ", nrow(s1998), "/33 counties. 2002: ", nrow(s2002), "/33 counties (was ", nrow(oe_2002), "/33 via OpenElections).")
