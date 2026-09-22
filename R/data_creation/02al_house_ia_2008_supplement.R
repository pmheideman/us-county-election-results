## Iowa U.S. House 2008: fills the 10-county gap in the existing OpenElections-sourced build (he_ia.rds / elect_he_cty_ia via 01ad, 89 of
## 99 counties -- Page, Plymouth, Pottawattamie, Ringgold, Sac, Shelby, Sioux, Taylor, Union, Woodbury all missing, all in the old
## District 5, IA's southwest/northwest corner). Source: R/data/county_house_files/IA_OfficialCanvass2008General.pdf, the State of Iowa's
## own Official Canvass Summary -- BORN-DIGITAL (clean text layer, unlike the 1990s scanned books), pages 20-21, "United States
## Representative District 5" (Steve King R / Rob Hubler D / Victor Vara Independent). Transcribed to
## R/data/county_house_files/iowa/transcribed/2008_d5_supplement.csv directly from pdftotext output (verified clean, no OCR involved).
##
## This is a SUPPLEMENT, not a replacement -- the existing 89 counties in he_ia.rds / elect_he_cty_ia.rds for 2008 are untouched. Output
## here (he_ia_2008_supp.rds / elect_he_cty_ia_2008_supp.rds) covers ONLY these 10 counties; the coordinating session adds them to the
## panel and, if it wants a single unified 2008 long table, merges this into he_ia.rds's 2008 rows (not done here, per this project's
## convention that fold-in into shared multi-source files is done centrally, not inside an individual build script).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

TDIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "iowa", "transcribed")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "IOWA", !is.na(county_fips)) %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
fips_of <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$county_name)]

gap_counties <- c("PAGE", "PLYMOUTH", "POTTAWATTAMIE", "RINGGOLD", "SAC", "SHELBY", "SIOUX", "TAYLOR", "UNION", "WOODBURY")

raw <- read_csv(file.path(TDIR, "2008_d5_supplement.csv"), show_col_types = FALSE) %>%
  mutate(year = 2008, county_fips = fips_of(county), district = "05",
         party_group = case_when(grepl("DEM", toupper(party)) ~ "DEM", grepl("REP", toupper(party)) ~ "REP", TRUE ~ "OTHER"))
stopifnot(!anyNA(raw$county_fips), nrow(raw) == 10 * 3, sort(unique(raw$county)) == sort(gap_counties))

## Confirm these 10 counties are genuinely the ones missing from the existing OpenElections-sourced 2008 build (not a duplicate/overlap).
existing <- readRDS(file.path(LONG_DIR, "he_ia.rds")) %>% filter(year == 2008)
stopifnot(length(intersect(unique(existing$county_fips), unique(raw$county_fips))) == 0)
message("existing 2008 IA counties: ", n_distinct(existing$county_fips), " | new counties from this supplement: ", n_distinct(raw$county_fips),
        " | combined: ", n_distinct(existing$county_fips) + n_distinct(raw$county_fips), " (should be 99)")

long <- finalize_long(raw %>% select(year, county_fips, district, candidate, party, party_group, votes), "ia_2008_supp")
save_long(long, "he_ia_2008_supp")
shares <- derive_shares(long) %>% transmute(state = "IOWA", year, cty_fips, sample, demovote, repuvote, totalvote)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_ia_2008_supp.rds"))
r <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ia_2008_supp.rds")); stopifnot(all(r$pass))
message("2008 supplement: ", nrow(shares), " of 10 gap counties")

share <- shares$repuvote + shares$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")
win <- long %>% group_by(candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% arrange(desc(votes))
message("Candidate totals across these 10 counties: ", paste(sprintf("%s=%d", win$candidate, win$votes), collapse = "; "))
