## New Mexico House 2004 and 2006, from the user-supplied born-digital statewide canvass PDFs
## (`R/data/county_house_files/NM_StatewideGen04.pdf`, `NM_StatewideGen06.pdf`). Parsed by
## 01eh_new_mexico_statewide_pdf_parse.py (see its header for the wide-table layout and the
## per-year office-boundary quirk); every row's county-sum verified exactly against the source's own
## printed "TOTAL FOR EACH CANDIDATE" column before this script runs (the parser stops on mismatch).
##
## Closes what were `source_not_found` (2004) and `none` coverage (2006) gaps -- 2004/2006 previously
## had no county-level House source at all despite America Votes district-level data existing.
##
## Party comes through as "DEMOCRATIC PARTY"/"REPUBLICAN PARTY" (2004/2006's own label style,
## distinct from 2000/2002's plain "DEMOCRAT"/"REPUBLICAN" already seen in 01ao/01ee) -- classified
## the same `startsWith`-after-toupper way as every other NM script, so no alias table needed.
## 2006 district 2's write-in candidate (C. Dean Burk) has a garbled party field in the source text
## (the OCR/extraction artifact "UNITED STATES REPRESENTATIVE" instead of blank) -- falls through to
## OTHER automatically since that string doesn't start with DEM or REP, which is the correct
## treatment for an unlabeled write-in anyway.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "new_mexico_statewide_pdf")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
nm_fips <- xw %>% filter(state == "NEW MEXICO") %>% select(county_name, county_fips) %>% distinct()
stopifnot(nrow(nm_fips) == 33)

to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }

## CSV column name (as printed in the source, matching COUNTIES_DISPLAY in the .py parser) -> the
## crosswalk's plain-ASCII spelling. Only the multi-word/abbreviated names need an explicit alias;
## single-word names match after toupper() with no alias needed.
DISPLAY_TO_XW <- c(
  Bernalillo = "BERNALILLO", Catron = "CATRON", Chaves = "CHAVES", Cibola = "CIBOLA", Colfax = "COLFAX",
  Curry = "CURRY", DeBaca = "DE BACA", `Dona Ana` = "DONA ANA", Eddy = "EDDY", Grant = "GRANT",
  Guadalupe = "GUADALUPE", Harding = "HARDING", Hidalgo = "HIDALGO", Lea = "LEA", Lincoln = "LINCOLN",
  `Los Alamos` = "LOS ALAMOS", Luna = "LUNA", McKinley = "MCKINLEY", Mora = "MORA", Otero = "OTERO",
  Quay = "QUAY", `Rio Arriba` = "RIO ARRIBA", Roosevelt = "ROOSEVELT", Sandoval = "SANDOVAL",
  `San Juan` = "SAN JUAN", `San Miguel` = "SAN MIGUEL", `Santa Fe` = "SANTA FE", Sierra = "SIERRA",
  Socorro = "SOCORRO", Taos = "TAOS", Torrance = "TORRANCE", Union = "UNION", Valencia = "VALENCIA")
stopifnot(setequal(unname(DISPLAY_TO_XW), nm_fips$county_name))

build_year <- function(year) {
  raw <- read_csv(file.path(RAW_DIR, sprintf("nm_%d_house.csv", year)), show_col_types = FALSE)
  county_display_cols <- names(DISPLAY_TO_XW)
  stopifnot(all(county_display_cols %in% names(raw)))
  long <- raw %>%
    pivot_longer(cols = all_of(county_display_cols), names_to = "county_display", values_to = "votes") %>%
    mutate(county_name = DISPLAY_TO_XW[county_display]) %>%
    left_join(nm_fips, by = "county_name") %>%
    filter(!is.na(county_fips), votes > 0) %>%
    transmute(year, county_fips, district = sprintf("%02d", district), candidate, party, party_group = to_party(party), votes)
  ## Source label "nm_<year>" (not "nm_statewide_pdf_<year>") to match source_registry.csv's base
  ## token for this row ("nm", 2004-2006, narrower span than OpenElections' "nm" 2000-2014 row so it
  ## wins the collision by the registry's own narrowest-span-wins tiebreak) -- see that row's notes.
  fl <- finalize_long(long, paste0("nm_", year))
  ## Saved as "he_nm_<year>" (not "he_nm_statewide_pdf_<year>") -- see 01em's header note for why
  ## this exact naming is required for 02z_house_long_assemble.R's file resolution to find it.
  save_long(fl, paste0("he_nm_", year))

  shares <- derive_shares(fl) %>% transmute(state = "NEW MEXICO", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", year)))
  r <- check_long_vs_source(fl, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", year))); stopifnot(all(r$pass))
  shares
}

s2004 <- build_year(2004)
s2006 <- build_year(2006)

for (yr_shares in list(list(y = 2004, s = s2004), list(y = 2006, s = s2006))) {
  missing <- nm_fips %>% filter(!county_fips %in% yr_shares$s$cty_fips)
  message(yr_shares$y, ": ", nrow(yr_shares$s), "/33 counties", if (nrow(missing) > 0) paste0(" -- missing: ", paste(missing$county_name, collapse = ", ")) else "")
}

sanity <- c(s2004$demovote + s2004$repuvote, s2006$demovote + s2006$repuvote)
message("sanity range demovote+repuvote: [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))
