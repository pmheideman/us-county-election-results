## SUPERSEDED FOR 1994 (2026-09-24): this 1994 block shifted every county column after Grant by one (it took Harding and Hidalgo as blank; the
## state canvassing board canvass on SRI microfiche micro_IA40706953_0112 shows all 33 counties), so 12 of 31 county rows were wrong. 1994 now comes
## from 01il_mississippi_new_mexico_1994_transcribe.py / 01io (label srinm_1994). SUPERSEDED FOR 1996 too: Harding's District 3 cells were filed
## under Guadalupe, Santa Fe's District 1 cells under San Miguel, Libertarian Nagel (D3) and D1's Green/Unaffiliated outside Bernalillo were left out;
## re-transcribed by 01ir_new_mexico_1996_retranscribe.py (label srinm_1996). This script now writes 1990 only.
## New Mexico House 1990, 1994, 1996 -- hand-transcribed from the user-supplied scanned "Canvass of
## Returns of General Election" PDFs (`NM_CanvassGeneral1990.pdf`, `NM_CanvassGeneral1994.pdf`,
## `NM_1996 General Summary.pdf`). Unlike most other states' scanned canvasses in this project, the
## ENTIRE election (every office, all 33 counties) fits on a single page each year -- a very wide
## table, counties as columns left-to-right in a fixed order (confirmed identical to the born-digital
## 2004/2006 statewide PDFs -- see 01eh/01ei): Bernalillo, Catron, Chaves, Cibola, Colfax, Curry,
## De Baca, Dona Ana, Eddy, Grant, Guadalupe, Harding, Hidalgo, Lea, Lincoln, Los Alamos, Luna,
## McKinley, Mora, Otero, Quay, Rio Arriba, Roosevelt, Sandoval, San Juan, San Miguel, Santa Fe,
## Sierra, Socorro, Taos, Torrance, Union, Valencia, then "TOTAL FOR EACH CANDIDATE".
##
## 1992 is a separate, still-open item (not included here) -- see the project's own notes for status.
## 1998/2002 come from the SOS archive (01ee); 2004/2006 from the born-digital statewide PDFs (01ei).
## This closes 1990/1994/1996, the last remaining `source_not_found` New Mexico House years other
## than 1992.
##
## Verification: every district's county column was checked to sum to the PDF's own printed
## "TOTAL FOR EACH CANDIDATE" value before being accepted. Two small, documented residuals were
## accepted rather than chased further (same threshold used throughout this project, e.g. West
## Virginia's Roane/Barbour 2012 residuals, ~0.01-0.2% of the total):
##  - 1990 District 3, Phil T. Archuletta (R): county cells sum to 60 votes (0.17%) under the printed
##    total (35,751); every other candidate in every other district ties exactly.
##  - 1994 District 2, Benjamin Anthony Chavez (D): county cells sum to 6 votes (0.01%) under the
##    printed total (45,316); Skeen (R) and Johnson (Green) in the same district tie exactly.
##
## 1994 is missing Harding and Hidalgo counties ENTIRELY (31/33, not 33/33) -- confirmed genuine,
## not a misread: every candidate's row across all 3 districts (checked column-by-column at the
## page's Harding/Hidalgo positions) is blank there, while every OTHER county's cell is a real
## printed number. Both counties DO appear (nonzero) in 1990's canvass, so this is a one-year
## printing/reporting gap in the source document itself, same class of gap already documented
## elsewhere in this project (e.g. 2002 OpenElections missing Cibola entirely, 01ao's own note).
## 1996 is missing Harding alone (32/33) -- not individually re-verified column-by-column the way
## 1994's gap was, but consistent with the same source-side pattern; Hidalgo IS present in 1996
## (in District 2, unlike 1994).
## A third caveat, narrower in scope: 1996 District 3's county-level attribution for Richardson (D)
## and Redmond (R) has more POSITIONAL uncertainty than other rows -- the printed totals for both
## candidates tie out exactly (124,594 and 56,580), confirming every digit was read correctly, but a
## few of the ~12 nonzero counties in that row may be attributed to the wrong specific county (the
## district total and the two-party shares are unaffected either way). Flagged, not fixed.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)

xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
nm_fips <- xw %>% filter(state == "NEW MEXICO") %>% select(county_name, county_fips) %>% distinct()
stopifnot(nrow(nm_fips) == 33)

## ---- 1990 (NM_CanvassGeneral1990.pdf, p.175 of the bound volume / p.1 of the PDF) ----
## 4 candidates per district that year: Republican, Democrat (+ a Green in D3).
d1_1990 <- tribble(~county, ~dem, ~rep,
  "BERNALILLO", 38399, 93510, "DE BACA", 323, 786, "GUADALUPE", 1233, 857, "TORRANCE", 1351, 2222) %>% mutate(district = 1)
d1_1990_tot <- c(dem = 41306, rep = 97375)
d2_1990 <- tribble(~county, ~rep,
  "CHAVES", 10423, "CURRY", 6803, "DONA ANA", 17702, "EDDY", 7237, "GRANT", 4874, "HIDALGO", 1049,
  "LEA", 9316, "LINCOLN", 3417, "LUNA", 2956, "OTERO", 7882, "QUAY", 1883, "ROOSEVELT", 3464,
  "SIERRA", 2581, "UNION", 1090) %>% mutate(district = 2)   ## Joe Skeen (R) ran unopposed
d2_1990_tot <- c(rep = 80677)
d3_1990 <- tribble(~county, ~rep, ~dem,
  "CATRON", 479, 778, "CIBOLA", 1148, 4216, "COLFAX", 828, 3092, "HARDING", 171, 409,
  "LOS ALAMOS", 2858, 4920, "MCKINLEY", 2012, 9386, "MORA", 633, 1766, "RIO ARRIBA", 2277, 7669,
  "SANDOVAL", 4472, 10598, "SAN JUAN", 6498, 14193, "SAN MIGUEL", 1236, 6088, "SANTA FE", 6587, 22488,
  "SOCORRO", 1339, 3769, "TAOS", 1869, 5491, "VALENCIA", 3284, 9362) %>% mutate(district = 3)
d3_1990_tot <- c(rep = 35751, dem = 104225)   ## rep sum is 35,691 (60 short, 0.17%) -- accepted residual, see header

## ---- 1994 (NM_CanvassGeneral1994.pdf, p.1) ----
d1_1994 <- tribble(~county, ~dem, ~rep,
  "BERNALILLO", 38773, 109979, "SANDOVAL", 1440, 3169, "SAN MIGUEL", 90, 475, "TORRANCE", 896, 3106,
  "VALENCIA", 1117, 3267) %>% mutate(district = 1)
d1_1994_tot <- c(dem = 42316, rep = 119996)
d2_1994 <- tribble(~county, ~dem, ~rep, ~grn,
  "BERNALILLO", 200, 189, 28, "CATRON", 318, 1066, 96, "CHAVES", 4100, 11117, 569, "CIBOLA", 2230, 2733, 239,
  "DE BACA", 313, 694, 38, "DONA ANA", 11040, 19423, 2007, "EDDY", 4795, 10016, 506, "GRANT", 3963, 4734, 565,
  "GUADALUPE", 1200, 900, 68, "LEA", 693, 1142, 70, "LINCOLN", 2686, 9583, 609, "LOS ALAMOS", 1092, 3940, 216,
  "LUNA", 1810, 3261, 307, "OTERO", 3126, 9499, 537, "SIERRA", 1015, 2630, 243, "SOCORRO", 2373, 3018, 307,
  "VALENCIA", 4356, 6021, 493) %>% mutate(district = 2)
d2_1994_tot <- c(dem = 45316, rep = 89966, grn = 6898)  ## dem sum is 45,310 (6 short, 0.01%) -- accepted residual, see header
d3_1994 <- tribble(~county, ~dem, ~rep, ~lib,
  "BERNALILLO", 1260, 1400, 69, "CIBOLA", 321, 115, 12, "COLFAX", 3079, 1537, 72, "CURRY", 4981, 5169, 160,
  "GUADALUPE", 385, 251, 4, "LOS ALAMOS", 4808, 3815, 366, "MCKINLEY", 9860, 3112, 201, "MORA", 1841, 483, 48,
  "OTERO", NA, NA, 51, "QUAY", 2267, 1396, 175, "RIO ARRIBA", 7744, 1813, 84, "ROOSEVELT", 2382, 2345, 346,
  "SANDOVAL", 9234, 6316, 613, "SAN JUAN", 12270, 13560, 171, "SAN MIGUEL", 6661, 1404, 1088, "SANTA FE", 25756, 8528, NA,
  "TAOS", 6184, 1531, 205, "UNION", 867, 740, 32) %>% mutate(district = 3, across(c(dem, rep, lib), ~ tidyr::replace_na(.x, 0)))
d3_1994_tot <- c(dem = 99900, rep = 53515, lib = 3697)

## ---- 1996 (NM_1996 General Summary.pdf, p.1) ----
d1_1996 <- tribble(~county, ~dem, ~rep, ~grn, ~una,
  "BERNALILLO", 65190, 99577, 7060, 4056, "SANDOVAL", 2420, 3000, NA, NA, "SAN MIGUEL", 216, 723, NA, NA,
  "TORRANCE", 1609, 2773, NA, NA, "VALENCIA", 2200, 3217, NA, NA) %>%
  mutate(district = 1, across(c(dem, rep, grn, una), ~ tidyr::replace_na(.x, 0)))
d1_1996_tot <- c(dem = 71635, rep = 109290, grn = 7060, una = 4056)   ## grn/una are Bernalillo-only, no other-county cells printed
d2_1996 <- tribble(~county, ~dem, ~rep,
  "BERNALILLO", 307, 206, "CATRON", 460, 1037, "CHAVES", 6310, 12044, "CIBOLA", 3072, 2954,
  "DE BACA", 369, 715, "DONA ANA", 21877, 21154, "EDDY", 7393, 11419, "GRANT", 5903, 4837,
  "GUADALUPE", 1057, 661, "HIDALGO", 853, 1087, "LEA", 4901, 9434, "LINCOLN", 2243, 4113,
  "LUNA", 3049, 3188, "OTERO", 5778, 10415, "SIERRA", 2290, 2535, "SOCORRO", 2999, 3146,
  "VALENCIA", 6054, 6146) %>% mutate(district = 2)
d2_1996_tot <- c(dem = 74915, rep = 95091)
## District 3's per-county attribution has more positional uncertainty than other rows -- see header
## note. Every value below was read directly off the page; only the (county) LABEL for a handful of
## the larger right-hand-side cells is a best-effort match against the shared column layout, not a
## re-verified one-by-one read. Both candidates' totals tie out to the printed value exactly.
d3_1996 <- tribble(~county, ~dem, ~rep,
  "BERNALILLO", 2051, 1877, "CIBOLA", 472, 179, "COLFAX", 3566, 1437, "CURRY", 6598, 5574,
  "GUADALUPE", 376, 231, "LOS ALAMOS", 5559, 4004, "MCKINLEY", 11654, 3398, "MORA", 1911, 404,
  "QUAY", 2789, 1318, "RIO ARRIBA", 8721, 2232, "ROOSEVELT", 3328, 2405, "SANDOVAL", 12152, 7495,
  "SAN JUAN", 16962, 14511, "SAN MIGUEL", 8009, 1267, "SANTA FE", 31499, 8091, "TAOS", 8019, 1481,
  "UNION", 928, 676) %>% mutate(district = 3)
d3_1996_tot <- c(dem = 124594, rep = 56580)

build_year <- function(year, districts, totals, tol = 0.002) {
  parts <- list()
  for (i in seq_along(districts)) {
    d <- districts[[i]]; tot <- totals[[i]]
    vote_cols <- setdiff(names(d), c("county", "district"))
    for (nm in names(tot)) {
      s <- sum(d[[nm]], na.rm = TRUE)
      resid <- abs(s - tot[nm]) / tot[nm]
      stopifnot(resid < tol)
      message(year, " district ", unique(d$district), " ", nm, ": sum=", s, " printed=", tot[nm],
              if (s != tot[nm]) paste0(" (residual ", round(resid * 100, 3), "%, accepted)") else " (exact)")
    }
    long_d <- purrr::map_dfr(vote_cols, function(pc) {
      party <- switch(pc, dem = "DEM", rep = "REP", grn = "GREEN", lib = "LIBERTARIAN", una = "UNAFFILIATED", "OTHER")
      pg <- switch(pc, dem = "DEM", rep = "REP", "OTHER")
      d %>% transmute(year = !!year, county, district = !!unique(d$district), candidate = paste0("CAND_", toupper(pc)),
                       party = !!party, party_group = !!pg, votes = .data[[pc]])
    })
    parts[[i]] <- long_d
  }
  bind_rows(parts)
}

raw_1990 <- build_year(1990, list(d1_1990, d2_1990, d3_1990), list(d1_1990_tot, d2_1990_tot, d3_1990_tot))
raw_1994 <- build_year(1994, list(d1_1994, d2_1994, d3_1994), list(d1_1994_tot, d2_1994_tot, d3_1994_tot))
raw_1996 <- build_year(1996, list(d1_1996, d2_1996, d3_1996), list(d1_1996_tot, d2_1996_tot, d3_1996_tot))

all_raw <- bind_rows(raw_1990, raw_1994, raw_1996) %>%
  left_join(nm_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), votes > 0)

for (yr in c(1990)) {   ## 1994 and 1996 superseded 2026-09-24, see header
  long_yr <- finalize_long(all_raw %>% filter(year == yr) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("nm_canvass_", yr))
  ## Saved as "he_nm_<year>" (not "he_nm_canvass_<year>") -- see 01em's header note for why this
  ## exact naming is required for 02z_house_long_assemble.R's file resolution to find it.
  save_long(long_yr, paste0("he_nm_", yr))
  shares <- derive_shares(long_yr) %>% transmute(state = "NEW MEXICO", year, cty_fips, sample, demovote, repuvote, totalvote)
  expected_n <- if (yr == 1994) 31 else if (yr == 1996) 32 else 33   ## 1994: Harding+Hidalgo missing; 1996: Harding missing (see header note)
  stopifnot(nrow(shares) == expected_n, !anyNA(shares$demovote), !anyNA(shares$repuvote))
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", yr)))
  r <- check_long_vs_source(long_yr, file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", yr))); stopifnot(all(r$pass))
  message(yr, ": ", nrow(shares), "/33 counties")
}

sanity <- purrr::map_dfr(c(1990), ~ readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_nm_%d.rds", .x))))
s <- sanity$demovote + sanity$repuvote
message("sanity range demovote+repuvote: [", round(min(s), 3), ", ", round(max(s), 3), "]")
stopifnot(all(s >= 0 & s <= 1.001))
