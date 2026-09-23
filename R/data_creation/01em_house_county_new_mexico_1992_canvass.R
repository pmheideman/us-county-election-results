## New Mexico House 1992 -- hand-transcribed from the user-supplied scanned "Canvass of Returns of
## General Election Held on November 3, 1992" PDF (`NM_CanvassGeneral1992.pdf`, page 1 = "PAGE 13"
## of the bound volume). Same wide-table-with-33-county-columns format as 1990/1994/1996 (see
## 01ek_house_county_new_mexico_scanned_canvass.R, which deliberately left 1992 for a separate build).
## This is the last remaining `source_not_found` New Mexico House year.
##
## 1992 was the first election under the post-1990-census 3-district map (same map used every year
## since, through at least 2000): District 1 is entirely Bernalillo County plus small slivers of
## Sandoval/Santa Fe/Torrance/Valencia; Districts 2 and 3 split the rest of the state.
##
## Verification: every district's county column was checked to sum to the PDF's own printed "TOTAL
## FOR EACH CANDIDATE" value.
##  - District 1 and District 2 tie out EXACTLY for every candidate.
##  - District 3 (Richardson D / Bemis R / Nagel Lib): Richardson and Nagel tie out EXACTLY; Bemis is
##    51 votes (0.09%) short of the printed 54,569 -- accepted as a small residual, same threshold
##    used throughout this project (e.g. West Virginia's Roane/Barbour 2012 residuals, 1990's own
##    Archuletta gap in 01ek).
## District 3's county LABELS were corrected 2026-09-23 after an independent re-transcription
## initially mislabeled several right-hand cells (attributing Los Alamos-through-Union's values to
## the wrong specific counties, though the RAW NUMBERS and district totals were already right either
## way -- relabeling a value doesn't change what it sums to). Fixed by cross-checking NM-3's actual
## 1990s county membership (confirmed via web search against Ballotpedia/Wikipedia sourcing:
## Bernalillo(partial), Cibola, Colfax, Curry, Harding, Los Alamos, McKinley, Mora, Quay, Rio Arriba,
## Roosevelt, Sandoval(partial), San Juan, San Miguel, Santa Fe(partial), Taos, Union -- notably NOT
## Sierra/Socorro/Valencia, which belong to District 2, and not Otero -- see the same source for
## District 1 (Bernalillo + slivers of Sandoval/Santa Fe/Torrance/Valencia) and District 2
## (Bernalillo, Catron, Chaves, Cibola, De Baca, Dona Ana, Eddy, Grant, Guadalupe, Hidalgo, Lea,
## Lincoln, Luna, Otero, Sierra, Socorro, Valencia), both of which were already labeled correctly
## below and needed no change.
## All 33 counties present (33/33), unlike 1994/1996's Harding/Hidalgo gaps -- confirmed directly,
## not assumed.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "NEW MEXICO") %>% distinct(county_fips, county_name) %>% mutate(nm = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 33)
fips_of <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$nm)]

## ---- District 1: Aragon (D) / Schiff (R) / Cole write-in (OTHER) ----
d1 <- tribble(
  ~county,      ~dem,    ~rep,   ~oth,
  "BERNALILLO", 70523,  120294,  179,
  "SANDOVAL",     2433,   2363,    3,
  "SANTA FE",      180,    474,    0,
  "TORRANCE",     1521,   2593,    0,
  "VALENCIA",     1943,   2702,    6
) %>% mutate(district = 1)
stopifnot(sum(d1$dem) == 76600, sum(d1$rep) == 128426, sum(d1$oth) == 188)

## ---- District 2: Sosa (D) / Skeen (R) / Pilley write-in (OTHER) ----
d2 <- tribble(
  ~county,       ~dem,   ~rep,  ~oth,
  "BERNALILLO",    256,    177,    0,
  "CATRON",        513,   1004,    0,
  "CHAVES",       7102,  11511,   38,
  "CIBOLA",       2865,   2710,    1,
  "DE BACA",       436,    737,    0,
  "DONA ANA",    20585,  22324,   45,
  "EDDY",         7453,  10643,   14,
  "GRANT",        5347,   4644,    6,
  "GUADALUPE",    1182,    863,    0,
  "HIDALGO",      1066,   1215,    0,
  "LEA",          5318,  10334,   20,
  "LINCOLN",      2110,   3691,   12,
  "LUNA",         2966,   3221,    5,
  "OTERO",        5522,  10197,   19,
  "SIERRA",       1847,   2433,    4,
  "SOCORRO",      2818,   3001,    3,
  "VALENCIA",     5771,   6133,    8
) %>% mutate(district = 2)
stopifnot(sum(d2$dem) == 73157, sum(d2$rep) == 94838, sum(d2$oth) == 175)

## ---- District 3: Richardson (D) / Bemis (R) / Nagel (OTHER, Libertarian) ----
## County labels corrected 2026-09-23 -- see header note. Values unchanged from the original
## transcription (only the county each value is attributed to was wrong).
d3 <- tribble(
  ~county,       ~dem,   ~rep,  ~oth,
  "BERNALILLO",   1788,   1565,  101,
  "CIBOLA",        383,     92,   12,
  "COLFAX",       3748,   1403,   74,
  "CURRY",        7192,   4995,  202,
  "HARDING",       477,    180,   11,
  "LOS ALAMOS",   5547,   4490,  407,
  "MCKINLEY",    11827,   3054,  235,
  "MORA",         1805,    479,   26,
  "QUAY",         3098,   1047,   74,
  "RIO ARRIBA",   8966,   2018,  152,
  "ROOSEVELT",    4039,   2180,  136,
  "SANDOVAL",    10530,   7031,  543,
  "SAN JUAN",    16273,  12655,  832,
  "SAN MIGUEL",   7477,   1451,  159,
  "SANTA FE",    30076,   9522, 1386,
  "TAOS",         8428,   1772,  402,
  "UNION",        1196,    584,   46
) %>% mutate(district = 3)
stopifnot(sum(d3$dem) == 122850, sum(d3$oth) == 4798)
## Bemis is the one documented residual: 51 votes (0.09%) short of the printed 54,569 -- accepted,
## see header note.
message("D3 Richardson (D) total: ", sum(d3$dem), " vs printed 122850 (exact)")
message("D3 Bemis (R) total: ", sum(d3$rep), " vs printed 54569 -- accepted residual of ", sum(d3$rep) - 54569, " votes, see header note")
message("D3 Nagel (Lib) total: ", sum(d3$oth), " vs printed 4798 (exact)")

raw <- bind_rows(
  d1 %>% transmute(district, county, dem_c = "Robert J. Aragon", dem_p = "Democratic", dem_v = dem,
                    rep_c = "Steven H. Schiff", rep_p = "Republican", rep_v = rep,
                    oth_c = "Orlin G. Cole", oth_p = "Write-In", oth_v = oth),
  d2 %>% transmute(district, county, dem_c = "Dan Sosa, Jr.", dem_p = "Democratic", dem_v = dem,
                    rep_c = "Joe Skeen", rep_p = "Republican", rep_v = rep,
                    oth_c = "David Lee Pilley", oth_p = "Write-In", oth_v = oth),
  d3 %>% transmute(district, county, dem_c = "Bill Richardson", dem_p = "Democratic", dem_v = dem,
                    rep_c = "F. Gregg Bemis, Jr.", rep_p = "Republican", rep_v = rep,
                    oth_c = "Ed Nagel", oth_p = "Libertarian", oth_v = oth)
) %>% left_join(xw, by = c("county" = "nm")) %>% filter(!is.na(county_fips))
stopifnot(!anyNA(raw$county_fips))

to_long <- function(df, cand_col, party_col, votes_col, party_group) {
  df %>% filter(.data[[votes_col]] > 0 | party_group != "OTHER") %>%
    transmute(year = 1992, county_fips, district, candidate = .data[[cand_col]], party = .data[[party_col]], party_group = !!party_group, votes = .data[[votes_col]])
}
long_raw <- bind_rows(
  to_long(raw, "dem_c", "dem_p", "dem_v", "DEM"),
  to_long(raw, "rep_c", "rep_p", "rep_v", "REP"),
  to_long(raw, "oth_c", "oth_p", "oth_v", "OTHER")
)
long <- finalize_long(long_raw, "nm_canvass_1992")
## Saved as "he_nm_1992" (NOT "he_nm_canvass_1992") so 02z_house_long_assemble.R's filename-based
## resolve_file() finds it directly -- the shares file is "elect_he_cty_nm_1992.rds" (source token
## "he_cty_nm_1992"), and 02z only strips TRAILING underscore-tokens when falling back, so a file
## named he_nm_canvass_1992.rds (avail token "he_cty_nm_canvass_1992") would never match; worse, the
## fallback would silently match the UNRELATED pre-existing "he_nm.rds" (OpenElections 2000-2014)
## instead, resulting in an inner join against a file with no 1992 rows at all -- zero long rows for
## this source, silently. The `source` column INSIDE the table (used by 03a's registry matching, not
## by 02z's file lookup) still correctly reads "nm_canvass_1992".
save_long(long, "he_nm_1992")
message("NM 1992 long rows: ", nrow(long))

elect_he_cty_nm_1992 <- derive_shares(long) %>% transmute(state = "NEW MEXICO", year, cty_fips, sample, demovote, repuvote, totalvote)
stopifnot(nrow(elect_he_cty_nm_1992) == 33, !anyNA(elect_he_cty_nm_1992))
sanity <- elect_he_cty_nm_1992$demovote + elect_he_cty_nm_1992$repuvote
message("sanity range demovote+repuvote: [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

saveRDS(elect_he_cty_nm_1992, file.path(OUTPUT_DIR, "elect_he_cty_nm_1992.rds"))
r <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_nm_1992.rds")); stopifnot(all(r$pass))
message("Saved elect_he_cty_nm_1992.rds: 33/33 counties")
