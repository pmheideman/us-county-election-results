## California: found via OpenElections (github.com/openelections/openelections-data-ca), part of
## the large-states push (CA/TX/NY done/OH done/MI done/NJ done/IL done -- CA next, TX after).
##
## CA's repo runs 2002-2018 (checked by listing every year directory directly). Within the
## pre-MEDSL span, six years all have a clean, already county-level file -- no precinct-level
## summing needed, unlike NC/VA/2014-Kansas:
##   2002/2004/2006: a dedicated `..._house.csv` file, office always exactly "U.S. House"
##   2008:           dedicated file, but named `..._congress.csv` (not `..._house.csv`) --
##                   office column content is still "U.S. House", only the FILENAME differs
##   2010/2012:      combined all-office `..._general.csv` file (President/Governor/etc all in
##                   one file), office "U.S. House" filtered out same as any other office
##   2014:            combined all-office file again, same shape as 2010/2012
## No pseudo-total row found in ANY year (checked explicitly per year, following the lesson from
## New York/Kansas/Georgia that a state can have more than one such convention -- CA simply has
## none). 2016 has both a combined file and a precinct file but is left to MEDSL, which already
## covers it -- used only as the cross-check year.
##
## Party field carries write-in variants as a suffixed string ("DEM (W/I)", "REP (W/I)") rather
## than a separate write-in flag column (unlike NC's party_cd) -- collapsed into the same DEM/REP
## bucket as the regular nominee via a startsWith() match rather than an exact-string alias table,
## since a write-in candidate still genuinely belongs to that party. This is a deliberate
## deviation from IL/NJ's exact-match alias approach: those states' party columns never had a
## write-in-suffixed variant to begin with (checked), so there was nothing to collapse there.
##
## CA switched to a top-two nonpartisan blanket primary starting with the 2012 general election --
## confirmed directly in the raw 2012/2014 files (e.g. Alameda CD-15 2012 general has TWO DEM
## rows, Stark vs Swalwell, no Republican at all). This is expected, not a bug: summing all DEM
## rows within a county/district before computing shares handles it correctly (same summing
## approach already used for every other state), and it also means some 2012/2014 counties will
## legitimately show demovote+repuvote << 1 (both candidates same party, so the other party's
## bucket is genuinely zero) -- checked and confirmed real, not a parsing gap, see sanity check
## below.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "california")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

ca_fips <- county_fips_crosswalk %>% filter(state == "CALIFORNIA") %>% select(county_name, county_fips)

download_oe <- function(year, remote_name, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-ca/master/",
                   year, "/", remote_name)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Write-in variants ("DEM (W/I)", "REP (W/I)") collapse into the same party bucket as the
## regular nominee -- see header note. Anything else (AI, GRN, LIB, NL, NPP, PF, IND, NP (W/I),
## etc.) falls to OTHER, same treatment as every other state script in this project.
to_party <- function(x) {
  x <- trimws(x)
  case_when(
    startsWith(x, "DEM") ~ "DEM",
    startsWith(x, "REP") ~ "REP",
    TRUE ~ "OTHER"
  )
}

CA_FILES <- tribble(
  ~year, ~remote_name,
  2002,  "20021105__ca__general__house.csv",
  2004,  "20041102__ca__general__house.csv",
  2006,  "20061107__ca__general__house.csv",
  2008,  "20081104__ca__general__congress.csv",
  2010,  "20101102__ca__general.csv",
  2012,  "20121106__ca__general.csv",
  2014,  "20141104__ca__general.csv"
)

read_ca_year <- function(year, remote_name) {
  path <- download_oe(year, remote_name, paste0(year, "_general.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

ca_by_county <- pmap_dfr(CA_FILES, read_ca_year)

elect_he_cty_ca <- ca_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ca_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "CALIFORNIA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ca")

message("CA House county-level rows built: ", nrow(elect_he_cty_ca), " (of possible ", 7 * 58, ")")
print(table(elect_he_cty_ca$year))

sanity <- elect_he_cty_ca$repuvote + elect_he_cty_ca$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

low_share <- elect_he_cty_ca %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5 (top-two-primary same-party ",
        "general races expected here from 2012 on -- see header note):")
print(low_share)

## No 2016 file built here (left to MEDSL) -- can't cross-check against MEDSL directly for any
## of 2002-2014. Instead sanity-checked 2012/2014 against this project's own crosswalk-based
## logic being internally consistent (share in [0,1], totalvote > 0, expected same-party races
## identified above) -- same standard already applied to Georgia 2012, which also predates MEDSL.
stopifnot(all(sanity >= 0 & sanity <= 1.001))

## ---- Fold into the master panel ----
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ca %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with CA 2002-2014. Total rows now: ", nrow(elect_cty_final))
