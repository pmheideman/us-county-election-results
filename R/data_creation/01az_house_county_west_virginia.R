## West Virginia: found via OpenElections (github.com/openelections/openelections-data-wv). One of
## the last 5 states in the project-wide sweep. Repo actually goes back to 1950 in clusters
## (1950-1960, 1980/1984/1988, then continuously 2000+) -- far deeper than the 1990 project goal,
## but 1962-1998 has no data at all (a real archive gap), so 1990-1998 specifically is unreachable
## here. Production target: 2000-2014 (MEDSL covers 2016+).
##
## Two file shapes: 2000/2002/2004/2006 and 2014 already have a single clean county-level file
## (`*__general__house.csv` for the first four, `*__general__county.csv` filtered to office for
## 2014) -- county already present, no crosswalk needed beyond name matching. 2008/2010/2012 only
## have PER-COUNTY precinct-level files (`*__general__<county>__precinct.csv`), and critically,
## NOT ALL counties have a file in the repo for those 3 years -- genuine partial coverage (48/55 in
## 2008, 36/55 in 2010, 26/55 in 2012), confirmed by listing the actual files present, not a
## download failure.
##
## Office label varies by source shape: "U.S. House" (2000-2006, 2014 county file) vs "U.S. HOUSE"
## (all-caps, 2008/2010/2012 precinct files) -- matched case-insensitively. Distinct from "State
## House"/"MEMBER OF HOUSE OF DELEGATES" in the same files, so no substring-grep trap here.
##
## Pseudo-total row: every county-level year (2000-2006, 2014) has a `county == "Totals"` row per
## candidate -- verified its value equals the district-wide sum before excluding (e.g. 2000 AL-1
## Mollohan: Totals=170974, matches the sum of that district's real county rows). The 2008/2010/2012
## precinct files have no such row (checked), and also carry unrelated pseudo-OFFICE rows
## ("BALLOTS CAST", "REGISTERED VOTERS", "STRAIGHT PARTY") that are naturally excluded by the
## office=="U.S. HOUSE" filter itself, not a separate concern.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "west_virginia")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

wv_fips <- county_fips_crosswalk %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)
stopifnot(nrow(wv_fips) == 55)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-wv/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(x %in% c("D", "DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM",
            x %in% c("R", "REP", "REPUBLICAN") ~ "REP",
            TRUE ~ "OTHER")
}

normalize_county <- function(x) toupper(trimws(x))

## A handful of 2008/2010/2012 per-county precinct files have a party column that is entirely
## blank for the U.S. House race (confirmed: every row for that county/office/year has NA party,
## not just some -- a different failure mode than a genuinely uncoded write-in). Found by scanning
## every downloaded file for an all-blank party column on the House subset; hand-mapped from known
## real-world party affiliations, cross-checked against each race's known outcome (e.g. Mollohan/
## Rahall/Capito/McKinley's party already confirmed directly from the clean 2000/2002/2004/2006
## county-level files in this same script).
CANDIDATE_PARTY_OVERRIDE <- tribble(
  ~candidate_norm,                     ~party,
  "SHELLY MOOORE CAPITO",              "REP",   # 2008 Clay (source typo: "MOOORE")
  "SHELLY MOORE CAPITO",               "REP",   # 2010 Morgan
  "ANNE BARTH",                        "DEM",   # 2008 WV-2 challenger
  "ALAN B. MOLLOHAN",                  "DEM",   # WV-1 longtime incumbent
  "ELLIOTT E \"SPIKE\" MAYNARD",       "REP",   # 2010 WV-3 challenger
  "ELLIOTT E. \"SPIKE\" MAYNARD",      "REP",
  "NICK JOE RAHALL II",                "DEM",   # WV-3 longtime incumbent
  "NICK JOE RAHALL, II",               "DEM",
  "WRITE IN",                          "OTHER",
  "DAVID B. MCKINLEY",                 "REP",   # WV-1, elected 2010
  "DAVID B MCKINLEY",                  "REP",
  "MIKE OLIVERIO",                     "DEM",   # 2010 WV-1 challenger
  "VIRGINIA LYNCH GRAF",               "DEM",   # 2010 WV-2 challenger
  "PHIL HUDOK",                        "OTHER", # Constitution Party (confirmed via 2014 Senate race, "CST,PHIL HUDOK")
  "BUTCH PAUGH",                       "OTHER", # 2010 WV-3 independent
  "SUE THORN",                         "DEM"    # 2012 WV-1 challenger
)

apply_party_override <- function(df) {
  cand_norm <- toupper(trimws(gsub('"', '"', df$candidate)))
  hit <- match(cand_norm, CANDIDATE_PARTY_OVERRIDE$candidate_norm)
  ifelse(is.na(df$party_std) & !is.na(hit), CANDIDATE_PARTY_OVERRIDE$party[hit], df$party_std)
}

## ---- 2000/2002/2004/2006: clean county-level house.csv ----
read_wv_county_house <- function(year, remote, local) {
  path <- download_oe(remote, local)
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House", normalize_county(county) != "TOTALS") %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

wv_2000 <- read_wv_county_house(2000, "2000/20001107__wv__general__house.csv", "2000_general_house.csv")
wv_2002 <- read_wv_county_house(2002, "2002/20021105__wv__general__house.csv", "2002_general_house.csv")
wv_2004 <- read_wv_county_house(2004, "2004/20041102__wv__general__house.csv", "2004_general_house.csv")
wv_2006 <- read_wv_county_house(2006, "2006/20061107__wv__general__house.csv", "2006_general_house.csv")

## ---- 2014: clean county-level file, filter by office ----
wv_2014 <- {
  path <- download_oe("2014/20141104__wv__general__county.csv", "2014_general_county.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House") %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes)) %>%
    filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = 2014)
}

## ---- 2008/2010/2012: per-county precinct files, genuine partial coverage ----
list_precinct_files <- function(year) {
  resp <- system2("curl", c("-sL", shQuote(paste0("https://api.github.com/repos/openelections/openelections-data-wv/contents/", year))), stdout = TRUE)
  js <- jsonlite::fromJSON(paste(resp, collapse = "\n"))
  nm <- js$name
  nm[grepl(paste0("^", year, ".*general.*precinct\\.csv$"), nm) & !grepl("special", nm)]
}

read_wv_precinct_year <- function(year) {
  files <- list_precinct_files(year)
  message(year, ": ", length(files), " county precinct files found")
  all_rows <- purrr::map_dfr(files, function(f) {
    dest <- download_oe(paste0(year, "/", f), paste0(year, "_", f))
    df <- read_csv(dest, show_col_types = FALSE, col_types = cols(.default = "c"))
    h <- df %>% filter(toupper(trimws(office)) == "U.S. HOUSE")
    if (nrow(h) == 0) return(tibble(county = character(), party = character(), votes = double()))
    ## Mercer County's 2010 file has `candidate` and `party` swapped for every House row (confirmed
    ## in the raw CSV: candidate is literally blank, and the real candidate name -- e.g. `Elliott E.
    ## "Spike" Maynard` -- sits in the `party` column instead; other offices in the same file are
    ## fine). Detect and un-swap: candidate blank + party non-blank and not a short party code.
    swapped <- is.na(h$candidate) & !is.na(h$party) & nchar(trimws(h$party)) > 4 &
      !toupper(trimws(h$party)) %in% c("DEM", "REP", "DEMOCRAT", "DEMOCRATIC", "REPUBLICAN", "LIBERTARIAN", "CONSTITUTION", "MOUNTAIN")
    if (any(swapped)) {
      tmp <- h$candidate[swapped]
      h$candidate[swapped] <- h$party[swapped]
      h$party[swapped] <- tmp
    }
    blank_party <- is.na(h$party) | trimws(h$party) == ""
    party_std <- to_party(h$party)
    party_std[blank_party] <- NA
    party_std <- ifelse(is.na(party_std) & blank_party,
                         apply_party_override(data.frame(candidate = h$candidate, party_std = party_std)),
                         party_std)
    party_std[is.na(party_std)] <- "OTHER"
    h %>% transmute(county = normalize_county(county), party = party_std, votes = as.numeric(votes))
  })
  all_rows %>% filter(!is.na(votes)) %>%
    group_by(county, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

library(jsonlite)
wv_2008 <- read_wv_precinct_year(2008)
wv_2010 <- read_wv_precinct_year(2010)
wv_2012 <- read_wv_precinct_year(2012)

wv_by_county <- bind_rows(wv_2000, wv_2002, wv_2004, wv_2006, wv_2008, wv_2010, wv_2012, wv_2014)

elect_he_cty_wv <- wv_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "WEST VIRGINIA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_wv")

message("WV House county-level rows built: ", nrow(elect_he_cty_wv), " (of possible ", 55 * 8, ")")
print(table(elect_he_cty_wv$year))

sanity <- elect_he_cty_wv$repuvote + elect_he_cty_wv$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_wv %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_wv$year))) {
  present <- elect_he_cty_wv %>% filter(year == yr) %>% pull(cty_fips)
  missing <- wv_fips %>% filter(!county_fips %in% present)
  message(yr, ": ", length(present), "/55 counties", if (nrow(missing) > 0) paste0(" -- missing: ", paste(missing$county_name, collapse = ", ")) else "")
}

## ---- Non-production 2016 cross-check against MEDSL ----
wv_2016_raw <- {
  path <- download_oe("2016/20161108__wv__general__county.csv", "2016_general_county.csv")
  df <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  df %>% filter(trimws(office) == "U.S. House") %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes))
}
wv_2016 <- wv_2016_raw %>%
  filter(!is.na(votes)) %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_wv_2016 <- medsl_final %>% filter(sample == "HE", year == 2016, cty_fips %in% wv_fips$county_fips)

if (nrow(medsl_wv_2016) > 0) {
  cmp <- wv_2016 %>%
    inner_join(medsl_wv_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL WV 2016 HE rows found to cross-check against.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- fold-in is done centrally after all remaining states are built, to avoid a
## lost-update race on those shared files.
