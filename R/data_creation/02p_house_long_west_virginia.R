## Candidate-level LONG table for West Virginia (he_cty_wv; OpenElections 2000-2006 county-by-candidate files, 2008-2012 per-county precinct
## files, 2014 county file; logic of 01az incl. the swapped candidate/party fix and the candidate->party override for blank party fields;
## "Totals" pseudo-county dropped; cached files only -- the county file list comes from the local folder instead of the GitHub API).
## Acceptance: derived shares == elect_he_cty_wv.rds.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "west_virginia")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
wv_fips <- xw %>% filter(state == "WEST VIRGINIA") %>% select(county_name, county_fips)
to_party <- function(x) { x <- toupper(trimws(x)); case_when(x %in% c("D", "DEM", "DEMOCRAT", "DEMOCRATIC") ~ "DEM", x %in% c("R", "REP", "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }
normalize_county <- function(x) toupper(trimws(x))
## same override table as 01az (copied from the script text so there is one copy)
src <- readLines(file.path(PROJECT_ROOT, "R", "data_creation", "01az_house_county_west_virginia.R"))
i0 <- grep("^CANDIDATE_PARTY_OVERRIDE <- tribble", src); i1 <- grep("^read_wv_county_house <-", src) - 1
eval(parse(text = src[i0:i1]))

county_year <- function(f, year_arg) read_csv(file.path(RAW_DIR, f), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(trimws(office) == "U.S. House", normalize_county(county) != "TOTALS") %>%
  transmute(year = year_arg, county = normalize_county(county), district, candidate = trimws(candidate), party_raw = trimws(party), party_group = to_party(party), votes = as.numeric(votes))
precinct_year <- function(year) {
  files <- list.files(RAW_DIR, pattern = paste0("^", year, "_", year, ".*general.*precinct\\.csv$"))
  files <- files[!grepl("special", files)]
  message(year, ": ", length(files), " cached county precinct files")
  purrr::map_dfr(files, function(f) {
    df <- read_csv(file.path(RAW_DIR, f), show_col_types = FALSE, col_types = cols(.default = "c"))
    h <- df %>% filter(toupper(trimws(office)) == "U.S. HOUSE"); if (nrow(h) == 0) return(NULL)
    swapped <- is.na(h$candidate) & !is.na(h$party) & nchar(trimws(h$party)) > 4 &
      !toupper(trimws(h$party)) %in% c("DEM", "REP", "DEMOCRAT", "DEMOCRATIC", "REPUBLICAN", "LIBERTARIAN", "CONSTITUTION", "MOUNTAIN")
    if (any(swapped)) { tmp <- h$candidate[swapped]; h$candidate[swapped] <- h$party[swapped]; h$party[swapped] <- tmp }
    blank_party <- is.na(h$party) | trimws(h$party) == ""
    party_std <- to_party(h$party); party_std[blank_party] <- NA
    party_std <- ifelse(is.na(party_std) & blank_party, apply_party_override(data.frame(candidate = h$candidate, party_std = party_std)), party_std)
    party_std[is.na(party_std)] <- "OTHER"
    h %>% transmute(year = .env$year, county = normalize_county(county), district, candidate = trimws(candidate), party_raw = trimws(party), party_group = party_std, votes = as.numeric(votes))
  })
}
raw <- bind_rows(county_year("2000_general_house.csv", 2000), county_year("2002_general_house.csv", 2002), county_year("2004_general_house.csv", 2004),
                 county_year("2006_general_house.csv", 2006), precinct_year(2008), precinct_year(2010), precinct_year(2012), county_year("2014_general_county.csv", 2014)) %>%
  filter(!is.na(votes)) %>% group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(wv_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long <- finalize_long(raw, "wv"); save_long(long, "he_wv")
message("WV long rows: ", nrow(long)); check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_wv.rds"))
