## Tennessee: found via OpenElections (github.com/openelections/openelections-data-tn). Part of
## the post-large-states push through the remaining state list. Repo starts at 2000 (no 1990-1998
## coverage -- a genuine archive limit, matches most states' repos).
##
## Cleanest state in a while: all 8 usable years (2000/02/04/06/08/10/12/14) already
## county-level in a single general-election file, identical schema
## (`county,office,district,party,candidate,votes`), office consistently labeled exactly
## "U.S. House" (distinct from "State House", which also contains the substring "House" -- exact
## match used, not a substring grep). Party is clean full words (Democratic/Republican) every
## year, no code/abbreviation drift. Full 95/95 counties every single year, zero exclusions, no
## pseudo-total row found (checked the full candidate list per year -- every "Write-In..."-style
## row names a real candidate, no generic "Total"/"Totals"/"Scattering"/"Over Votes"/"Under Votes"
## rows for the U.S. House office specifically). All 95 county names matched the project's
## crosswalk exactly, zero aliases needed.
##
## One thing to watch for if this repo is ever revisited: some year directories (2006, 2010) also
## contain a `*special__general__county.csv` file (special elections, State House only in both
## cases) alongside the real general-election file -- naive globbing for "general" without
## excluding "special" would pick up the wrong file. Filtered explicitly by full filename pattern
## below, not by glob order.
##
## No MEDSL overlap year built (2016+ already fully covered by MEDSL); verified via internal
## consistency instead -- sanity range in [0,1], full county-count-per-year check.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "tennessee")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

tn_fips <- county_fips_crosswalk %>% filter(state == "TENNESSEE") %>% select(county_name, county_fips)
stopifnot(nrow(tn_fips) == 95)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-tn/master/", remote_path)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

to_party <- function(x) {
  x <- toupper(trimws(x))
  case_when(
    grepl("^DEM", x) ~ "DEM",
    grepl("^REP", x) ~ "REP",
    TRUE ~ "OTHER"
  )
}

## The literal general-election-file names, one regular general file per year (special-election
## files in 2005-2007/2010/2011/2013 excluded by not being referenced here at all).
TN_GENERAL_FILES <- c(
  "2000" = "2000/20001107__tn__general__county.csv",
  "2002" = "2002/20021105__tn__general__county.csv",
  "2004" = "2004/20041102__tn__general__county.csv",
  "2006" = "2006/20061107__tn__general__county.csv",
  "2008" = "2008/20081104__tn__general__county.csv",
  "2010" = "2010/20101102__tn__general__county.csv",
  "2012" = "2012/20121106__tn__general__county.csv",
  "2014" = "2014/20141104__tn__general__county.csv"
)

read_tn_year <- function(year) {
  remote <- TN_GENERAL_FILES[[as.character(year)]]
  path <- download_oe(remote, paste0(year, "_general_county.csv"))
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(toupper(trimws(office)) == "U.S. HOUSE") %>%
    transmute(
      county = toupper(trimws(county)), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = year
    )
}

message("Fetching Tennessee House data, 8 years...")
all_rows <- bind_rows(lapply(c(2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014), read_tn_year)) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_tn <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(tn_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "TENNESSEE", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_tn")

message("TN House county-level rows built: ", nrow(elect_he_cty_tn), " (of possible ", 95 * 8, ")")
print(table(elect_he_cty_tn$year))

sanity <- elect_he_cty_tn$repuvote + elect_he_cty_tn$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_tn %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_tn$year))) {
  present <- elect_he_cty_tn %>% filter(year == yr) %>% pull(cty_fips)
  missing <- tn_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}
