## Candidate-level LONG table for the Iowa House build (shares file elect_he_cty_ia.rds, script 01ad). All per-year parsers, the pseudo-row filter, the 2002
## District-5 blank-party override ("King" -> REP) and the hand-mapped 2010 parties are copied from 01ad UNCHANGED; the district and the raw party label
## are additionally kept. Where the source has no party (2010) the party label stays blank and finalize_long() fills it from the party group.
## Output: R/output/long/he_ia.rds
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "iowa")
ia_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name))) %>% filter(state == "IOWA") %>% select(county_name, county_fips)
stopifnot(nrow(ia_fips) == 99)
rd <- function(local) read_csv(file.path(RAW_DIR, local), show_col_types = FALSE, col_types = cols(.default = "c"))

to_party <- function(x) { x <- toupper(trimws(x)); case_when(grepl("^DEM", x) ~ "DEM", grepl("^REP", x) ~ "REP", TRUE ~ "OTHER") }
is_pseudo_row <- function(candidate) { c <- toupper(trimws(candidate)); grepl("^TOTAL", c) | grepl("^WRITE-IN", c) | grepl("^SCATTERING", c) | grepl("^OVER VOTES", c) | grepl("^UNDER VOTES", c) }

read_2000 <- function() rd("2000_house.csv") %>% filter(!is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(jurisdiction)), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes), year = 2000)

is_us_house_combined <- function(office) { office <- toupper(trimws(office)); grepl("REPRESENTATIVE", office) & !grepl("^STATE", office) }
IA_2002_PARTY_OVERRIDE <- c("King" = "REP")
read_combined <- function(year) {
  raw <- rd(paste0(year, "_general.csv")); county_col <- if ("county" %in% names(raw)) "county" else "jurisdiction"
  raw %>% filter(is_us_house_combined(office), !is_pseudo_row(candidate)) %>%
    transmute(county = toupper(trimws(.data[[county_col]])), district, candidate = trimws(candidate), party_raw = party,
              party_group = if (year == 2002) { case_when(candidate %in% names(IA_2002_PARTY_OVERRIDE) ~ unname(IA_2002_PARTY_OVERRIDE[candidate]), TRUE ~ to_party(party)) } else to_party(party),
              votes = as.numeric(votes), year = year)
}
IA_2010_PARTY <- c("Bruce Braley" = "DEM", "Benjamin M. Lange" = "REP", "Dave Loebsack" = "DEM", "Mariannette Miller-Meeks" = "REP",
                   "Leonard L. Boswell" = "DEM", "Brad Zaun" = "REP", "Tom Latham" = "REP", "Bill Maske" = "DEM", "Steve King" = "REP", "Matthew Campbell" = "DEM")
read_2010 <- function() rd("2010_house.csv") %>% filter(!is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(jurisdiction)), district, candidate = trimws(candidate), party_raw = NA_character_,
            party_group = case_when(candidate %in% names(IA_2010_PARTY) ~ unname(IA_2010_PARTY[candidate]), TRUE ~ "OTHER"), votes = as.numeric(votes), year = 2010)
slugify_county <- function(x) { x <- tolower(x); x <- gsub("'", "", x); gsub(" ", "_", x) }
read_2012_county <- function(county_name) {
  local <- paste0("2012_", slugify_county(county_name), ".csv"); if (!file.exists(file.path(RAW_DIR, local)) || file.size(file.path(RAW_DIR, local)) == 0) return(NULL)
  rd(local) %>% filter(toupper(trimws(office)) == "U.S. HOUSE OF REPRESENTATIVES", !is_pseudo_row(candidate)) %>%
    transmute(county = toupper(county_name), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes), year = 2012)
}
read_2012 <- function() map_dfr(ia_fips$county_name, read_2012_county)
read_2014 <- function() rd("2014_general_precinct.csv") %>% filter(toupper(trimws(office)) == "U.S. HOUSE", !is_pseudo_row(candidate)) %>%
  transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party_raw = party, party_group = to_party(party), votes = as.numeric(votes), year = 2014)

all_rows <- bind_rows(read_2000(), read_combined(2002), read_combined(2004), read_combined(2006), read_combined(2008), read_2010(), read_2012(), read_2014()) %>% filter(!is.na(votes))
raw <- all_rows %>% group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  rename(party = party_raw) %>% left_join(ia_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips))
long <- finalize_long(raw, "ia"); save_long(long, "he_ia")
res <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ia.rds")); print(res)
message("rows ", nrow(long), "; candidates ", nrow(distinct(long, year, district, candidate)), "; blank-party raw rows: ", sum(is.na(raw$party) | raw$party == ""), " (all 2010 = no party column in source)")
