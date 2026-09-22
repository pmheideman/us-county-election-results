## Candidate-level LONG tables for Rhode Island (01as), Vermont (01ax) and Delaware (01y) U.S. House, from the same cached OpenElections files and with the same
## party grouping / pseudo-row exclusions / town->county mapping as the originals.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAWB <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
fips_of <- function(st) xw %>% filter(state == st) %>% select(county_name, county_fips)
rd <- function(...) read_csv(file.path(RAWB, ...), show_col_types = FALSE, col_types = cols(.default = "c"))

## ---------------- Rhode Island ----------------
ri_town_county <- tribble(~town, ~county,
  "BARRINGTON", "BRISTOL", "BRISTOL", "BRISTOL", "WARREN", "BRISTOL", "COVENTRY", "KENT", "EAST GREENWICH", "KENT", "WARWICK", "KENT", "WEST GREENWICH", "KENT", "WEST WARWICK", "KENT",
  "JAMESTOWN", "NEWPORT", "LITTLE COMPTON", "NEWPORT", "MIDDLETOWN", "NEWPORT", "NEWPORT", "NEWPORT", "PORTSMOUTH", "NEWPORT", "TIVERTON", "NEWPORT",
  "BURRILLVILLE", "PROVIDENCE", "CENTRAL FALLS", "PROVIDENCE", "CRANSTON", "PROVIDENCE", "CUMBERLAND", "PROVIDENCE", "EAST PROVIDENCE", "PROVIDENCE", "FOSTER", "PROVIDENCE",
  "GLOCESTER", "PROVIDENCE", "JOHNSTON", "PROVIDENCE", "LINCOLN", "PROVIDENCE", "NORTH PROVIDENCE", "PROVIDENCE", "NORTH SMITHFIELD", "PROVIDENCE", "PAWTUCKET", "PROVIDENCE",
  "PROVIDENCE", "PROVIDENCE", "SCITUATE", "PROVIDENCE", "SMITHFIELD", "PROVIDENCE", "WOONSOCKET", "PROVIDENCE",
  "CHARLESTOWN", "WASHINGTON", "EXETER", "WASHINGTON", "HOPKINTON", "WASHINGTON", "NARRAGANSETT", "WASHINGTON", "NEW SHOREHAM", "WASHINGTON", "NORTH KINGSTOWN", "WASHINGTON",
  "RICHMOND", "WASHINGTON", "SOUTH KINGSTOWN", "WASHINGTON", "WESTERLY", "WASHINGTON")
stopifnot(nrow(ri_town_county) == 39)
ri_t2c <- function(x) ri_town_county$county[match(toupper(trimws(x)), ri_town_county$town)]
ri_party <- function(x) { x <- toupper(trimws(x)); case_when(x %in% c("D", "DEM", "DEMOCRATIC") ~ "DEM", x %in% c("R", "REP", "REPUBLICAN") ~ "REP", TRUE ~ "OTHER") }
ri_pseudo <- function(c) toupper(trimws(c)) == "TOTAL"
ri08 <- rd("rhode_island", "2008_town.csv") %>% filter(office == "U.S. Representative", !ri_pseudo(candidate)) %>% mutate(county = toupper(trimws(county)))
ri12 <- rd("rhode_island", "2012_town.csv") %>% filter(grepl("^REPRESENTATIVE IN CONGRESS", office), !ri_pseudo(candidate)) %>% mutate(county = ri_t2c(county))
ri14 <- rd("rhode_island", "2014_town.csv") %>% filter(grepl("^REPRESENTATIVE IN CONGRESS", office), !ri_pseudo(candidate)) %>% mutate(county = ri_t2c(town))
## RI has 2 congressional districts. 2014/2012: district is in the office string ("... DISTRICT 1"); 2008: district column, but a few stray rows carry the wrong
## value, so each candidate takes his/her most common district.
ri12$district <- ifelse(is.na(ri12$district), sub(".*DISTRICT\\s*([0-9]+).*", "\\1", ifelse(grepl("DISTRICT", ri12$office), ri12$office, NA)), ri12$district)
ri14$district <- ifelse(grepl("DISTRICT", ri14$office), sub(".*DISTRICT\\s*([0-9]+).*", "\\1", ri14$office), NA)
modal <- function(x) { x <- x[!is.na(x)]; if (length(x)) names(sort(table(x), decreasing = TRUE))[1] else NA_character_ }
ri08 <- ri08 %>% group_by(candidate) %>% mutate(district = modal(district)) %>% ungroup()
ri <- bind_rows(ri08 %>% mutate(year = 2008), ri12 %>% mutate(year = 2012), ri14 %>% mutate(year = 2014)) %>%
  mutate(party_lbl = party, party_group = ri_party(party), votes = as.numeric(votes)) %>% filter(!is.na(county), !is.na(votes)) %>%
  inner_join(fips_of("RHODE ISLAND"), by = c("county" = "county_name")) %>% transmute(year, county_fips, district, candidate, party = party_lbl, party_group, votes)
long_ri <- finalize_long(ri, "ri"); save_long(long_ri, "he_ri"); res_ri <- check_long_vs_source(long_ri, file.path(OUTPUT_DIR, "elect_he_cty_ri.rds")); print(res_ri)

## ---------------- Vermont ----------------
vt_party <- function(x) { x <- toupper(trimws(x)); case_when(startsWith(x, "DEM") ~ "DEM", startsWith(x, "REP") ~ "REP", TRUE ~ "OTHER") }
vt <- purrr::map_dfr(list(c(2012, "2012_general_precinct.csv"), c(2014, "2014_general_precinct.csv")), function(z) rd("vermont", z[2]) %>%
  filter(trimws(office) == "U.S. House", !toupper(trimws(candidate)) %in% c("TOTAL VOTES CAST", "BLANKS")) %>%
  transmute(year = as.integer(z[1]), county = toupper(trimws(county)), district, candidate, party_lbl = party, party_group = vt_party(party), votes = as.numeric(votes))) %>%
  filter(!is.na(votes)) %>% inner_join(fips_of("VERMONT"), by = c("county" = "county_name")) %>%
  transmute(year, county_fips, district = "00", candidate, party = party_lbl, party_group, votes)   # VT: at-large seat; the source district column is a STATE house code
long_vt <- finalize_long(vt, "vt"); save_long(long_vt, "he_vt"); res_vt <- check_long_vs_source(long_vt, file.path(OUTPUT_DIR, "elect_he_cty_vt.rds")); print(res_vt)

## ---------------- Delaware ----------------
de_party <- function(x) { x <- trimws(x); case_when(x == "DEMOCRATIC" ~ "DEM", x == "REPUBLICAN" ~ "REP", TRUE ~ "OTHER") }
de <- purrr::map_dfr(c(2000, 2002, 2004, 2006, 2008, 2010, 2014), function(y) rd("delaware", paste0(y, "_general_precinct.csv")) %>%
  filter(office == "U.S. House", election_district != "Total") %>%
  transmute(year = y, county = toupper(trimws(county)), district, candidate, party_lbl = party, party_group = de_party(party), votes = as.numeric(votes))) %>%
  filter(!is.na(votes)) %>% inner_join(fips_of("DELAWARE"), by = c("county" = "county_name")) %>% transmute(year, county_fips, district = "00", candidate, party = party_lbl, party_group, votes)   # DE: at-large seat, source has no district
long_de <- finalize_long(de, "de"); save_long(long_de, "he_de"); res_de <- check_long_vs_source(long_de, file.path(OUTPUT_DIR, "elect_he_cty_de.rds")); print(res_de)
for (n in c("ri", "vt", "de")) { l <- get(paste0("long_", n)); message(toupper(n), ": rows ", nrow(l), "; NA district ", sum(is.na(l$district)), "; candidates ", n_distinct(l$year, l$candidate)) }
