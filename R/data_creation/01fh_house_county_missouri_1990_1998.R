## Missouri U.S. House, county level, 1990-1998, from the Official Manual (Blue Book) tables transcribed by hand from page images (01fg_missouri_1990_1998_parse.py ->
## R/data/county_house_files/missouri/missouri_house_county_1990_1998.csv): per district, the votes of every listed candidate in every county (or part of a county); every district's county rows add up to the
## printed TOTALS row except 1992 District 8's Republican column (270 votes short, see the parse script). Split counties (St. Louis, St. Louis City, Jackson, St. Charles, Franklin) are summed over the districts;
## "Kansas City" rows belong to Jackson County. Party: D -> DEM, R -> REP, everything else (L, NL, I, Green, Reform, U.S. Taxpayers, Write-In) OTHER.
## Outputs: R/output/long/he_mobb_<year>.rds and R/output/elect_he_cty_mobb_<year>.rds for 1990-1998 (folded into the panel by 01fi_missouri_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
d <- read_csv(file.path(PROJECT_ROOT, "R/data/county_house_files/missouri/missouri_house_county_1990_1998.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), raw_county = county,
         county = toupper(trimws(sub("\\s*\\(part of\\)\\s*$", "", county))),
         county = case_when(county == "ST. LOUIS" | county == "ST. LOUIS COUNTY" ~ "ST. LOUIS COUNTY", county == "KANSAS CITY" ~ "JACKSON", county == "DEKALB" ~ "DEKALB", TRUE ~ county))
mo <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "MISSOURI", !is.na(county_fips), county_fips > 29000, county_fips < 30000) %>%
  transmute(county_fips, county = gsub("\\.", "", toupper(trimws(county_name)))) %>% distinct(county, .keep_all = TRUE); d$county <- gsub("\\.", "", d$county)   # dotted and undotted spellings ("ST. LOUIS" / "ST LOUIS") both occur
stopifnot(n_distinct(mo$county_fips) == 115, all(d$county %in% mo$county))
raw <- d %>% inner_join(mo %>% select(county, county_fips), by = "county") %>% mutate(party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"),
  party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", party_code == "L" ~ "Libertarian", party_code == "NL" ~ "Natural Law", party_code == "I" ~ "Independent", TRUE ~ party_code)) %>%
  group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
chk <- raw %>% distinct(year, district, candidate, party_group) %>% count(year, district, party_group) %>% filter(party_group != "OTHER", n > 1); cat("district-years with more than one DEM or REP candidate:", nrow(chk), "\n")
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y), paste0("mobb_", y)); save_long(long, paste0("he_mobb_", y))
  shares <- derive_shares(long) %>% transmute(state = "MISSOURI", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 115)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mobb_%d.rds", y))); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mobb_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
