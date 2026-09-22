## Candidate-level LONG tables for Georgia U.S. House: 2000-2010 + 2014 (OpenElections county files; shares file elect_he_cty_ga.rds)
## and 2012 (manual browser export georgia_2012.csv; shares file elect_he_cty_ga_2012.rds). Southeast batch.
## Mirrors 01z / 01d exactly: OE party via startsWith DEM/REP else OTHER, 2014 votes = sum of the 4 method columns; the 2012 file is already
## county-level with a "Total Votes" pseudo-row per county-district that the ORIGINAL used as totalvote. Here the pseudo-row is not a
## candidate; each candidate row keeps its votes, and if candidate rows fall short of the printed Total Votes the shortfall is added as an
## explicit "Unattributed (source total exceeds candidate rows)" OTHER row so the derived totalvote still equals the original's.
## Outputs: R/output/long/he_ga.rds, R/output/long/he_ga_2012.rds.

source(file.path("R", "00_setup.R"))
save_step <- function(df, name) invisible(df)
library(readr)
source(file.path("R", "long_helpers.R"))
exprs <- parse(file = file.path("R", "data_creation", "01z_house_county_georgia.R"))
want <- c("RAW_DIR", "county_fips_crosswalk", "ga_fips", "download_oe", "to_party", "GA_COUNTY_FILES")
eval(exprs[vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% want, NA)])

grp <- function(p) case_when(startsWith(toupper(trimws(p)), "DEM") ~ "DEM", startsWith(toupper(trimws(p)), "REP") ~ "REP", TRUE ~ "OTHER")
read_year <- function(year, remote_dir, remote_name) {
  read_csv(download_oe(remote_dir, remote_name, paste0(year, "_general.csv")), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House") %>%
    mutate(county = toupper(trimws(county)), votes = as.numeric(votes)) %>% filter(!is.na(votes)) %>%
    transmute(year = year, county, district, candidate, party, party_group = grp(party), votes)
}
a <- purrr::pmap_dfr(GA_COUNTY_FILES, read_year)
b <- read_csv(download_oe("2014", "20141104__ga__general__county-level.csv", "2014_general_county.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  filter(trimws(office) == "U.S. House") %>%
  mutate(county = toupper(trimws(county)),
         votes = as.numeric(election_day_votes) + as.numeric(advanced_votes) + as.numeric(absentee_by_mail_votes) + as.numeric(provisional_votes)) %>%
  filter(!is.na(votes)) %>% transmute(year = 2014, county, district, candidate, party, party_group = grp(party), votes)
ga_long <- bind_rows(a, b) %>% inner_join(ga_fips, by = c("county" = "county_name")) %>% select(-county) %>% finalize_long("ga")
save_long(ga_long, "he_ga")
r1 <- check_long_vs_source(ga_long, file.path(OUTPUT_DIR, "elect_he_cty_ga.rds"))

## ---- 2012 manual file --------------------------------------------------------------------------------------------------------------------------
raw <- read_csv(file.path(PROJECT_ROOT, "R", "data", "county_house_files", "georgia_2012.csv"), show_col_types = FALSE) %>%
  filter(grepl("^U.S. Representative", `Office Name`)) %>%
  mutate(county = gsub("\\s+COUNTY$", "", toupper(trimws(Precinct))), district = sub("^.*?(\\d+).*$", "\\1", `Office Name`))
tot <- raw %>% filter(`Ballot Name` == "Total Votes") %>% transmute(county, office = `Office Name`, district, printed_total = Total)
cand <- raw %>% filter(`Ballot Name` != "Total Votes") %>%
  transmute(county, office = `Office Name`, district, candidate = sub("\\s*\\([A-Za-z]+\\)[A-Za-z]*\\s*$", "", `Ballot Name`), party = Party,
            party_group = ifelse(!is.na(Party) & Party == "DEM", "DEM", ifelse(!is.na(Party) & Party == "REP", "REP", "OTHER")), votes = Total)
gap <- tot %>% left_join(cand %>% group_by(county, office) %>% summarise(cand_sum = sum(votes, na.rm = TRUE), .groups = "drop"), by = c("county", "office")) %>%
  mutate(cand_sum = coalesce(cand_sum, 0), shortfall = printed_total - cand_sum)
message("2012: county-districts ", nrow(tot), "; candidate sums differ from printed Total Votes in ", sum(gap$shortfall != 0), " (net ", sum(gap$shortfall), ")")
extra <- gap %>% filter(shortfall != 0) %>% transmute(county, district, candidate = "Unattributed (source total exceeds candidate rows)", party = "", party_group = "OTHER", votes = shortfall)
if (any(extra$votes < 0)) print(as.data.frame(extra %>% filter(votes < 0)))
ga12 <- bind_rows(cand %>% filter(!is.na(votes)) %>% select(county, district, candidate, party, party_group, votes), extra) %>%
  inner_join(ga_fips %>% mutate(county_name = gsub("\\s+COUNTY$", "", county_name)), by = c("county" = "county_name")) %>%
  mutate(year = 2012) %>% select(year, county_fips, district, candidate, party, party_group, votes) %>% finalize_long("ga_2012")
save_long(ga12, "he_ga_2012")
r2 <- check_long_vs_source(ga12, file.path(OUTPUT_DIR, "elect_he_cty_ga_2012.rds"))
print(bind_rows(r1, r2))
message("GA rows ", nrow(ga_long), " / GA2012 rows ", nrow(ga12), "; candidates ", nrow(distinct(ga_long, year, district, candidate)), " / ", nrow(distinct(ga12, district, candidate)))
print(as.data.frame(head(ga12 %>% filter(county_fips == 13001), 6)))
