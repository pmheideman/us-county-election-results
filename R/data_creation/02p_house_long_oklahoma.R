## Candidate-level LONG tables for Oklahoma: he_cty_ok (OpenElections 2004-2014, logic of 01aq) and he_cty_ok_pdfs (official PDFs 1994-2010,
## from ok_pdfs_house_long.rds built by 01bz). Acceptance: derived shares == each shares file.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)
RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "oklahoma")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ok_fips <- xw %>% filter(state == "OKLAHOMA") %>% select(county_name, county_fips)
to_party <- function(x) { x <- toupper(trimws(gsub("[()]", "", x))); case_when(grepl("^DEM|^D$", x) ~ "DEM", grepl("^REP|^R$", x) ~ "REP", TRUE ~ "OTHER") }
is_pseudo_row <- function(candidate) { c <- toupper(trimws(candidate)); is.na(c) | c == "" | grepl("^OVER VOTES", c) | grepl("^UNDER VOTES", c) | grepl("^TOTAL", c) | grepl("^WRITE-IN", c) }
rd <- function(f, office_val, year, vcol = "votes", numf = as.numeric)
  read_csv(file.path(RAW_DIR, f), show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(office == office_val, !is_pseudo_row(candidate)) %>%
    transmute(county = toupper(trimws(county)), district, candidate = trimws(candidate), party_raw = trimws(gsub("[()]", "", party)), party_group = to_party(party),
              votes = numf(.data[[vcol]]), year = year)
raw <- bind_rows(rd("2004_general_county.csv", "U.S. House of Represenatives", 2004, numf = function(x) as.numeric(gsub(",", "", x))),
                 rd("2008_general_county.csv", "U.S. House", 2008), rd("2010_general_precinct.csv", "U.S. House", 2010),
                 rd("2012_general_county.csv", "U.S. House", 2012), rd("2014_general_precinct.csv", "U.S. House", 2014, vcol = "total_votes")) %>%
  filter(!is.na(votes)) %>% mutate(county = case_when(county == "LEFLORE" ~ "LE FLORE", TRUE ~ county)) %>%
  group_by(year, county, district, candidate, party_raw, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  left_join(ok_fips, by = c("county" = "county_name")) %>% filter(!is.na(county_fips)) %>% rename(party = party_raw)
long_a <- finalize_long(raw, "ok"); save_long(long_a, "he_ok")
message("OK (OE) long rows: ", nrow(long_a)); check_long_vs_source(long_a, file.path(OUTPUT_DIR, "elect_he_cty_ok.rds"))

## --- he_cty_ok_pdfs: official PDFs (party letters D/R/L/I/Rfm/...; group as in 01bz: D -> DEM, R -> REP, else OTHER)
p <- readRDS(file.path(OUTPUT_DIR, "ok_pdfs_house_long.rds")); print(table(p$party))
pl <- p %>% transmute(year, county_fips, district, candidate, party_letter = party, votes = as.numeric(votes),
                      party = c(D = "Democratic", R = "Republican", L = "Libertarian", I = "Independent", Rfm = "Reform", O = "Other")[party],
                      party_group = case_when(party_letter == "D" ~ "DEM", party_letter == "R" ~ "REP", TRUE ~ "OTHER"))
long_b <- finalize_long(pl %>% select(-party_letter), "ok_pdfs"); save_long(long_b, "he_ok_pdfs")
message("OK (PDF) long rows: ", nrow(long_b)); check_long_vs_source(long_b, file.path(OUTPUT_DIR, "elect_he_cty_ok_pdfs.rds"))
