## North Dakota: found via OpenElections (github.com/openelections/openelections-data-nd). Part of
## the post-large-states push through the remaining state list. Repo spans 2000-2024, GitHub's
## listing API was rate-limited so the repo was fetched as a codeload.github.com tarball instead.
## Every general-election year 2000-2014 already has a single county-level file
## (county,office,district,party,candidate,votes), office consistently labeled exactly "U.S. House"
## every year (distinct from "State House"/"State House Legislative") -- no per-year alias table
## needed. North Dakota has had only ONE at-large U.S. House seat for its entire statehood, so every
## row is the same single district -- no district filtering needed either (same structure as
## Montana). All 53 counties present every year (row counts are always an exact multiple of 53:
## 53 x [5,2,2,2,2,3,4,4] candidates for 2000/02/04/06/08/10/12/14) -- no pseudo-total row in any
## year (checked: no row count is 54x anything, and no county value looks like "Total").
##
## One real gotcha: 2000's `party` column is entirely NA/blank (unlike every other year, which has
## clean DEM/REP codes from 2002 on) -- hand-mapped from the two-candidate vote totals, which match
## the real historical 2000 ND-AL race exactly (Earl Pomeroy, the real Democratic incumbent: 151,173
## votes; John Dorso, the real Republican challenger: 127,251 votes; three minor candidates get
## <5,000 votes each and fall to OTHER by the normal classifier).
##
## All 53 county names matched the shared crosswalk exactly -- zero aliases needed, unusually clean.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "north_dakota")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

nd_fips <- county_fips_crosswalk %>% filter(state == "NORTH DAKOTA") %>% select(county_name, county_fips)
stopifnot(nrow(nd_fips) == 53)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-nd/master/", remote_path)
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

## 2000's party column is blank -- hand-map the two major-party candidates by name.
to_party_2000 <- function(candidate) {
  case_when(
    candidate == "Earl Pomeroy" ~ "DEM",
    candidate == "John Dorso" ~ "REP",
    TRUE ~ "OTHER"
  )
}

ND_FILES <- c(
  "2000" = "2000/20001107__nd__general__county.csv",
  "2002" = "2002/20021105__nd__general__county.csv",
  "2004" = "2004/20041102__nd__general__county.csv",
  "2006" = "2006/20061107__nd__general__county.csv",
  "2008" = "2008/20081104__nd__general__county.csv",
  "2010" = "2010/20101102__nd__general__county.csv",
  "2012" = "2012/20121106__nd__general__county.csv",
  "2014" = "2014/20141104__nd__general__county.csv"
)

read_year <- function(year_chr) {
  path <- download_oe(ND_FILES[[year_chr]], paste0(year_chr, "_general_county.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  out <- raw %>%
    filter(trimws(office) == "U.S. House") %>%
    transmute(
      county = toupper(trimws(county)), candidate = trimws(candidate), party_raw = party,
      votes = as.numeric(votes), year = as.numeric(year_chr)
    )
  if (year_chr == "2000") {
    out <- out %>% mutate(party = to_party_2000(candidate))
  } else {
    out <- out %>% mutate(party = to_party(party_raw))
  }
  out %>% select(-party_raw)
}

message("Fetching North Dakota House data, 8 years...")
all_rows <- bind_rows(lapply(names(ND_FILES), read_year)) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_nd <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nd_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NORTH DAKOTA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_nd")

message("ND House county-level rows built: ", nrow(elect_he_cty_nd), " (of possible ", 53 * 8, ")")
print(table(elect_he_cty_nd$year))

sanity <- elect_he_cty_nd$repuvote + elect_he_cty_nd$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_nd %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

for (yr in sort(unique(elect_he_cty_nd$year))) {
  present <- elect_he_cty_nd %>% filter(year == yr) %>% pull(cty_fips)
  missing <- nd_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## ---- Non-production 2016 cross-check against MEDSL (validation only, not folded in) ----
## 2016's file is a different, party-less schema (county,office,candidate,votes) with office
## labeled "Us House" (not "U.S. House") and a "TOTALS" pseudo-candidate row (confirmed: its value
## exactly equals the sum of the 4 real candidate rows) -- none of this affects the production
## 2000-2014 build above, just this bonus validation check. Party hand-mapped from the real 2016
## ND-AL race (Kevin Cramer R vs Chase Iron Eyes D).
nd_2016_raw <- {
  path <- download_oe("2016/20161108__nd__general__county.csv", "2016_general_county.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "Us House", trimws(candidate) != "TOTALS") %>%
    transmute(
      county = toupper(trimws(county)),
      party = case_when(candidate == "Kevin Cramer" ~ "REP", candidate == "Chase Iron Eyes" ~ "DEM", TRUE ~ "OTHER"),
      votes = as.numeric(votes)
    )
}
nd_2016 <- nd_2016_raw %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(nd_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_nd_2016 <- medsl_final %>%
  filter(sample == "HE", year == 2016, cty_fips %in% nd_fips$county_fips)

if (nrow(medsl_nd_2016) > 0) {
  cmp <- nd_2016 %>%
    inner_join(medsl_nd_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL ND 2016 HE rows found to cross-check against.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
