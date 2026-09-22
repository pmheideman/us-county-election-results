## Montana: found via OpenElections (github.com/openelections/openelections-data-mt). Part of the
## post-large-states push through the remaining state list. Repo spans 2000-2020, and unusually
## clean for this project: every general-election year (2000, 2002, 2004, 2006, 2008, 2010, 2012,
## 2014) already has a single county-level file with an identical schema
## (county,office,district,party,candidate,votes), office consistently labeled exactly "U.S. House"
## every year (distinct from "State House") -- no per-year alias table or file-shape branching
## needed, unlike almost every other state handled so far. Montana had only ONE at-large U.S. House
## seat for this entire span (lost its 2nd seat after the 1990 census, regained one only in 2022),
## so every row is district 1 -- no district filtering needed either.
##
## One pseudo-row found: 2002 and 2010 (only those two years) have an extra "Total" county row
## alongside the real 56 -- confirmed, not assumed, to be a state-total duplicate by checking its
## value equals the sum of the real per-candidate totals exactly (e.g. 2002 Rehberg: Total=214100 ==
## sum of the 56 real counties' Rehberg votes). Filtered by `county != "Total"`.
##
## One crosswalk alias needed: source spells "Lewis & Clark" (ampersand), crosswalk has
## "LEWIS AND CLARK" -- otherwise all 56 Montana county names matched exactly, no other aliases.
##
## Cross-checked 2016 (not part of production output, MEDSL already covers 2016+) against MEDSL:
## used purely as a methodology validation given how clean this state's format is.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "montana")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))

mt_fips <- county_fips_crosswalk %>% filter(state == "MONTANA") %>% select(county_name, county_fips)
stopifnot(nrow(mt_fips) == 56)

download_oe <- function(remote_path, local_name) {
  dest <- file.path(RAW_DIR, local_name)
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-mt/master/", remote_path)
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

## Alias for the one county-name spelling mismatch against the shared crosswalk.
normalize_county <- function(x) {
  x <- toupper(trimws(x))
  case_when(x == "LEWIS & CLARK" ~ "LEWIS AND CLARK", TRUE ~ x)
}

MT_FILES <- c(
  "2000" = "2000/20001107__mt__general__county.csv",
  "2002" = "2002/20021105__mt__general__county.csv",
  "2004" = "2004/20041102__mt__general__county.csv",
  "2006" = "2006/20061107__mt__general__county.csv",
  "2008" = "2008/20081104__mt__general__county.csv",
  "2010" = "2010/20101102__mt__general__county.csv",
  "2012" = "2012/20121106__mt__general__county.csv",
  "2014" = "2014/20141104__mt__general__county.csv"
)

read_year <- function(year_chr) {
  path <- download_oe(MT_FILES[[year_chr]], paste0(year_chr, "_general_county.csv"))
  raw <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  raw %>%
    filter(trimws(office) == "U.S. House", trimws(county) != "Total") %>%
    transmute(
      county = normalize_county(county), candidate = trimws(candidate),
      party = to_party(party), votes = as.numeric(votes), year = as.numeric(year_chr)
    )
}

message("Fetching Montana House data, 8 years...")
all_rows <- bind_rows(lapply(names(MT_FILES), read_year)) %>%
  filter(!is.na(votes))

message("Raw rows fetched: ", nrow(all_rows))
print(table(all_rows$year))

elect_he_cty_mt <- all_rows %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mt_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "MONTANA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_mt")

message("MT House county-level rows built: ", nrow(elect_he_cty_mt), " (of possible ", 56 * 8, ")")
print(table(elect_he_cty_mt$year))

sanity <- elect_he_cty_mt$repuvote + elect_he_cty_mt$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

low_share <- elect_he_cty_mt %>% filter(demovote + repuvote < 0.5) %>% arrange(year)
message(nrow(low_share), " county-years with demovote+repuvote < 0.5:")
print(low_share)

## Which counties are missing per year, for the coverage record
for (yr in sort(unique(elect_he_cty_mt$year))) {
  present <- elect_he_cty_mt %>% filter(year == yr) %>% pull(cty_fips)
  missing <- mt_fips %>% filter(!county_fips %in% present)
  if (nrow(missing) > 0) {
    message(yr, ": missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
  }
}

## ---- Non-production 2016 cross-check against MEDSL (validation only, not folded in) ----
mt_2016_raw <- {
  path <- download_oe("2016/20161108__mt__general__county.csv", "2016_general_county.csv")
  read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
    filter(trimws(office) == "U.S. House", trimws(county) != "Total") %>%
    transmute(county = normalize_county(county), party = to_party(party), votes = as.numeric(votes))
}
mt_2016 <- mt_2016_raw %>%
  group_by(county) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(mt_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(cty_fips = county_fips, demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote)

medsl_final <- readRDS(file.path(PROJECT_ROOT, "R", "output", "elect_cty_final.rds"))
medsl_mt_2016 <- medsl_final %>%
  filter(sample == "HE", year == 2016, cty_fips %in% mt_fips$county_fips)

if (nrow(medsl_mt_2016) > 0) {
  cmp <- mt_2016 %>%
    inner_join(medsl_mt_2016, by = "cty_fips", suffix = c("_oe", "_medsl")) %>%
    mutate(diff_dem = abs(demovote_oe - demovote_medsl), diff_rep = abs(repuvote_oe - repuvote_medsl))
  message("2016 cross-check vs MEDSL: ", nrow(cmp), " counties matched. ",
          "mean diff_dem=", round(mean(cmp$diff_dem), 5), " max diff_dem=", round(max(cmp$diff_dem), 5),
          " mean diff_rep=", round(mean(cmp$diff_rep), 5), " max diff_rep=", round(max(cmp$diff_rep), 5))
} else {
  message("No MEDSL MT 2016 HE rows found to cross-check against.")
}

## NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv NOT
## re-run here -- multiple states are being built in parallel by concurrent agents, and the
## fold-in / tracker-refresh step is being done centrally afterward to avoid a lost-update race
## on those shared files.
