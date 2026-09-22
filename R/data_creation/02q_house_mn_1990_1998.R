## Minnesota U.S. House, county level, 1990-1998 (Nov general elections), from the Minnesota Secretary of State election books
## R/data/county_house_files/MN_{1990,1992,1994,1996,1998}-*-g-sec.pdf (scanned; OCR text layer): the section "VOTE FOR UNITED STATES REPRESENTATIVE BY COUNTY"
## (pp. 9-10 in 1990/1992, 11-12 in 1994, 13-14 in 1996, 15-18 in 1998): one table per district with a county row per candidate column and a printed TOTAL row.
## The page text was parsed with R/data/raw_house_county_open_states/minnesota_official/mn_parse.py / mn_build.py (kept as provenance; not needed to re-run this script)
## into mn_transcription_1990_1998.csv (year, district, county key, candidate index, votes), mn_candidates_1990_1998.csv and mn_printed_totals_1990_1998.csv.
## Values with OCR damage were read from the page images and fixed by the printed totals: R/output/mn_ocr_corrections_1990_1998.csv.
## Checks (all pass): every district's county sums equal the printed TOTAL row for every candidate column; 87 counties per year; shares vs Wikipedia's district percentages (within
## rounding; see notes); House total vs the presidential/Senate total per county in the panel.
## LIMITATION: the 1990-1996 books print only the listed candidates (no write-in column), so write-in votes (about 0.1-0.5% of a district's vote) are not in these county totals; 1998 prints a
## Write-In column and includes them. Parties: DFL -> DEM group; IR (Independent-Republican, 1990-1994) and R (1996+) -> REP group ("Republican"); everything else OTHER.
## Outputs: R/output/long/he_mn_<year>.rds and R/output/elect_he_cty_mn_<year>.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "minnesota_official")
tr0 <- read_csv(file.path(DIR, "mn_transcription_1990_1998.csv"), col_types = "iccii")
cd0 <- read_csv(file.path(DIR, "mn_candidates_1990_1998.csv"), col_types = "icicc")
pt0 <- read_csv(file.path(DIR, "mn_printed_totals_1990_1998.csv"), col_types = "icii")
shift <- function(df, col) df %>% mutate(!!col := ifelse(year == 1996 & district == "04B", .data[[col]] + 4L, .data[[col]]), district = ifelse(district == "04B", "04", district))
tr <- shift(tr0, "candidate_idx"); cd <- shift(cd0, "candidate_idx"); pt <- shift(pt0, "candidate_idx")

## ---- counties -> FIPS ------------------------------------------------------------------------------------------------------------------------------
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MINNESOTA", !is.na(county_fips)) %>% distinct(county_name, county_fips) %>% mutate(key = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(key, county_fips)
stopifnot(n_distinct(xw$county_fips) == 87)
xw <- bind_rows(xw, tibble(key = "STLOUIS", county_fips = xw$county_fips[xw$key == "SAINTLOUIS"])) %>% distinct(key, .keep_all = TRUE)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(county_key, xw$key)])
if (anyNA(tr$county_fips)) { print(unique(tr$county_key[is.na(tr$county_fips)])); stop("unmapped county names") }

## ---- tie to the printed TOTAL rows ------------------------------------------------------------------------------------------------------------------
sums <- tr %>% group_by(year, district, candidate_idx) %>% summarise(s = sum(votes), .groups = "drop") %>% left_join(pt, by = c("year", "district", "candidate_idx"))
stopifnot(!anyNA(sums$printed_total), all(sums$s == sums$printed_total), nrow(sums) == nrow(cd))
cat("ALL", nrow(sums), "candidate columns tie exactly to the printed TOTAL rows (", n_distinct(sums$year), "years,", nrow(distinct(sums, year, district)), "district-years )\n")

## ---- long table ---------------------------------------------------------------------------------------------------------------------------------------
raw <- tr %>% left_join(cd %>% select(year, district, candidate_idx, candidate, party_code, party), by = c("year", "district", "candidate_idx")) %>%
  mutate(party_group = case_when(party_code == "DFL" ~ "DEM", party_code %in% c("IR", "R") ~ "REP", TRUE ~ "OTHER")) %>%
  group_by(year, county_fips, district) %>% filter(sum(votes) > 0) %>% ungroup()                       # rows that are all zero (e.g. Hennepin listed in district 2) carry no votes
stopifnot(!anyNA(raw$candidate), all(raw$party_group %in% c("DEM", "REP", "OTHER")))
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("mn_", y))
  save_long(long, paste0("he_mn_", y))
  shares <- derive_shares(long) %>% transmute(state = "MINNESOTA", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mn_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_mn_%d.rds", y))); stopifnot(all(r$pass))
  d <- raw %>% filter(year == y) %>% group_by(county_fips) %>% summarise(nd = n_distinct(district))
  cat(y, ": candidates", n_distinct(long$candidate), "| districts", n_distinct(long$district), "| counties", n_distinct(long$county_fips), "| split counties", sum(d$nd > 1), "\n")
}

## ---- county House total vs the presidential / Senate total in the panel -----------------------------------------------------------------------------
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(cty_fips %/% 1000 == 27, sample %in% c("PE", "SE"))
for (y in sort(unique(raw$year))) {
  s <- readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_mn_%d.rds", y)))
  ref <- panel %>% filter(year == y) %>% group_by(cty_fips) %>% summarise(ref = max(totalvote), .groups = "drop")
  r <- s %>% inner_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(y, ": House / same-year president-or-Senate total in the panel: n =", nrow(r), " min", round(min(r$ratio), 2), " median", round(median(r$ratio), 2), " max", round(max(r$ratio), 2), "\n")
}
