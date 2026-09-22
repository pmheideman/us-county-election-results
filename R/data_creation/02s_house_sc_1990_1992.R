## South Carolina 1990 and 1992 U.S. House by county, from the State Election Commission election reports (scanned computer printouts; the OCR text layer is unusable, so the page
## images were READ BY HAND):
##   1992: R/data/county_house_files/SC_Election_Report_1992-1993.pdf, pp. 82-84 ("REPRESENTATIVE IN CONGRESS, DISTRICT n", NOVEMBER 3, 1992 GENERAL ELECTION): clear print, one table per district
##         (county rows + TOTALS row).
##   1990: SC_Election_Report_1990-1991.pdf, pp. 93-95 ("U.S. CONGRESSIONAL DISTRICTS - VOTES CAST IN GENERAL ELECTION HELD NOVEMBER 6, 1990", DISTRICT NO 001-006, county rows + DISTRICT TOTALS).
##         Dot-matrix print (digits 0/8/6/9 and 3/5/8 are easily confused) and candidate names printed vertically. The tables are printed TWICE (p.93-94 and a second copy on p.95): each
##         cell was read in both copies; where the copies disagree, the value that makes the column add up to the printed DISTRICT TOTALS was taken (in all cases exactly one combination
##         of the two readings did, e.g. 1990 D3 Democratic column: 3,832 / 3,061 / 1,361 / 9,832 / 2,826 -> 86,103). The printed district totals were confirmed against Wikipedia's results
##         (which cite the SEC report), and the SEC's county precinct pages (COUNTY TOTALS rows) support the odd Sumter row.
## Transcriptions: R/data/raw_house_county_open_states/south_carolina_official/sc_1990_transcription.csv, sc_1992_transcription.csv; printed district totals: sc_1990_1992_printed_totals.csv.
## Checks: every candidate's county sum == the printed total (36 columns); county count per district; House total vs the same-year presidential total (1992) / 1988 (1990) in the panel.
## Split counties: a county in two districts has one row per district (1992: Aiken D2/D3, Beaufort D2/D6, Berkeley D1/D6, Calhoun D2/D6, Charleston D1/D6, Colleton D2/D6, Darlington D5/D6, Dorchester D1/D6, Laurens D3/D4, Lee D5/D6, Orangeburg D2/D6, Richland D2/D6, Sumter D5/D6 = 13 counties; 1990: only Berkeley D1/D6).
## Outputs: R/output/long/he_sc_1990.rds, he_sc_1992.rds, R/output/elect_he_cty_sc_1990.rds, elect_he_cty_sc_1992.rds (NEW files); R/output/sc_ocr_corrections_1990_1992.csv (log).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_carolina_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "SOUTH CAROLINA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 46)
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", A = "American", W = "Write-In")
printed <- read_csv(file.path(DIR, "sc_1990_1992_printed_totals.csv"), col_types = cols(year = "i", district = "i", candidate = "c", party_code = "c", printed_total = "d"))
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
stopifnot(!any(pe$sample == "HE" & pe$cty_fips %/% 1000 == 45 & pe$year %in% c(1990, 1992)))     # the panel has no SC House rows for these years
for (yr in c(1990L, 1992L)) {
  t <- read_csv(file.path(DIR, sprintf("sc_%d_transcription.csv", yr)), col_types = cols(year = "i", district = "i", county = "c", candidate = "c", party_code = "c", votes = "d")) %>%
    mutate(county_fips = xw$county_fips[match(norm(county), xw$key)], district_c = sprintf("%02d", district)); stopifnot(!anyNA(t$county_fips))
  chk <- t %>% group_by(district, candidate, party_code) %>% summarise(county_sum = sum(votes), n_cty = n_distinct(county_fips), .groups = "drop") %>%
    full_join(printed %>% filter(year == yr), by = c("district", "candidate", "party_code")); stopifnot(!anyNA(chk$county_sum), !anyNA(chk$printed_total), all(chk$county_sum == chk$printed_total))
  cat(yr, ": all", nrow(chk), "candidate columns tie exactly to the printed totals; counties per district:", paste(unique(t %>% distinct(district, county_fips) %>% count(district) %>% mutate(s = paste0("D", district, "=", n)) %>% pull(s)), collapse = " "), "\n")
  raw <- t %>% transmute(year = yr, county_fips, district = district_c, candidate, party = PARTY[party_code], party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes)
  long <- finalize_long(raw, paste0("sc_", yr))
  hit <- long$candidate %in% c("Arthur Ravenel Jr", "Bill Oberst Jr", "Liz J Patterson", "John R Peeples"); NM <- c("Arthur Ravenel Jr" = "Arthur Ravenel, Jr.", "Bill Oberst Jr" = "Bill Oberst, Jr.", "Liz J Patterson" = "Liz J. Patterson", "John R Peeples" = "John R. Peeples")
  long$candidate[hit] <- NM[long$candidate[hit]]; long$candidate[toupper(long$candidate) == "WRITE-IN"] <- "Write-In"            # finalize_long strips punctuation from names
  save_long(long, paste0("he_sc_", yr))
  shares <- derive_shares(long) %>% transmute(state = "SOUTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_sc_%d.rds", yr)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_sc_%d.rds", yr))) %>% select(source, keys_source, matched, mismatched, pass))
  cat("counties:", n_distinct(long$county_fips), " split counties:", paste(sort(unique(t$county[duplicated(paste(t$county, t$candidate)) ])), collapse = ", "), " | districts:", n_distinct(long$district), " | candidates:", n_distinct(long$candidate), "\n")
  ref <- pe %>% filter(sample == "PE", year == ifelse(yr == 1990, 1988, 1992)) %>% select(cty_fips, pe = totalvote)
  r <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / pe)
  cat("House", yr, "/ presidential", ifelse(yr == 1990, 1988, 1992), "total per county: min", round(min(r$ratio, na.rm = TRUE), 2), "median", round(median(r$ratio, na.rm = TRUE), 2), "max", round(max(r$ratio, na.rm = TRUE), 2), "; counties missing:", 46 - nrow(shares), "\n")
  print(as.data.frame(r %>% filter(ratio < 0.5 | ratio > 1.1) %>% select(cty_fips, totalvote, pe, ratio)))
}
