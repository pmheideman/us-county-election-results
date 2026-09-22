## Indiana 2010 U.S. House by county, from the "2010 Indiana Election Report" (Indiana Election Division; R/data/county_house_files/Indiana_2010_ELECTION_RESULTS_155618.pdf,
## 111 pages, image-only 300 dpi scans; the general-election "United States Representative" section is pdf pages 68-70 = report pages 6-8). Same layout as the 2002 report:
## district -> candidate (party) with printed total -> rows of "County votes". The candidate names, parties and printed totals and every county cell were READ FROM THE PAGE IMAGES
## (in2010_build_transcription.py; tesseract text of the same pages agrees in 344 of 348 cells it could read, the 4 OCR errors are logged in R/output/in_ocr_corrections_2010.csv).
## Checks: county sums == printed candidate totals (32 of 32); county count per district; district totals == Wikipedia; House total vs the Senate 2010 total per county (panel sample SE).
## The booklet also holds the May 4 2010 primary returns (pp. 8-62) and the general election returns for Senator, Secretary of State, Auditor, Treasurer, U.S. Representative, State Senator,
## State Representative, judges and prosecuting attorneys (pp. 63-107).
## Outputs: R/output/long/he_in_2010.rds, R/output/elect_he_cty_in_2010.rds, R/output/in_ocr_corrections_2010.csv (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "indiana_official")
tr <- read_csv(file.path(DIR, "in_2010_transcription.csv"), col_types = cols(district = "i", .default = "c", votes = "d", year = "i")) %>% mutate(district = sprintf("%02d", district))
cand <- jsonlite::fromJSON(file.path(DIR, "in2010_candidates.json")); names(cand) <- NULL
printed <- tibble(district = sprintf("%02d", as.integer(cand[, 1])), candidate = cand[, 2], party = cand[, 3], printed = as.numeric(cand[, 4]))
chk <- tr %>% group_by(district, candidate, party) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(printed, by = c("district", "candidate", "party")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(chk), "| county sums equal the printed candidate total:", sum(chk$ok), "\n"); stopifnot(all(chk$ok))
cat("counties per district:", paste(names(table(tr$district[!duplicated(tr[, c("district", "county")])])), table(tr$district[!duplicated(tr[, c("district", "county")])]), sep = "=", collapse = ", "), "\n")
## district totals vs Wikipedia (2010 United States House of Representatives elections in Indiana; R, D, other per district)
wp <- tribble(~district, ~rep, ~dem, ~other, "01", 65558, 99387, 4762, "02", 88803, 91341, 9447, "03", 116140, 61267, 7642, "04", 138732, 53167, 10423, "05", 146899, 60024, 29484, "06", 126027, 56647, 6635, "07", 55213, 86011, 4815, "08", 117259, 76265, 10240, "09", 118040, 95353, 12139)
mine <- tr %>% group_by(district) %>% summarise(rep = sum(votes[party == "R"]), dem = sum(votes[party == "D"]), other = sum(votes[!party %in% c("D", "R")]), .groups = "drop")
w <- wp %>% inner_join(mine, by = "district", suffix = c(".wp", ".me")); cat("district totals equal Wikipedia (R, D, other) in", sum(w$rep.wp == w$rep.me & w$dem.wp == w$dem.me & w$other.wp == w$other.me), "of 9 districts\n"); stopifnot(all(w$rep.wp == w$rep.me & w$dem.wp == w$dem.me & w$other.wp == w$other.me))

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "INDIANA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 92)
tr <- tr %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(tr$county_fips))
PARTY <- c(D = "Democratic", R = "Republican", L = "Libertarian", I = "Independent", W = "Write-In")
raw <- tr %>% transmute(year = 2010L, county_fips, district, candidate, party_group = case_when(party == "D" ~ "DEM", party == "R" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party], votes)     # party_group BEFORE the label overwrites the code
long <- finalize_long(raw, "in_2010")
## finalize_long strips punctuation from names: hand-restore the ones affected
NAMES <- c("Peter J Visclosky" = "Peter J. Visclosky", "Marlin A Stutzman" = "Marlin A. Stutzman", "Richard Chard Reid" = "Richard (Chard) Reid", "Jesse C Trueblood" = "Jesse C. Trueblood",
           "Talmage T J Thompson Jr" = "Talmage (T. J.) Thompson, Jr.", "Andre D Carson" = "Andre D. Carson", "Marvin B Scott" = "Marvin B. Scott", "Larry D Bucshon" = "Larry D. Bucshon",
           "W Trent Vanhaaften" = "W. Trent VanHaaften", "Greg No Bull Knott" = "Greg \"No Bull\" Knott", "Jerry R Lucas" = "Jerry R. Lucas")
key <- function(z) gsub("[^a-z]", "", tolower(z)); nm <- setNames(unname(NAMES), key(names(NAMES))); hit <- key(long$candidate) %in% names(nm); long$candidate[hit] <- nm[key(long$candidate[hit])]
cat("candidates:", paste(unique(long$candidate), collapse = " | "), "\n")
save_long(long, "he_in_2010")
shares <- derive_shares(long) %>% transmute(state = "INDIANA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_in_2010.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_in_2010.rds")) %>% select(source, keys_source, matched, mismatched, pass))
cat("counties:", n_distinct(long$county_fips), " split counties:", paste(sort(unique(long$county_fips[long$county_fips %in% (long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% filter(d > 1) %>% pull(county_fips))])), collapse = ","), "\n")
## House total vs the U.S. Senate 2010 total of the same county (independent source: panel sample SE, MEDSL-independent historical file)
se <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "SE", year == 2010, cty_fips %/% 1000 == 18) %>% select(cty_fips, sen = totalvote)
r <- shares %>% inner_join(se, by = "cty_fips") %>% mutate(ratio = totalvote / sen)
cat("House 2010 / Senate 2010 total per county (", nrow(r), " counties): min", round(min(r$ratio), 3), "median", round(median(r$ratio), 3), "max", round(max(r$ratio), 3), "\n"); print(as.data.frame(r %>% filter(ratio < 0.93 | ratio > 1.07) %>% select(cty_fips, totalvote, sen, ratio)))
## OCR-vs-image differences log
d <- jsonlite::fromJSON(file.path(DIR, "in2010_ocr_vs_image.json"))$ocr_disagreements
log <- tibble(district = c("03", "04", "04", "05"), county = d[, 2], candidate = d[, 1], ocr_value = as.numeric(d[, 3]), corrected_value = as.numeric(d[, 4]), how_verified = "tesseract misread or truncated the cell; image read, and the candidate's county sum equals the printed total")
write_csv(log, file.path(OUTPUT_DIR, "in_ocr_corrections_2010.csv")); write_csv(log, file.path(PROJECT_ROOT, "R", "output", "in_ocr_corrections_2010.csv"))
## comparison with the old OpenElections rows in the panel (partial, 66 counties)
old <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_in.rds")) %>% filter(year == 2010) %>% select(cty_fips, o_tot = totalvote, o_dem = demovote, o_rep = repuvote)
cmp <- shares %>% left_join(old, by = "cty_fips") %>% mutate(status = case_when(is.na(o_tot) ~ "missing in old data", abs(totalvote - o_tot) < 0.5 & abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 ~ "identical", TRUE ~ "old differs"))
cat("\nvs the old OpenElections rows for 2010:\n"); print(table(cmp$status)); print(as.data.frame(cmp %>% filter(status == "old differs") %>% mutate(rel = round(o_tot / totalvote, 2)) %>% select(cty_fips, totalvote, o_tot, rel) %>% head(12)))
write_csv(cmp %>% select(cty_fips, totalvote, o_tot, status), file.path(PROJECT_ROOT, "R", "output", "in_2010_vs_old_openelections.csv"))
