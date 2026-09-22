## Utah U.S. House, county-level, 1990-1998 general elections, from vote.utah.gov's official scanned canvass books
## (R/data/county_house_files/utah/<year>Gen.pdf). These 5 years are image-only (pdftotext extracts ~10-20 characters per file,
## confirmed) -- OCR was used only to LOCATE the U.S. House pages (grep the OCR text for "HOUSE"/"CONGRESSIONAL"), never trusted
## for the actual vote numbers. Every county x candidate cell was read directly off 300-400dpi page renders and hand-transcribed
## into R/data/county_house_files/utah/transcribed/<year>.csv, then verified: every district's county column sums were checked to
## tie EXACTLY to that district's own printed Total row before being accepted (all 5 years, all districts, tie exactly -- see the
## per-year comments below for the one real misread each of 1990 and 1992 caught this way).
##
## Utah had 3 U.S. House districts this whole period. District 2 is wholly contained in Salt Lake County every year (no other
## county ever has a District 2 row) -- confirmed by the printed District 2 county-level page never showing any other county's
## row filled, and by the District 2 total always equalling Salt Lake's own row where a per-county breakdown happens to be
## printed. District 1 and District 3 split the other 28 counties with NO further splits (each of those 28 sits wholly in D1 or
## D3), except Salt Lake, which is split across all three districts.
##
## 1990 gotcha: District 2's single row of totals is printed, by page layout accident, vertically aligned with the "SAN JUAN"
## county label rather than "SALT LAKE" -- confirmed NOT San Juan's data (San Juan's own "number voting" that year was only
## 4,432, far short of District 2's ~148,000 total votes) and confirmed IS Salt Lake's by elimination (Salt Lake is the only
## county in the state large enough, and District 3's own Salt Lake row plus District 2's total sum to just under Salt Lake's
## total "number voting", as expected for a split county with some undervote in this specific race).
## 1992 gotcha: initial column sum for the District 1 Lawrence(I) column was off by 1 vote from the printed total (16,504 vs
## 16,505) -- traced to a misread Tooele cell (766, not 765) on a second, more careful re-read; matches exactly once corrected.
## 1992 also has a 6th, second-page District 3 candidate (Charles M. Wilson, no party letter shown -- treated as OTHER) whose
## column was initially undercounted by 1 vote (Morgan County's "1" was missed on a first pass); ties exactly once included.
##
## Party classification: each column's own printed letter code (R/D/I/IA/L/NL/SW/WI/A/U) is kept as the raw party label;
## party_group is DEM only for "D", REP only for "R", everything else (Independent, Independent American, Libertarian, Natural
## Law, Socialist Workers, write-in, American, Unaffiliated, and 1992's unlabeled Wilson) is OTHER -- the project's standard rule.
##
## Companion build: 02af_house_ut_2000_2006.R covers the born-digital 2000-2006 years from the same vote.utah.gov archive.
## 2008/2010 were already done via 01bm_house_county_utah_historical_xls.R; 2012+ via OpenElections (01aw).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
TDIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "utah", "transcribed")

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "UTAH") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 29)

to_party_group <- function(x) ifelse(x == "D", "DEM", ifelse(x == "R", "REP", "OTHER"))

years <- c(1990, 1992, 1994, 1996, 1998)
raw_all <- list()
for (y in years) {
  df <- read_csv(file.path(TDIR, paste0(y, ".csv")), show_col_types = FALSE) %>%
    mutate(county = toupper(trimws(county)), party_group = to_party_group(party), year = y) %>%
    left_join(xw, by = c("county" = "county_name"))
  stopifnot(!anyNA(df$county_fips))
  raw_all[[as.character(y)]] <- df %>% transmute(year, county_fips, district = norm_district(district), candidate, party, party_group, votes)
}
raw <- bind_rows(raw_all)
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")

## ---- per-year tie check against each district's own printed Total row (reproduced here from the transcription/verification
## work above, as a permanent regression guard -- if a future edit to the CSVs breaks a tie, this stops the build) ----
PRINTED_TOTALS <- tribble(
  ~year, ~district, ~candidate, ~total,
  1990, "01", "James Hansen", 82746, 1990, "01", "Kenley Brunsdale", 69491, 1990, "01", "Reva Marx Wadsworth", 6429,
  1990, "02", "Genevieve Atwood", 58869, 1990, "02", "Wayne Owens", 85167, 1990, "02", "Eleanor Garcia", 411, 1990, "02", "Lawrence Topham", 3424,
  1990, "03", "Karl Snow", 49452, 1990, "03", "Bill Orton", 79163, 1990, "03", "Robert Smith", 6542, 1990, "03", "Anthony Dutrow", 519,
  1992, "01", "James V. Hansen", 160037, 1992, "01", "Ron Holt", 68712, 1992, "01", "William J. Lawrence", 16505,
  1992, "02", "Enid Greene", 118307, 1992, "02", "Karen Shepard", 127738, 1992, "02", "Eileen Koschak", 650, 1992, "02", "A. Peter Crane", 6274,
  1992, "03", "Richard R. Harrington", 84019, 1992, "03", "Bill Orton", 135029, 1992, "03", "Wayne L. Hill", 5764, 1992, "03", "Doug Jones", 1797, 1992, "03", "Nels J'Anthony", 384, 1992, "03", "Charles M. Wilson", 2068,
  1994, "01", "Bobbie Coray", 57664, 1994, "01", "James V. Hansen", 104954,
  1994, "02", "Enid Greene Waldholtz", 85507, 1994, "02", "Karen Shepherd", 66911, 1994, "02", "Merrill Cook", 34167,
  1994, "03", "Dixie Thompson", 61839, 1994, "03", "Bill Orton", 91505, 1994, "03", "Barbara Greenway", 1802,
  1996, "01", "James V. Hansen", 150126, 1996, "01", "Gregory J. Sanders", 65866, 1996, "01", "Randall Tolpinrud", 3787,
  1996, "02", "Merrill Cook", 129963, 1996, "02", "Ross Anderson", 100283, 1996, "02", "Arly H. Pedersen", 3070, 1996, "02", "Catherine Carter", 2981, 1996, "02", "David J. Murtha", 24,
  1996, "03", "Christopher Cannon", 106220, 1996, "03", "Bill Orton", 98178, 1996, "03", "Amy L. Lassen", 2341, 1996, "03", "John P. Langford", 270, 1996, "03", "Gerald Slothower", 706,
  1998, "01", "Gerard A. Arthus", 3070, 1998, "01", "Steve Beierlein", 49307, 1998, "01", "James V. Hansen", 109708,
  1998, "02", "Merrill Cook", 93718, 1998, "02", "Lily Eskelsen", 77198, 1998, "02", "Ken Larsen", 3998, 1998, "02", "Robert C. Lesh", 524, 1998, "02", "Arly H. Pedersen", 813, 1998, "02", "Brian E. Swim", 1390,
  1998, "03", "Kitty K. Burton", 9553, 1998, "03", "Chris Cannon", 100830, 1998, "03", "Will Christensen", 20720, 1998, "03", "Jeremy Friedbaum", 20
)
chk <- raw %>% group_by(year, district, candidate) %>% summarise(sum_votes = sum(votes), .groups = "drop") %>%
  inner_join(PRINTED_TOTALS, by = c("year", "district", "candidate"))
stopifnot(nrow(chk) == nrow(PRINTED_TOTALS))
bad <- chk %>% filter(sum_votes != total)
if (nrow(bad) > 0) { print(as.data.frame(bad)); stop("transcription does not tie to a printed district total -- see above") }
cat("All", nrow(chk), "candidate totals tie EXACTLY to their printed district total.\n")

for (y in years) save_long(finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ut_", y)), paste0("he_ut_", y))

## ---- shares files for every year + acceptance test ----------------------------------------------------------------------------
for (y in years) {
  long <- readRDS(file.path(LONG_DIR, sprintf("he_ut_%d.rds", y)))
  shares <- derive_shares(long) %>% transmute(state = "UTAH", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 29 UT counties")
}

sanity <- bind_rows(lapply(years, function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ut_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")
