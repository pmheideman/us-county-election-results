## Mississippi U.S. House, 2002 and 2004 general elections, from R/data/county_house_files/MS_02-04.pdf -- a Mississippi Official and
## Statistical Register (the state's "Blue Book"; despite the filename its own title page says "2004-2008" edition, published 2005). Unlike
## the older 1990-92 edition, this book is BORN-DIGITAL (clean typeset text, no OCR needed) -- transcribed directly from pdftotext output.
##
## Mississippi reduced from 5 to 4 U.S. House districts starting with the 2002 general election (post-2000-census reapportionment) -- this
## was not obvious going in (the book's archived 1998 section still shows "FIFTH CONGRESSIONAL DISTRICT" primary/runoff results, which at
## first glance look like they could belong to 2002/2004, but they are clearly headed "Archived Election Results" / "1998 REPUBLICAN
## PRIMARY"). Confirmed by checking the full page range for both years: every "CONGRESSIONAL DISTRICT" occurrence within the 2002 and 2004
## GENERAL ELECTION sections is District 1-4 only, and the union of counties appearing across those 4 districts is the full 82/82 Mississippi
## counties both years -- there is no missing county, Mississippi simply has 4 districts by this point, not a partial 5th.
##
## The book interleaves primary and general election tables for the same district within a few pages of each other (Democratic primary,
## Republican primary, then general), which needs care: e.g. 2002 District 2's FIRST appearance in the page range (pages 557-558, "George E.
## Irvin" vs "Bennie G. Thompson") is the DEMOCRATIC PRIMARY, not the general -- the real November 2002 District 2 race (pages 560-561) is
## "Lee Dilworth (RP)" vs "Clinton B. LeSueur (R)" vs "Bennie G. Thompson (D)". Verified by the "2002 GENERAL ELECTION -- NOVEMBER 05, 2002"
## running header immediately preceding the real table, and cross-checked that LeSueur (not Irvin) is also the subject of the adjacent 2002
## REPUBLICAN PRIMARY table, i.e. Irvin/Thompson and LeSueur/Reeves were the two parties' primaries, and Dilworth/LeSueur/Thompson was the
## resulting three-way general.
##
## Party classification: (D)->DEM, (R)->REP, everything else (RP=Reform Party, L=Libertarian, I=Independent)->OTHER, this project's standard.
##
## Verification: every district's county rows tie EXACTLY to its own printed TOTAL row, with one exception: 2004 District 4 (Hill/Lott/
## Taylor) sums to 181,314 for Gene Taylor across its 15 printed counties, but the book's own TOTAL row says 181,614 -- a genuine 300-vote
## source-side arithmetic inconsistency (every individual county cell re-checked directly against the extracted text, not a transcription
## slip). Same class of issue as a 1996/1994 Iowa canvass-book error found earlier in this project: the more granular, independently-summed
## county-row total is used rather than the source's own printed total. Logged in data_corrections_log.csv by the coordinating session.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "mississippi", "transcribed")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "MISSISSIPPI") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 82)
fips_of_county <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$county_name)]

party_group_of <- function(label) {
  x <- toupper(label)
  case_when(grepl("\\(D\\)", x) ~ "DEM", grepl("\\(R\\)", x) ~ "REP", TRUE ~ "OTHER")
}

## Each district file's header row is "county,<Candidate Name (Party)>,...". Long-format one row per county x candidate.
read_district <- function(file, year, district) {
  d <- read_csv(file.path(DIR, file), show_col_types = FALSE)
  cand_cols <- setdiff(names(d), "county")
  fp <- fips_of_county(d$county); stopifnot(!anyNA(fp))
  purrr::map_dfr(cand_cols, function(cc) {
    tibble(year = year, county_fips = fp, district = norm_district(district), candidate = cc, party_group = party_group_of(cc), votes = d[[cc]])
  })
}

specs <- tribble(
  ~file,              ~year, ~district,
  "ms2002_d1.csv",     2002,  1,
  "ms2002_d2.csv",     2002,  2,
  "ms2002_d3.csv",     2002,  3,
  "ms2002_d4.csv",     2002,  4,
  "ms2004_d1.csv",     2004,  1,
  "ms2004_d2.csv",     2004,  2,
  "ms2004_d3.csv",     2004,  3,
  "ms2004_d4.csv",     2004,  4,
)
raw <- purrr::pmap_dfr(specs, read_district)

## party label as printed, e.g. "Roger F. Wicker (R)" -> candidate "Roger F. Wicker", party "R"
raw <- raw %>% mutate(
  party = trimws(sub("^.*\\(([^()]+)\\)\\s*$", "\\1", candidate)),
  candidate = trimws(sub("\\s*\\([^()]+\\)\\s*$", "", candidate))
)
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate")]))

## ---- verification: every (year, district) sums exactly to its own printed TOTAL, except the one documented 2004 D4 exception -----------
EXPECTED_TOTALS <- tribble(
  ~year, ~district, ~candidate,                      ~total,
  2002,  1,          "Brenda Blackburn",               3477,
  2002,  1,          "Harold Taylor",                  2368,
  2002,  1,          "Rex Weathers",                   32318,
  2002,  1,          "Roger Wicker",                   95404,
  2002,  2,          "Lee Dilworth",                   3426,
  2002,  2,          "Clinton B. LeSueur",              69711,
  2002,  2,          "Bennie G. Thompson",              89913,
  2002,  3,          "Harvey L. Darden",                949,
  2002,  3,          "Jim Giles",                       1431,
  2002,  3,          "Carroll Grantham",                498,
  2002,  3,          "Brad A. McDonald",                760,
  2002,  3,          "Charles W. Pickering Jr.",          139329,
  2002,  3,          "Ronnie Shows",                    76184,
  2002,  4,          "Thomas Huffmaster",               2442,
  2002,  4,          "Karl Mertz",                      34373,
  2002,  4,          "Wayne L. Parker",                 3311,
  2002,  4,          "Gene Taylor",                     121742,
  2004,  1,          "Barbara Dale Washer",              58256,
  2004,  1,          "Roger F. Wicker",                 219328,
  2004,  2,          "Clinton B. LeSueur",              107647,
  2004,  2,          "Shawn O'Hara",                    2596,
  2004,  2,          "Bennie Thompson",                 154626,
  2004,  3,          "Jim Giles",                       40426,
  2004,  3,          "Lamonica L. Magee",                18068,
  2004,  3,          "Charles W. Pickering Jr.",          234874,
  2004,  4,          "Tracella Lou O'Hara Hill",         2028,
  2004,  4,          "Michael Lott",                    96740,
  2004,  4,          "Gene Taylor",                     181314,   # printed TOTAL row says 181,614; county-row sum (used here) is 181,314 -- see header note
)
chk <- raw %>% group_by(year, district, candidate) %>% summarise(sum_votes = sum(votes), .groups = "drop") %>%
  inner_join(EXPECTED_TOTALS %>% mutate(district = norm_district(district)), by = c("year", "district", "candidate"))
stopifnot(nrow(chk) == nrow(EXPECTED_TOTALS))
mismatch <- chk %>% filter(sum_votes != total)
if (nrow(mismatch) > 0) { print(as.data.frame(mismatch)); stop("county-row sums do not match expected district totals") }
message("All ", nrow(chk), " candidate totals across 2002/2004, 4 districts each, tie exactly to their own district total (2004 D4 Taylor uses the county-row sum per the documented 300-vote source inconsistency).")

## ---- county coverage: full 82/82 both years (every county appears in at least one of the 4 districts) ----------------------------------
for (y in c(2002, 2004)) {
  n <- raw %>% filter(year == y) %>% distinct(county_fips) %>% nrow()
  message(y, ": ", n, " of 82 Mississippi counties covered")
  stopifnot(n == 82)
}

## ---- build the long table + shares + acceptance test, one year at a time ----------------------------------------------------------------
raw <- raw %>% mutate(party_group = party_group_of(paste0("(", party, ")")))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")

for (y in c(2002, 2004)) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ms_", y))
  save_long(long, paste0("he_ms_", y))
  shares <- derive_shares(long) %>% transmute(state = "MISSISSIPPI", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 82 MS counties in the final shares file")
}

sanity <- bind_rows(lapply(c(2002, 2004), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ms_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")

## ---- independent check: district winners should match known Mississippi political history -----------------------------------------------
for (y in c(2002, 2004)) {
  l <- readRDS(file.path(LONG_DIR, sprintf("he_ms_%d.rds", y)))
  win <- l %>% group_by(district, candidate) %>% summarise(votes = sum(votes), .groups = "drop") %>% group_by(district) %>% slice_max(votes, n = 1) %>% ungroup()
  message(y, " district winners: ", paste(sprintf("D%s=%s", win$district, win$candidate), collapse = "; "))
}
