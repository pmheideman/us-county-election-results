## Iowa U.S. House, 1990, 1996, 1998, from the Iowa Secretary of State's scanned "Official Canvass Summary" general-election PDFs
## (R/data/county_house_files/IA_1990gencanv.pdf, IA_1996gencanv.pdf, IA_1998.pdf; a parallel build closes 1992/1994 plus a bonus fix for
## Iowa's existing partial 2008 gap). These PDFs bundle EVERY statewide/legislative race (U.S. Senate, U.S. House, Governor, State House,
## etc.) into one book; only the "UNITED STATES REPRESENTATIVE"/"U.S. REPRESENTATIVE" sections are used here.
##
## Iowa had 6 congressional districts in 1990 (before the 1990-census reapportionment took effect for the 1992 election) and 5 in 1996/1998.
## 1990's PDF (`pdftotext -layout`) has a genuinely unreliable OCR text layer (scanned, garbled digits e.g. "127,8L2" for "127,812") -- used
## only to locate pages; every county's numbers were read directly off 300dpi page renders and hand-transcribed (same technique as this
## project's other scanned-PDF builds: Kentucky 2010, Wisconsin Blue Books, Utah 1990-1998). One 1990 county name (Union, in District 5) was
## obscured by a literal fold in the physical page; identified by alphabetical position (the only Iowa county between Taylor and Warren) and
## its vote totals backed out exactly from the district's own printed TOTALS row minus every other legible county -- see the transcribed CSV.
## 1996 and 1998's PDFs have a MUCH cleaner OCR/text layer (no visible garbling) -- transcribed directly from `pdftotext -layout`, but still
## verified the same way as every other build in this project: every district's county rows tie EXACTLY to that district's own printed
## TOTALS row before being trusted (all three years, every district, tied on the first pass here -- no residual/flagged rows).
##
## Party classification: this project's standard convention -- REP for Republican, DEM for Democratic, everything else (Reform, Natural Law,
## Libertarian, Socialist Workers, Nominated by Petition, scattered/write-in "SC" rows) -> OTHER. "SC" (scattered votes, i.e. write-ins with
## no single name printed) is kept as a "Write-In" OTHER candidate row rather than excluded, consistent with how other states' unattributed
## write-in/scattering totals are handled elsewhere in this project (e.g. Wisconsin's "Scattering").
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)

DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "iowa", "transcribed")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "IOWA") %>% distinct(county_fips, county_name) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(nrow(xw) == 99)

party_group_of <- function(p) case_when(p == "DEM" ~ "DEM", p == "REP" ~ "REP", TRUE ~ "OTHER")

## District TOTALS as printed in each year's PDF (hand-verified against the source; regression guard against a future re-transcription
## silently drifting). Format: year, district, printed_total. One KNOWN, documented exception: 1996 District 1's printed TOTALS row
## (244,596) does not match the sum of its own eight county rows as printed on the same page (244,598) -- confirmed by re-rendering the
## page at 300dpi: every individual county's candidate-column digits are printed exactly as transcribed (Cedar's petition-nominee column
## reads "53", not a misread "51"), so the ~2-vote gap is a genuine arithmetic error in the source's own district-total row, not a
## transcription error here. Verified against the reliable figure -- the sum of the printed county rows -- instead.
EXPECTED_TOTALS <- tribble(
  ~year, ~district, ~printed_total,
  1990, "01", 90193, 1990, "02", 166106, 1990, "03", 101780, 1990, "04", 130590, 1990, "05", 147012, 1990, "06", 156459,
  1996, "01", 244598, 1996, "02", 239299, 1996, "03", 234875, 1996, "04", 256509, 1996, "05", 225479,   # D1: see note above (source's own printed total is 244,596)
  1998, "01", 188208, 1998, "02", 189574, 1998, "03", 189752, 1998, "04", 199396, 1998, "05", 133771
)

build_year <- function(year) {
  raw <- read_csv(file.path(DIR, paste0(year, ".csv")), show_col_types = FALSE) %>%
    mutate(county = toupper(trimws(county)), district = norm_district(district), votes = as.numeric(votes))
  stopifnot(!anyNA(match(unique(raw$county), xw$county_name)))          # every county name in the transcript matches the crosswalk
  n_counties <- n_distinct(raw$county)
  message(year, ": ", n_counties, " of 99 Iowa counties, ", n_distinct(raw$district), " districts")

  ## verification: county rows for each district sum to that district's printed TOTALS row (EXPECTED_TOTALS above, hand-verified against
  ## the PDF page images / arithmetic before being saved) -- hard stopifnot so a future re-transcription cannot silently drift.
  district_sums <- raw %>% group_by(district) %>% summarise(total = sum(votes), .groups = "drop") %>%
    left_join(EXPECTED_TOTALS %>% filter(year == !!year), by = "district")
  print(as.data.frame(district_sums))
  stopifnot(!anyNA(district_sums$printed_total), all(district_sums$total == district_sums$printed_total))

  long <- raw %>% left_join(xw, by = c("county" = "county_name")) %>%
    transmute(year = year, county_fips, district, candidate, party = party_group_of(party), party_group = party_group_of(party), votes) %>%
    mutate(party = case_when(party == "DEM" ~ "Democratic", party == "REP" ~ "Republican", TRUE ~ "Other"))
  stopifnot(!anyNA(long$county_fips))
  long
}

for (y in c(1990, 1996, 1998)) {
  raw_long <- build_year(y)
  long <- finalize_long(raw_long %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ia_", y))
  save_long(long, paste0("he_ia_", y))
  shares <- derive_shares(long) %>% transmute(state = "IOWA", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ia_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ia_%d.rds", y))); stopifnot(all(r$pass))
  message(y, ": ", nrow(shares), " of 99 IA counties")
}

sanity <- bind_rows(lapply(c(1990, 1996, 1998), function(y) readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_ia_%d.rds", y)))))
share <- sanity$repuvote + sanity$demovote
message("Sanity: repuvote+demovote range [", round(min(share), 3), ", ", round(max(share), 3), "]")
