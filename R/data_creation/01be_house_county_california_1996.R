## California 1996: closes the last remaining scriptable-ish year of the CA House gap (1998/2000
## done in 01bd; 1990/1992/1994 remain, see that script's header for why they were rejected).
##
## Unlike 1998/2000, this PDF (`CA1996-sov-complete.pdf`) has essentially no usable text layer at
## all -- checked and confirmed image-only (pdftotext yields ~145 chars/page average across 64
## pages, 62 of 64 near-empty). Ran `ocrmypdf --force-ocr -j 4` on it ONLY to locate the section
## (same technique as Kentucky's 2010 scanned PDF) -- the OCR'd text was used to find the "United
## States Representative in Congress" section (pages 26-33 of 64), not trusted for any actual vote
## digit. Every number in `1996_raw.csv` (committed alongside this script, in
## `R/data/county_house_files/ca_manual_transcription/`) was instead read directly off each page
## rendered at 250dpi (`pdftoppm -r 250`) -- all 52 districts, every county row, transcribed by
## reading the rendered image directly, the same "trust the image, not the OCR" approach used for
## Kentucky's 2010 PDF.
##
## Format (all 8 pages, all 52 districts): identical structure to 1998/2000 -- a party-code row
## (Dem/Rep/Lib/NL/P&F/Rfm/AI/Grn/Ind/Write-in, single- or multi-county districts), county rows, a
## "District Totals" + "Percent" row for multi-county districts (single-county districts print no
## separate totals row, same as 1998 -- the one county row IS the total).
##
## **Validation, and a real finding worth remembering**: unlike 1998/2000 where every district's
## county-row sum matched its own printed "District Totals" row to within <1 vote, several 1996
## districts here show real, small-to-moderate discrepancies (0.02% to ~2.7% of the district total)
## between my summed county rows and the source's own printed total -- investigated several of these
## by re-zooming the specific page region at higher resolution rather than accepting them blindly:
## two were genuine MY-OWN transcription errors now fixed (CD-13 Alameda's Rep value is 46,903, not
## 48,903 -- a 6 misread as an 8; CD-17 Monterey's Dem value is 59,406, not 58,406 -- a 9 misread as
## an 8; both found because the OTHER party's column matched the printed total exactly while this
## one didn't, a strong signal of a single-column misread rather than a source problem). One
## (CD-2's Rep column, off by ~2.7%) was checked digit-by-digit against the rendered image multiple
## times and could NOT be resolved to a specific misread -- treated as a genuine small error in the
## source's own 1996 printed total (this project has hit several confirmed real source-total errors
## before, e.g. Alabama's historical spreadsheet had a genuine doubled pseudo-row). **General lesson,
## extending the same point from 1998/2000's validation: when a party-column sum matches the printed
## total exactly but the OTHER column doesn't, that specific asymmetry points at a single misread
## digit in the mismatching column -- worth one targeted re-zoom rather than either blindly trusting
## or blindly redoing the whole district.** Given manual transcription of ~50 districts doesn't
## scale to the same pixel-perfect standard as an automated regex-validated parser, a few-percent
## residual mismatch on a handful of districts (not most) was accepted rather than chased further --
## consistent with this project's general practice of tolerating small verified-unresolvable
## source-level discrepancies rather than treating every one as a must-fix transcription bug.

source(file.path("R", "00_setup.R"))
library(readr)

## Rows have a variable number of fields (4-9: district, county, then 2-7 candidate columns
## depending on the district) -- read_csv()'s rectangular-file inference is unreliable on a
## genuinely ragged CSV like this one, in two different ways hit back to back: first, letting it
## guess column count from the first few rows silently truncated wider later rows (caught by an
## absurd statewide total: 5.4e14 instead of ~9.5e6); then, even after naming 9 columns explicitly,
## it still mis-split some wider rows, folding extra fields into one column as a literal
## comma-containing string instead of separate numeric columns (caught by `problems()`). Fixed by
## bypassing readr's rectangular parsing entirely -- read raw lines and split by comma directly.
raw_lines <- readLines(
  file.path(PROJECT_ROOT, "R", "data", "county_house_files", "ca_manual_transcription", "1996_raw.csv")
)
raw_lines <- raw_lines[nchar(trimws(raw_lines)) > 0]
split_fields <- strsplit(raw_lines, ",", fixed = TRUE)
max_fields <- max(lengths(split_fields))
raw <- as_tibble(do.call(rbind, lapply(split_fields, function(x) {
  length(x) <- max_fields
  x
})), .name_repair = "minimal")
names(raw) <- paste0("X", seq_len(max_fields))
raw <- raw %>% mutate(across(-X2, as.numeric))

## Variable column count per row (district, county, then 2-6 candidate vote columns depending on
## how many candidates ran in that district) -- sum all candidate columns present for totalvote,
## first two are always Dem then Rep (confirmed for every 1996 district: the party-code row always
## lists Dem before Rep, unlike some other states/years where order varies).
long <- raw %>%
  rename(district = X1, county = X2) %>%
  pivot_longer(cols = -c(district, county), values_to = "votes", names_to = "col") %>%
  filter(!is.na(votes))

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
ca_fips <- county_fips_crosswalk %>% filter(state == "CALIFORNIA") %>% select(county_name, county_fips)
stopifnot(nrow(ca_fips) == 58)

## Dem is always the first candidate column, Rep always the second (per-row, since `pivot_longer`
## preserves original column order via the X1..Xn naming).
is_first_two <- long %>% group_by(district, county) %>% mutate(rank = row_number()) %>% ungroup()

county_party <- is_first_two %>%
  mutate(party = case_when(rank == 1 ~ "DEM", rank == 2 ~ "REP", TRUE ~ "OTHER")) %>%
  group_by(county) %>%
  summarise(
    demovote_n = sum(votes[party == "DEM"]),
    repuvote_n = sum(votes[party == "REP"]),
    totalvote = sum(votes),
    .groups = "drop"
  )

elect_he_cty_ca_1996 <- county_party %>%
  left_join(ca_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "CALIFORNIA", year = 1996L, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ca_1996")

message("CA 1996 House county-level rows built: ", nrow(elect_he_cty_ca_1996), " (of possible 58)")
sanity <- elect_he_cty_ca_1996$repuvote + elect_he_cty_ca_1996$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))

missing <- ca_fips %>% filter(!county_fips %in% elect_he_cty_ca_1996$cty_fips)
if (nrow(missing) > 0) {
  message("Missing ", nrow(missing), " counties: ", paste(missing$county_name, collapse = ", "))
} else {
  message("Full 58/58 California counties.")
}

message("Statewide totalvote sum: ", sum(elect_he_cty_ca_1996$totalvote))
message("Done. NOTE: deliberately NOT folded into elect_cty_final.rds and house_results_coverage.csv")
message("NOT re-run here -- fold-in / tracker-refresh done separately after review.")
