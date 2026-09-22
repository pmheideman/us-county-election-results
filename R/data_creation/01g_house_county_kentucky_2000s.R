## Kentucky, 2000-2008: elect.ky.gov's "by county" general-election files for this era use a
## transposed layout -- counties as column headers (4-letter truncated codes, wrapped 6 per
## block), candidates as rows -- and crucially DON'T show party letters anywhere in the block,
## just candidate full names. Party comes from a separate lookup built by
## ky_wikipedia_party_lookup.py (run first -- reads Wikipedia's own "<year> United States House
## of Representatives elections in Kentucky" articles' Infobox fields, matched here by
## (year, district, candidate last name)).
##
## Confirmed file URLs (NOT guessable from a pattern -- found one at a time via each year's
## results-index page, same as the 1990s files in 01f):
##   2000: 2000-2009/2000/00Gen_Statewidebycounty.txt
##   2002: 2000-2009/2002/General%20Election/2002statebycounty.txt
##   2004: 2000-2009/2004/General%20Election/2004statebyCOUNTY.txt
##   2006: 2000-2009/2006/General%20Election/STATEwidebycounty.txt
##   2008: 2000-2009/2008/General%20Election/STATEwide%20by%20candidate%20by%20county%20gen%2008.txt
## 2010 (precinct-file only), 2012/2014 (their "by county" links turned out to be voter-
## registration-stats files, not election results -- same trap as 2000's initial
## "congressionalregstat" red herring), and 2016 (index page had no file links at all) are NOT
## covered here -- separate, harder follow-ups.
##
## County-code collision, RESOLVED (not dropped): codes are the first 4 letters of the county
## name, and two pairs collide -- GREE could be Green or Greenup, MCCR could be McCracken or
## McCreary. Two independent fixes, both needed for full 120/120 coverage:
##  (1) It turns out Kentucky's own system already avoids the collision for the LARGE member of
##      each pair by using a different, non-obvious code in whichever district it actually
##      appears in (Greenup = "GREU" in district 4; McCracken = "MCCK" in district 1) -- found by
##      checking why the magnitude-based resolution in (2) below only ever matched the SMALL
##      member; these two codes need no disambiguation, just an explicit mapping (SPECIAL_CODES).
##  (2) The remaining "GREE"/"MCCR" occurrences (district 2 and 5 respectively, every year) are
##      genuinely the small member of each pair every time -- resolved by comparing each
##      occurrence's observed vote total, in log-space, against the two candidate counties'
##      typical size. Reliable because the size gap is large (Green ~12k pop vs Greenup ~36k;
##      McCracken ~65k vs McCreary ~17k) with zero overlap in observed total-vote range across
##      every year checked (1990-2008, both from our own unambiguous 1990s KY House data and
##      from MEDSL's presidential totals).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kentucky_2000s")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

KY_2000s_FILES <- tribble(
  ~year, ~url,
  2000, "2000-2009/2000/00Gen_Statewidebycounty.txt",
  2002, "2000-2009/2002/General%20Election/2002statebycounty.txt",
  2004, "2000-2009/2004/General%20Election/2004statebyCOUNTY.txt",
  2006, "2000-2009/2006/General%20Election/STATEwidebycounty.txt",
  2008, "2000-2009/2008/General%20Election/STATEwide%20by%20candidate%20by%20county%20gen%2008.txt"
)

fetch_ky_file <- function(rel_url) {
  dest <- file.path(RAW_DIR, gsub("[/%]", "_", rel_url))
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0("https://elect.ky.gov/SiteCollectionDocuments/Election%20Results/", rel_url)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

## Ambiguous 4-letter county codes -- see header note. RESOLVED below (not dropped) using
## county-size magnitude: Green (pop ~11k) vs Greenup (~36k) and McCracken (~65k, Paducah) vs
## McCreary (~17k) differ by 2-4x with zero overlap in observed total-vote range across
## 1990-2008 (checked both from our own unambiguous 1990s KY House data, which lists full county
## names not codes, and from MEDSL's presidential totals for 2000/2004/2008) -- so each
## occurrence of the ambiguous code can be assigned to whichever real county's typical vote
## count it's closer to in log-space, per (year, district) row rather than a single fixed
## mapping (confirmed both members of a pair can appear in the same year under different
## districts, since they're unrelated real counties that happen to share 4 letters).
AMBIGUOUS_CODES <- c("GREE", "MCCR")
AMBIGUOUS_CODE_CANDIDATES <- list(GREE = c("GREEN", "GREENUP"), MCCR = c("MCCRACKEN", "MCCREARY"))

## Resolved further, after the log-space disambiguation below turned up that GREE/MCCR only
## ever resolved to the SMALL member of each pair (Green, McCreary) -- checked why: turns out
## Kentucky's own system uses a genuinely different, non-obvious 4-letter code for the large
## member of each pair in the district where it actually appears (Greenup = "GREU" in district
## 4; McCracken = "MCCK" in district 1) rather than truncating to the same 4 letters and letting
## them collide. Confirmed identical across all 5 years (2000/2002/2004/2006/2008). These need
## no disambiguation at all -- just an explicit code->county mapping, since they never collide
## with anything.
SPECIAL_CODES <- c(GREU = "GREENUP", MCCK = "MCCRACKEN")

parse_ky_2000s_file <- function(path, year) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  ## These are combined "all offices, by county" files (President, US House, Commonwealth
  ## Attorney, State Senate, ...), not US-House-only -- a block must be bounded by the next
  ## OFFICE marker of ANY code, not just the next A04 one, or the last US House district's block
  ## silently swallows everything after it in the file (confirmed: happened for 2000, where
  ## "district 6" absorbed every down-ballot race through EOF, producing 1600+ bogus
  ## "candidates" that don't exist in any real congressional race).
  any_office_idx <- grep("OFFICE:", lines)
  office_idx <- grep("OFFICE:\\s*A04/\\d+/000", lines)
  if (length(office_idx) == 0) return(tibble())

  all_rows <- list()
  for (oi in seq_along(office_idx)) {
    start <- office_idx[oi]
    next_any <- any_office_idx[any_office_idx > start]
    end <- if (length(next_any) > 0) min(next_any) - 1 else length(lines)
    district <- as.integer(gsub(".*A04/(\\d+)/000.*", "\\1", lines[start]))
    block <- lines[(start + 1):end]

    ## Split into chunks separated by blank lines; a chunk whose first line has county codes
    ## ("*XXXX" tokens) is real county data, anything else (the trailing grand-total chunk, or
    ## stray blank chunks) is skipped.
    is_blank <- trimws(block) == ""
    chunk_id <- cumsum(is_blank)
    chunk_id[is_blank] <- NA
    chunks <- split(block[!is_blank], chunk_id[!is_blank])

    for (chunk in chunks) {
      if (length(chunk) < 2 || !grepl("\\*[A-Z]{3,6}", chunk[1])) next  # not a county-code header
      codes <- regmatches(chunk[1], gregexpr("\\*[A-Z]{3,6}", chunk[1]))[[1]]
      codes <- gsub("\\*", "", codes)
      for (row in chunk[-1]) {
        ## Name is everything before the run of comma-formatted numbers at the end of the line.
        m <- regmatches(row, regexec("^(.*?)\\s+((?:[\\d,]+\\s*)+)$", row, perl = TRUE))[[1]]
        if (length(m) != 3) next
        name <- trimws(m[2])
        vote_toks <- strsplit(trimws(m[3]), "\\s+")[[1]]
        votes <- suppressWarnings(as.numeric(gsub(",", "", vote_toks)))
        if (length(votes) != length(codes) || any(is.na(votes))) next
        all_rows[[length(all_rows) + 1]] <- tibble(
          year = year, district = district, county_code = codes,
          candidate = name, votes = votes
        )
      }
    }
  }
  if (length(all_rows) == 0) return(tibble())
  bind_rows(all_rows)
}

ky_2000s_raw <- pmap_dfr(KY_2000s_FILES, function(year, url) {
  parse_ky_2000s_file(fetch_ky_file(url), year)
})

message("KY 2000s raw rows parsed: ", nrow(ky_2000s_raw))

## ---- Attach party via the Wikipedia lookup, matched on (year, district, last name) ----
party_lookup <- read_csv(file.path(RAW_DIR, "ky_wiki_party_lookup.csv"), show_col_types = FALSE)

last_name_of <- function(x) toupper(gsub("[^A-Za-z]", "", sapply(strsplit(trimws(x), "\\s+"), function(w) w[length(w)])))

ky_2000s_raw <- ky_2000s_raw %>%
  mutate(last_name = last_name_of(candidate)) %>%
  left_join(party_lookup %>% select(year, district, last_name, party), by = c("year", "district", "last_name"))

unmatched <- ky_2000s_raw %>% filter(is.na(party)) %>% distinct(year, district, candidate)
if (nrow(unmatched) > 0) {
  message("WARNING: ", nrow(unmatched), " (year, district, candidate) combos did not match the Wikipedia party lookup:")
  print(unmatched)
}
ky_2000s_raw <- ky_2000s_raw %>% mutate(party = coalesce(party, "OTHER"))  # unmatched -> other, not silently dropped from totalvote

## ---- County code -> FIPS, built from the first 4 letters of each real KY county name ----
ky_all_counties <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% filter(state == "KENTUCKY") %>% distinct(county_name, county_fips) %>%
  mutate(county_code = substr(county_name, 1, 4))

## ---- Resolve the ambiguous codes by matching each (year, district) row's total votes against
## the two candidate real counties' typical size, in log-space (see header note for why this is
## reliable: the size gap is 2-4x with zero overlap across every year checked).
countypres_raw <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
)
pres_reference <- countypres_raw %>%
  filter(state == "KENTUCKY", county_name %in% unlist(AMBIGUOUS_CODE_CANDIDATES)) %>%
  distinct(year, county_name, totalvotes) %>%
  group_by(county_name) %>% summarise(ref_votes = mean(totalvotes, na.rm = TRUE), .groups = "drop")

## Independent cross-check using our own unambiguous 1990-1998 KY House data (full county names,
## not codes) -- confirms the same ranking/gap holds in actual House (not just presidential)
## turnout, one election type closer to what we're actually disambiguating.
ky_1990s_reference <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ky.rds")) %>%
  filter(year <= 1998) %>%
  left_join(ky_all_counties, by = c("cty_fips" = "county_fips")) %>%
  filter(county_name %in% unlist(AMBIGUOUS_CODE_CANDIDATES)) %>%
  group_by(county_name) %>% summarise(ref_votes_1990s = mean(totalvote, na.rm = TRUE), .groups = "drop")

message("Reference sizes for disambiguation (presidential mean 2000-2008 | 1990s House mean):")
print(pres_reference %>% left_join(ky_1990s_reference, by = "county_name"))

resolve_ambiguous_code <- function(code, observed_total) {
  candidates <- AMBIGUOUS_CODE_CANDIDATES[[code]]
  refs <- pres_reference$ref_votes[match(candidates, pres_reference$county_name)]
  candidates[which.min(abs(log(observed_total) - log(refs)))]
}

ambiguous_rows <- ky_2000s_raw %>%
  filter(county_code %in% AMBIGUOUS_CODES) %>%
  group_by(year, district, county_code) %>% summarise(observed_total = sum(votes), .groups = "drop") %>%
  rowwise() %>%
  mutate(resolved_county = resolve_ambiguous_code(county_code, observed_total)) %>%
  ungroup()

message("\nAmbiguous-code occurrences resolved (", nrow(ambiguous_rows), " total):")
print(ambiguous_rows)

## Attach the resolved real county name onto each raw row (only for ambiguous codes; everything
## else already has a clean 1:1 code->county already handled via ky_all_counties below).
ky_2000s_raw <- ky_2000s_raw %>%
  left_join(ambiguous_rows %>% select(year, district, county_code, resolved_county),
            by = c("year", "district", "county_code")) %>%
  mutate(county_code = coalesce(resolved_county, county_code)) %>%
  select(-resolved_county)

## Now county_code is either a clean 4-letter code (unambiguous counties) or a resolved full
## name (the 4 previously-ambiguous ones) -- build a lookup covering both.
ky_lookup <- bind_rows(
  ky_all_counties %>% filter(!county_code %in% AMBIGUOUS_CODES) %>% select(county_code, county_fips),
  ky_all_counties %>% filter(county_name %in% unlist(AMBIGUOUS_CODE_CANDIDATES)) %>%
    transmute(county_code = county_name, county_fips),
  ky_all_counties %>% filter(county_name %in% SPECIAL_CODES) %>%
    transmute(county_code = names(SPECIAL_CODES)[match(county_name, SPECIAL_CODES)], county_fips)
)

elect_he_cty_ky_2000s <- ky_2000s_raw %>%
  group_by(year, county_code, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, county_code) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(ky_lookup, by = "county_code") %>%
  filter(totalvote > 0) %>%
  transmute(
    state = "KENTUCKY", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ky_2000s")

message("KY 2000s House county-level rows built: ", nrow(elect_he_cty_ky_2000s), " (of possible ",
        5 * 120, " = 5 years x all 120 KY counties, ambiguous codes now resolved not dropped)")
print(table(elect_he_cty_ky_2000s$year))

sanity <- elect_he_cty_ky_2000s$repuvote + elect_he_cty_ky_2000s$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Fold into the master panel (append to the existing KY 1990-1998 table's output file) ----
elect_he_cty_ky_all <- bind_rows(
  readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ky.rds")),
  elect_he_cty_ky_2000s
) %>% distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  save_step("elect_he_cty_ky")

elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ky_2000s %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with KY 2000-2008. Total rows now: ", nrow(elect_cty_final))
