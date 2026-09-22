## Kentucky, 2012/2014/2016: elect.ky.gov's text-file "by county" results disappeared after 2008
## -- everything under those filenames from 2012 on turned out to be voter-REGISTRATION
## statistics (same misleading labels, "By County"/"By Congressional District", as a real
## results file would have -- confirmed by content, not just filename). Real vote results for
## these years exist only as the state's official PDF ("<year>genresults.pdf" or similar,
## found via each year's own results-index page -- 2016's lives in a completely different
## document library path than every other year, `/results/2010-2019/Documents/` instead of
## `/SiteCollectionDocuments/...`, which is why it wasn't found initially).
##
## 2012/2014/2016's PDFs are born-digital (real text layer, `pdftotext -layout` extracts
## cleanly) with FULL county names -- no 4-letter-code collision problem at all this time. 2010's
## PDF, checked the same way, has ZERO extractable text (scanned/image-only) -- would need OCR,
## bringing back the same reliability problems fought through on the America Votes PDFs. Not
## attempted here; 2010 remains open, a separate decision.
##
## Same as 2000-2008: no party letters shown anywhere in the PDF table, just candidate names --
## party comes from the same Wikipedia-infobox lookup (ky_wikipedia_party_lookup.py, extended to
## include these years).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "kentucky_2010s")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

KY_2010s_PDFS <- tribble(
  ~year, ~url,
  2012, "https://elect.ky.gov/SiteCollectionDocuments/Election%20Results/2010-2019/2012/2012genresults.pdf",
  2014, "https://elect.ky.gov/SiteCollectionDocuments/Election%20Results/2010-2019/2014/2014%20General%20Election%20Results.pdf",
  2016, "https://elect.ky.gov/results/2010-2019/Documents/2016%20General%20Election%20Results.pdf"
)

fetch_and_extract <- function(year, url) {
  pdf_path <- file.path(RAW_DIR, paste0(year, ".pdf"))
  txt_path <- file.path(RAW_DIR, paste0(year, ".txt"))
  if (!file.exists(pdf_path) || file.size(pdf_path) == 0) {
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(pdf_path), shQuote(url)))
  }
  if (!file.exists(txt_path) || file.size(txt_path) == 0) {
    system2("pdftotext", c("-layout", shQuote(pdf_path), shQuote(txt_path)))
  }
  txt_path
}

KY_COUNTIES <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% filter(state == "KENTUCKY") %>% distinct(county_name, county_fips)
KY_COUNTY_SET <- KY_COUNTIES$county_name  # already uppercase

last_name_of <- function(x) toupper(gsub("[^A-Za-z]", "", sapply(strsplit(trimws(x), "\\s+"), function(w) w[length(w)])))

parse_ky_pdf <- function(path, year) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  dist_idx <- grep("^\\s*\\d+(st|nd|rd|th)\\s+Congressional District\\s*$", lines, ignore.case = TRUE)
  if (length(dist_idx) == 0) return(tibble())

  all_rows <- list()
  for (di in seq_along(dist_idx)) {
    start <- dist_idx[di]
    end <- if (di < length(dist_idx)) dist_idx[di + 1] - 1 else length(lines)
    district <- as.integer(gsub("\\D.*", "", trimws(lines[start])))
    block <- lines[start:end]

    last_name_header <- NULL
    for (li in seq_along(block)) {
      line <- block[li]
      m <- regmatches(line, regexec("^\\s*([A-Za-z][A-Za-z .'-]*?)\\s+((?:[\\d,]+\\s*)+)$", line, perl = TRUE))[[1]]
      if (length(m) != 3) next
      name <- toupper(trimws(m[2]))
      ## STOP (not skip) at this district's own "Total Votes" line, once we're past the header --
      ## the LAST congressional district in a PDF has no next "Congressional District" marker to
      ## bound its block, so without this it silently runs into whatever comes next in the file
      ## (State Senator, State Representative, ... races) and sums their votes in as if they were
      ## more counties in the US House race, because those races' county rows pass the same
      ## "recognized KY county name" filter. Confirmed: this is exactly what inflated several
      ## 2016 district-6 counties (e.g. Jackson County's total nearly doubled, pulling in a
      ## State Senate race's numbers) until this fix.
      if (name == "TOTAL VOTES" && !is.null(last_name_header)) break
      if (name == "TOTAL VOTES") next
      if (!name %in% KY_COUNTY_SET) next  # page-break junk, repeated headers, etc. -- not a county row

      if (is.null(last_name_header)) {
        ## The candidate last-name row is the nearest preceding non-blank line that ISN'T
        ## itself a county row (captured once per district; page-break repeats of this same
        ## header later in the block are harmless since we don't re-capture).
        pj <- li - 1
        while (pj > 0 && trimws(block[pj]) == "") pj <- pj - 1
        if (pj > 0) last_name_header <- strsplit(trimws(block[pj]), "\\s{2,}")[[1]]
      }
      if (is.null(last_name_header)) next

      vote_toks <- strsplit(trimws(m[3]), "\\s+")[[1]]
      votes <- suppressWarnings(as.numeric(gsub(",", "", vote_toks)))
      if (length(votes) != length(last_name_header) || any(is.na(votes))) next

      all_rows[[length(all_rows) + 1]] <- tibble(
        year = year, district = district, county_name = name,
        last_name = last_name_of(last_name_header), votes = votes
      )
    }
  }
  if (length(all_rows) == 0) return(tibble())
  bind_rows(all_rows)
}

ky_2010s_raw <- pmap_dfr(KY_2010s_PDFS, function(year, url) {
  parse_ky_pdf(fetch_and_extract(year, url), year)
})

message("KY 2010s (2012/2014/2016) raw rows parsed: ", nrow(ky_2010s_raw))

party_lookup <- read_csv(file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states",
                                    "kentucky_2000s", "ky_wiki_party_lookup.csv"), show_col_types = FALSE)

ky_2010s_raw <- ky_2010s_raw %>%
  left_join(party_lookup %>% select(year, district, last_name, party), by = c("year", "district", "last_name"))

unmatched <- ky_2010s_raw %>% filter(is.na(party)) %>% distinct(year, district, last_name)
if (nrow(unmatched) > 0) {
  message(nrow(unmatched), " (year, district, last_name) combos did not match the Wikipedia party lookup (likely minor/write-in candidates not in the infobox -- treated as OTHER):")
  print(unmatched)
}
ky_2010s_raw <- ky_2010s_raw %>% mutate(party = coalesce(party, "OTHER"))

elect_he_cty_ky_2010s <- ky_2010s_raw %>%
  group_by(year, county_name, party) %>% summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, county_name) %>%
  summarise(
    totalvote = sum(votes, na.rm = TRUE),
    demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
    repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  inner_join(KY_COUNTIES, by = "county_name") %>%
  filter(totalvote > 0) %>%
  transmute(
    state = "KENTUCKY", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_ky_2010s")

message("KY 2010s House county-level rows built: ", nrow(elect_he_cty_ky_2010s), " (of possible ", 3 * 120, ")")
print(table(elect_he_cty_ky_2010s$year))

sanity <- elect_he_cty_ky_2010s$repuvote + elect_he_cty_ky_2010s$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")

## ---- Cross-check 2016 against MEDSL ----
medsl_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))
check <- elect_he_cty_ky_2010s %>% filter(year == 2016) %>%
  inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_ky", "_medsl")) %>%
  mutate(diff = abs(repuvote_ky - repuvote_medsl))
message("KY 2016 vs MEDSL cross-check: ", nrow(check), " counties matched, max diff = ",
        round(max(check$diff, na.rm = TRUE), 4), ", mean diff = ", round(mean(check$diff, na.rm = TRUE), 5))

## ---- Fold into the master panel and the combined KY file ----
elect_he_cty_ky_all <- bind_rows(
  readRDS(file.path(OUTPUT_DIR, "elect_he_cty_ky.rds")),
  elect_he_cty_ky_2010s
) %>% distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  save_step("elect_he_cty_ky")

elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
  bind_rows(elect_he_cty_ky_2010s %>% filter(year < 2016) %>% select(-state)) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated with KY 2012/2014 (2016 left to MEDSL). Total rows now: ", nrow(elect_cty_final))
