## Arizona county-level U.S. House results, 1998 general election (extends 01u's 2000-2014 back one cycle).
##
## Source: Arizona Secretary of State's OFFICIAL CANVASS, 1998 General Election (Wayback copy of
##   https://apps.azsos.gov/election/1998/General/Canvass1998GE.pdf), linked from the archived
##   historical-results page https://azsos.gov/elections/voter-registration-historical-election-data/
##   historical-election-results-information. Unlike the 1990-1996 canvass PDFs (image-only scans),
##   the 1998 PDF has a real text layer, and prints every candidate x county vote count for each of
##   AZ's 6 districts, plus a TOTAL column. So the canvass itself IS a county-level source -- no
##   precinct summing needed -- and its own TOTAL column gives a hard internal check.
##
## Parsing notes:
##  * County column headers are font-garbled in the text layer ("$SDFKH" = "Apache", i.e. shifted by
##    a constant), so columns are mapped by POSITION: the canvass always lists the 15 counties
##    alphabetically (Apache ... Yuma) then TOTAL. Every candidate row must therefore have exactly 16
##    numeric-or-"---" tokens after the name (checked).
##  * "---" = the district does not touch that county (treated as 0). Split counties (Maricopa,
##    Pima, Pinal, Coconino, Mohave, Yavapai ...) are summed across districts by county.
##  * Candidate blocks are bounded by "the first non-candidate, non-blank line" after the district
##    header, NOT by a fixed row count or by EOF (the KY/OH "last section swallows the rest" lesson).
##  * Party: (DEM)/(REP) as printed. "(DEM) Benjamin H. Jankowski (Write-In)" (26 votes, CD3) is a
##    DEM-labelled write-in and follows the source label (-> DEM), same treatment as California's
##    "DEM (W/I)"; moving it to OTHER would change no county's share in the 3rd decimal. LBT/RPA/IND/
##    NONE -> OTHER.
##  * totalvote = all candidates incl. write-ins; the canvass prints no blank/overvote line for House.
##
## Output: R/output/elect_he_cty_az_1998.rds. Does NOT touch elect_cty_final.rds / the coverage
## tracker (serialized fold-in is done by the coordinating session).

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "arizona_historical", "1998")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
pdf <- file.path(RAW_DIR, "Canvass1998GE.pdf")
if (!file.exists(pdf) || file.size(pdf) == 0)
  system2("curl", c("-sL", "-m", "120", "-A", shQuote("Mozilla/5.0"), "-o", shQuote(pdf),
                    shQuote("https://web.archive.org/web/20201020063110id_/https://apps.azsos.gov/election/1998/General/Canvass1998GE.pdf")))
lines <- system2("pdftotext", c("-layout", shQuote(pdf), "-"), stdout = TRUE)

AZ_COUNTIES <- c("APACHE", "COCHISE", "COCONINO", "GILA", "GRAHAM", "GREENLEE", "LA PAZ", "MARICOPA",
                 "MOHAVE", "NAVAJO", "PIMA", "PINAL", "SANTA CRUZ", "YAVAPAI", "YUMA")

az_fips <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
                      delim = "\t", show_col_types = FALSE) %>%
  filter(state == "ARIZONA", !is.na(county_fips)) %>%
  distinct(county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
stopifnot(setequal(az_fips$county_name, AZ_COUNTIES))

## ---- parse the six House blocks --------------------------------------------------------------
NUM <- "(?:---|[0-9][0-9,]*)"
row_re <- paste0("^\\s*\\((\\w+)\\)\\s+(.+?)\\s+((?:", NUM, "\\s+){15}", NUM, ")\\s*$")   # name, then 15 counties + TOTAL
hdr_re <- "^\\s*U\\.S\\. REPRESENTATIVE IN CONGRESS - DISTRICT NO\\. ([0-9]+)\\s*$"

hdr_idx <- grep(hdr_re, lines)
stopifnot(length(hdr_idx) == 6)                                    # regex-drift guard: exactly 6 districts
to_n <- function(x) { x <- gsub(",", "", x); x[x == "---"] <- "0"; as.numeric(x) }

rows <- list()
for (h in hdr_idx) {
  dist <- as.integer(sub(hdr_re, "\\1", lines[h]))
  i <- h + 1
  while (i <= length(lines) && !nzchar(trimws(lines[i]))) i <- i + 1    # skip blank lines after header
  while (i <= length(lines) && grepl(row_re, lines[i], perl = TRUE)) {
    m <- regmatches(lines[i], regexec(row_re, lines[i], perl = TRUE))[[1]]
    vals <- to_n(strsplit(trimws(m[4]), "\\s+")[[1]])
    stopifnot(length(vals) == 16)
    rows[[length(rows) + 1]] <- tibble(district = dist, party_raw = m[2], candidate = trimws(m[3]),
                                       county = c(AZ_COUNTIES, "TOTAL"), votes = vals)
    i <- i + 1
  }
  ## whatever stopped the block must be a blank line / "* Elected" / next header, never an unparsed candidate-looking row
  if (i <= length(lines) && grepl("^\\s*\\(\\w+\\)", lines[i])) stop("unparsed candidate-like row: ", lines[i])
}
long <- bind_rows(rows)
cands <- distinct(long, district, party_raw, candidate)
message("Candidates parsed per district (expect 2,4,3,4,4,3 = 20 total): ",
        paste(table(cands$district), collapse = ","), " = ", nrow(cands))
stopifnot(as.vector(table(cands$district)) == c(2, 4, 3, 4, 4, 3))

## ---- internal check: 15 county columns must sum to the printed TOTAL column, per candidate ---------
chk <- long %>% group_by(district, candidate) %>%
  summarise(cty_sum = sum(votes[county != "TOTAL"]), printed = votes[county == "TOTAL"], .groups = "drop")
stopifnot(all(chk$cty_sum == chk$printed))
message("All ", nrow(chk), " candidates: sum of 15 county columns == printed TOTAL column (max diff 0)")

## ---- aggregate to county (sum across districts) ---------------------------------------------------
county_long <- long %>% filter(county != "TOTAL") %>%
  mutate(party = case_when(party_raw == "DEM" ~ "DEM", party_raw == "REP" ~ "REP", TRUE ~ "OTHER"))
stopifnot(!any(grepl("TOTAL|BLANK|OVER|UNDER", toupper(county_long$candidate))))   # pseudo-row guard

az98 <- county_long %>% group_by(county) %>%
  summarise(totalvote = sum(votes), demovote_n = sum(votes[party == "DEM"]),
            repuvote_n = sum(votes[party == "REP"]), .groups = "drop") %>%
  left_join(az_fips, by = c("county" = "county_name"))
stopifnot(nrow(az98) == 15, !anyNA(az98$county_fips), all(az98$totalvote > 0))

elect_he_cty_az_1998 <- az98 %>%
  transmute(state = "ARIZONA", year = 1998, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(cty_fips) %>%
  save_step("elect_he_cty_az_1998")

## ---- sanity output ---------------------------------------------------------------------------------
message("\nrows: ", nrow(elect_he_cty_az_1998), " (expect 15); repuvote+demovote range: ",
        paste(round(range(elect_he_cty_az_1998$demovote + elect_he_cty_az_1998$repuvote), 4), collapse = " - "))
message("Statewide House totals by party (sum of counties): D ", sum(az98$demovote_n), ", R ", sum(az98$repuvote_n),
        ", all ", sum(az98$totalvote))
print(elect_he_cty_az_1998)

## ---- Independent cross-check vs the archived precinct-level "stream" files (where downloaded) ---------
## Each county's <county>1998GEstream.txt is tab-delimited long form: office, candidate, (blank), candidate,
## precinct-or-"TOTAL", votes. For every candidate we compare (a) the file's own TOTAL row and (b) the sum of
## its precinct rows against the canvass county column above. Counties whose file was not retrievable from
## the Wayback Machine are listed and skipped, never guessed.
STREAM_DIRS <- c(APACHE = "Apache", COCHISE = "Cochise", COCONINO = "Coconino", GILA = "Gila", GRAHAM = "Graham",
                 GREENLEE = "Greenlee", `LA PAZ` = "LaPaz", MARICOPA = "Maricopa", MOHAVE = "Mohave",
                 NAVAJO = "Navajo", PIMA = "Pima", PINAL = "Pinal", `SANTA CRUZ` = "SantaCruz",
                 YAVAPAI = "Yavapai", YUMA = "Yuma")
last_name <- function(x) toupper(trimws(sub(",.*$", "", sub("^\\s*\\(\\w+\\)\\s*", "", x))))
canv_lastname <- function(x) toupper(sub("^.*\\s(\\S+?)(\\s*\\(Write-In\\))?\\s*$", "\\1", trimws(sub("\\s*\\*$", "", x)), perl = TRUE))

xres <- list(); skipped <- character()
for (cty in names(STREAM_DIRS)) {
  fs <- list.files(file.path(RAW_DIR, STREAM_DIRS[[cty]]), pattern = "stream\\.txt$", ignore.case = TRUE, full.names = TRUE)
  if (length(fs) == 0) { skipped <- c(skipped, cty); next }
  s <- read_tsv(fs[1], col_names = FALSE, col_types = cols(.default = col_character()), quote = "", show_col_types = FALSE)
  s <- s[grepl("^U\\.S\\. REPRESENTATIVE IN CONGRESS", s$X1), ]
  if (nrow(s) == 0) { skipped <- c(skipped, paste0(cty, " (no House rows)")); next }
  s <- s %>% mutate(district = as.integer(sub(".*DIST\\.?\\s*([0-9]+).*", "\\1", X1)),
                    last = last_name(X2), precinct = X5, votes = as.numeric(gsub(",", "", X6)))
  xres[[cty]] <- s %>% group_by(county = cty, district, last) %>%
    summarise(stream_total = sum(votes[precinct == "TOTAL"]), stream_prec_sum = sum(votes[precinct != "TOTAL"]), .groups = "drop")
}
if (length(xres) > 0) {
  x <- bind_rows(xres)
  canv <- long %>% filter(county != "TOTAL") %>% filter(votes > 0 | TRUE) %>%
    mutate(last = canv_lastname(candidate)) %>% select(county, district, last, canvass = votes)
  cmp <- x %>% left_join(canv, by = c("county", "district", "last"))
  cmp$canvass[is.na(cmp$canvass)] <- 0
  message("\nPrecinct-file cross-check: ", length(xres), " counties compared; skipped: ",
          if (length(skipped)) paste(skipped, collapse = ", ") else "none")
  message("  candidate-county cells compared: ", nrow(cmp),
          "; TOTAL-row mismatches vs canvass: ", sum(cmp$stream_total != cmp$canvass),
          "; precinct-sum mismatches vs canvass: ", sum(cmp$stream_prec_sum != cmp$canvass))
  print(cmp %>% filter(stream_total != canvass | stream_prec_sum != canvass), n = 40)
}
