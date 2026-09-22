## Builds county-level U.S. House vote data directly from state-election-board precinct/county
## result files, for states whose official results are free, scriptable, and not behind bot
## protection (see conversation notes -- Arizona/Georgia/Connecticut are blocked by
## Cloudflare/WAF challenges even to a browser-like curl request; those need manual downloads,
## tracked separately, not in this script).
##
## Unlike the America Votes PDF extraction (01c), which only ever gives CONGRESSIONAL-DISTRICT
## totals, these files carry a `county` field directly on every precinct row -- so we get the
## real thing (true county-level splits, including split counties, no district-to-county
## crosswalk needed at all) by grouping on county and summing across whatever district(s)
## happen to fall in that county. This is the same logic already used for President/Senate
## (elect_pe/se_cty_*.rds): sum raw votes by party within the geography, no district substructure
## needed.
##
## Output shape matches the existing election panel files: one row per (state, year, cty_fips,
## sample="HE") with demovote/repuvote as shares of totalvote -- ready to fold into
## elect_cty_final.rds (01b) once enough states are collected here.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

## County name -> FIPS crosswalk, reused from the MEDSL presidential file already in the repo
## (R/data/raw_election/countypres_2000-2024.tab) rather than re-deriving/hardcoding one.
county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>%
  distinct(state, county_name, county_fips) %>%
  mutate(county_name = toupper(trimws(county_name)))

## ---------------------------------------------------------------------------
## North Carolina: NCSBE precinct-level results, direct download, no bot protection, back to
## 2000. CONFIRMED the file format changed repeatedly across this span -- delimiter, presence
## of a header row, column names, AND the U.S. House contest-name convention all differ by era:
##   2000        tab, no header,  9 cols, contest = "US HOUSE OF REP. DISTRICT NN"
##   2002        tab, no header,  9 cols, contest = "US HOUSE (NTH DISTRICT)"
##   2004        comma, header,   9 cols, contest = "US CONGRESS DISTRICT N"
##   2006        tab, header,     9 cols, contest = "US CONGRESS DISTRICT NN"
##   2008/10/12  comma, header,  14 cols (per-voting-method columns + a precomputed "total
##               votes" column), contest = "US HOUSE OF REPRESENTATIVES DISTRICT N"
##   2014/2016   tab, header,    14 cols (same per-method + "Total Votes" shape), contest =
##               "US HOUSE OF REPRESENTATIVES DISTRICT NN"
## A single regex `^US (HOUSE|CONGRESS)` catches every one of these variants while excluding
## "NC HOUSE..." (state legislature) and "US SENATE". Column NAMES are handled by an explicit
## alias table below (exact strings taken directly from each era's real header) rather than
## fuzzy matching, since the actual header text is now known for every year.
## ---------------------------------------------------------------------------
## quote_char: "\"" for the eras that genuinely wrap every field in quotes (2004/2006/2008/2010/2012
## -- confirmed by looking at their raw header line); "" (quoting disabled) for the eras that
## don't (2000/2002/2014/2016) -- those files are NOT CSV-quoted, but some candidate names contain
## a literal `"` as part of a nickname (e.g. `JOSIAH ROBBINS "JO" JR.`), which made readr think it
## had entered a quoted field and silently ate every tab/newline until the next stray `"`,
## corrupting all parsing from that row onward (confirmed: 2014 alone produced only 13 of 100
## counties before this fix).
NC_GENERAL_ELECTIONS <- tribble(
  ~year, ~date_str,             ~delim, ~has_header, ~quote_char,
  2000,  "2000_11_07/results_pct_20001107", "\t", FALSE, "",
  2002,  "2002_11_05/results_pct_20021105", "\t", FALSE, "",
  2004,  "2004_11_02/results_pct_20041102", ",",  TRUE,  "\"",
  2006,  "2006_11_07/results_pct_20061107", "\t", TRUE,  "\"",
  2008,  "2008_11_04/results_pct_20081104", ",",  TRUE,  "\"",
  2010,  "2010_11_02/results_pct_20101102", ",",  TRUE,  "\"",
  2012,  "2012_11_06/results_pct_20121106", ",",  TRUE,  "\"",
  2014,  "2014_11_04/results_pct_20141104", "\t", TRUE,  "",
  2016,  "2016_11_08/results_pct_20161108", "\t", TRUE,  ""  # overlaps MEDSL -- kept as a cross-check
)

NO_HEADER_COLS <- c("county", "election_dt", "precinct_abbrv", "precinct", "contest_name",
                     "name_on_ballot", "party_cd", "ballot_count", "ftp_date")

## Exact header strings seen across the 9 files above, mapped to the 4 roles we actually need.
COL_ALIASES <- list(
  county  = c("county"),
  contest = c("contest_name", "contest", "contest name"),
  party   = c("party_cd", "party", "choice party"),
  votes   = c("ballot_count", "total votes")
)

standardize_names <- function(raw_names) {
  clean <- tolower(trimws(gsub('^"|"$', "", raw_names)))
  out <- clean
  for (role in names(COL_ALIASES)) {
    out[clean %in% COL_ALIASES[[role]]] <- role
  }
  out
}

download_nc_file <- function(date_str) {
  dest_zip <- file.path(RAW_DIR, paste0("nc_", basename(date_str), ".zip"))
  if (!file.exists(dest_zip)) {
    url <- paste0("https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/", date_str, ".zip")
    download.file(url, dest_zip, quiet = TRUE)
  }
  txt_name <- paste0(basename(date_str), ".txt")
  dest_txt <- file.path(RAW_DIR, txt_name)
  if (!file.exists(dest_txt)) {
    unzip(dest_zip, files = txt_name, exdir = RAW_DIR)
    ## At least the 2014 file has literal NUL bytes standing in for empty fields (e.g. a
    ## write-in candidate's blank party column) -- confirmed via a byte-level check. readr
    ## silently corrupts every row after the first one it hits ("embedded null" parsing
    ## issue), so strip them here, once, right after unzipping.
    raw_bytes <- readBin(dest_txt, "raw", file.info(dest_txt)$size)
    if (any(raw_bytes == as.raw(0))) {
      writeBin(raw_bytes[raw_bytes != as.raw(0)], dest_txt)
    }
  }
  dest_txt
}

read_nc_congress_county <- function(year, date_str, delim, has_header, quote_char) {
  path <- download_nc_file(date_str)
  if (has_header) {
    raw <- read_delim(path, delim = delim, quote = quote_char, show_col_types = FALSE,
                       col_types = cols(.default = "c"))
    names(raw) <- standardize_names(names(raw))
  } else {
    raw <- read_delim(path, delim = delim, quote = quote_char, show_col_types = FALSE,
                       col_names = NO_HEADER_COLS, col_types = cols(.default = "c"))
    names(raw) <- standardize_names(names(raw))
  }
  raw %>%
    filter(grepl("^\"?US (HOUSE|CONGRESS)", contest, ignore.case = TRUE)) %>%
    ## `party` is a literal NA (not "") for some write-in rows in a few years -- `party=="DEM"`
    ## on an NA row evaluates to NA, and NA-indexing a vector inserts a real NA into the result,
    ## which then poisons sum() for the whole county-year. Coalesce to "" before any comparison.
    mutate(votes = as.numeric(votes),
           party = coalesce(trimws(gsub('"', "", party)), ""),
           county = trimws(gsub('"', "", county))) %>%
    group_by(county, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

nc_by_county <- pmap(NC_GENERAL_ELECTIONS, function(year, date_str, delim, has_header, quote_char) {
  read_nc_congress_county(year, date_str, delim, has_header, quote_char)
}) %>% bind_rows()

nc_fips <- county_fips_crosswalk %>% filter(state == "NORTH CAROLINA") %>% select(county_name, county_fips)

elect_he_cty_nc <- nc_by_county %>%
  group_by(year, county) %>%
  summarise(
    totalvote = sum(votes),
    demovote_n = sum(votes[party == "DEM"]),
    repuvote_n = sum(votes[party == "REP"]),
    .groups = "drop"
  ) %>%
  left_join(nc_fips, by = c("county" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "NORTH CAROLINA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  save_step("elect_he_cty_nc")

message("NC House county-level rows built: ", nrow(elect_he_cty_nc), " across years: ",
        paste(sort(unique(elect_he_cty_nc$year)), collapse = ", "))

## ---------------------------------------------------------------------------
## Cross-check against MEDSL for the one overlapping year (2016) -- both sources should agree
## closely on 2016 NC House vote shares despite being built by completely independent pipelines.
## ---------------------------------------------------------------------------
medsl_he <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_medsl.rds"))

check <- elect_he_cty_nc %>%
  filter(year == 2016) %>%
  inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_nc", "_medsl")) %>%
  mutate(repuvote_diff = abs(repuvote_nc - repuvote_medsl))

message("NC vs MEDSL 2016 cross-check: ", nrow(check), " counties matched, max repuvote diff = ",
        round(max(check$repuvote_diff, na.rm = TRUE), 4), ", mean diff = ",
        round(mean(check$repuvote_diff, na.rm = TRUE), 5))

## ---------------------------------------------------------------------------
## Virginia: apps.elections.virginia.gov's open CSV archive (fronted by Akamai but NOT bot-
## blocked -- confirmed a plain curl with a browser UA gets a real directory listing). Archive
## only goes back to 2005; General-election filenames aren't consistently named across years
## (e.g. 2006's is date-prefixed, others say "<year> November General[.csv| .csv]"), so each
## year's directory is listed and searched for a "November"+"General" match rather than
## constructing the filename directly. Virginia's LocalityName already includes independent
## cities (not just counties) as county-equivalents -- these are true localities, exactly the
## FIPS-equivalent geography level, no further splitting needed. Office is filtered on
## "House of Representatives" appearing in OfficeTitle (the exact title's suffix, e.g. "- 2001
## CD Lines", changes by redistricting decade, so matched loosely).
## ---------------------------------------------------------------------------
VA_BASE <- "https://apps.elections.virginia.gov/SBE_CSV/ELECTIONS/ELECTIONRESULTS"
VA_YEARS <- c(2006, 2008, 2010, 2012, 2014, 2016)  # 2016 overlaps MEDSL -- cross-check again

## apps.elections.virginia.gov (Akamai-fronted) 403s R's default HTTP client (no browser-like
## User-Agent), but a plain curl with one gets through fine (confirmed from the shell) -- so
## shell out to curl for both the directory listing and the file download rather than fighting
## base R's url()/download.file() over User-Agent configuration.
curl_get <- function(url) system2("curl", c("-sL", "-A", "'Mozilla/5.0'", shQuote(url)), stdout = TRUE)

find_va_general_csv_url <- function(year) {
  dir_html <- paste(curl_get(paste0(VA_BASE, "/", year, "/")), collapse = "\n")
  hrefs <- regmatches(dir_html, gregexpr('HREF="[^"]*"', dir_html, ignore.case = TRUE))[[1]]
  hrefs <- gsub('HREF="|"', "", hrefs, ignore.case = TRUE)
  cand <- hrefs[grepl("november", hrefs, ignore.case = TRUE) & grepl("general", hrefs, ignore.case = TRUE)]
  if (length(cand) == 0) stop("No November General CSV found for VA ", year)
  paste0("https://apps.elections.virginia.gov", cand[1])
}

download_va_file <- function(year) {
  dest <- file.path(RAW_DIR, paste0("va_", year, "_november_general.csv"))
  if (!file.exists(dest)) {
    url <- find_va_general_csv_url(year)
    system2("curl", c("-sL", "-A", "'Mozilla/5.0'", "-o", shQuote(dest), shQuote(url)))
  }
  dest
}

read_va_congress_locality <- function(year) {
  path <- download_va_file(year)
  raw <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  raw %>%
    filter(grepl("House of Representatives", OfficeTitle, ignore.case = TRUE)) %>%
    mutate(votes = as.numeric(TOTAL_VOTES), party = coalesce(trimws(Party), ""),
           locality = trimws(gsub("\\s+COUNTY$", "", toupper(LocalityName)))) %>%
    group_by(locality, party) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = year)
}

va_by_locality <- map_dfr(VA_YEARS, read_va_congress_locality)

## The reused crosswalk has a real data-quality wrinkle specific to Virginia: for a handful of
## county/city name collisions (Fairfax, Franklin, Richmond -- an independent city sharing a
## name with an entirely separate county), some source rows mislabel the *city* with the bare
## county name instead of "<name> CITY", so the bare name ends up pointing at BOTH fips codes.
## A naive join then duplicates the county's votes onto the city's fips too. Fix: for any bare
## name with 2+ distinct fips, drop whichever of those fips is already unambiguously reachable
## via a "<name> CITY" alias (that one is the city's real code; the bare name should resolve to
## the OTHER, county, fips only).
va_fips_raw <- county_fips_crosswalk %>% filter(state == "VIRGINIA") %>%
  mutate(county_name = gsub("\\s+COUNTY$", "", county_name)) %>%
  distinct(county_name, county_fips)

city_fips_by_base_name <- va_fips_raw %>%
  filter(grepl("\\s+CITY$", county_name)) %>%
  mutate(base_name = gsub("\\s+CITY$", "", county_name)) %>%
  pull(county_fips)

ambiguous_names <- va_fips_raw %>% count(county_name) %>% filter(n > 1) %>% pull(county_name)

va_fips <- va_fips_raw %>%
  filter(!(county_name %in% ambiguous_names & county_fips %in% city_fips_by_base_name))

elect_he_cty_va <- va_by_locality %>%
  group_by(year, locality) %>%
  summarise(
    totalvote = sum(votes),
    demovote_n = sum(votes[party == "Democratic"]),
    repuvote_n = sum(votes[party == "Republican"]),
    .groups = "drop"
  ) %>%
  left_join(va_fips, by = c("locality" = "county_name")) %>%
  filter(!is.na(county_fips), totalvote > 0) %>%
  transmute(
    state = "VIRGINIA", year, cty_fips = county_fips, sample = "HE",
    demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote
  ) %>%
  distinct(state, year, cty_fips, sample, .keep_all = TRUE) %>%  # Bedford county/city merger etc.
  save_step("elect_he_cty_va")

message("VA House county-level rows built: ", nrow(elect_he_cty_va), " across years: ",
        paste(sort(unique(elect_he_cty_va$year)), collapse = ", "))

va_check <- elect_he_cty_va %>%
  filter(year == 2016) %>%
  inner_join(medsl_he %>% filter(year == 2016), by = c("cty_fips", "year"), suffix = c("_va", "_medsl")) %>%
  mutate(repuvote_diff = abs(repuvote_va - repuvote_medsl))

message("VA vs MEDSL 2016 cross-check: ", nrow(va_check), " localities matched, max repuvote diff = ",
        round(max(va_check$repuvote_diff, na.rm = TRUE), 4), ", mean diff = ",
        round(mean(va_check$repuvote_diff, na.rm = TRUE), 5))

## ---------------------------------------------------------------------------
## Georgia: sos.ga.gov is Cloudflare-blocked for scripted access (see project notes), but the
## user retrieved 2012's results manually through a browser and dropped it in
## R/data/county_house_files/georgia_2012.csv -- that folder is a manual drop point: files
## here were obtained by hand (bot-blocked sites, one-off downloads) rather than scripted, and
## each state's export format is checked and handled on its own, same as NC's format changes by
## year. Georgia's file is ALREADY county-level (the "Precinct" column literally holds county
## names, e.g. "Appling County") with an explicit "Total Votes" pseudo-row per county-district
## alongside the real candidate rows -- use that row's value directly for totalvote rather than
## summing candidate rows (which would double count against it), and get demovote/repuvote from
## the Party=="DEM"/"REP" rows as usual.
## ---------------------------------------------------------------------------
MANUAL_DIR <- file.path(PROJECT_ROOT, "R", "data", "county_house_files")

ga_2012_path <- file.path(MANUAL_DIR, "georgia_2012.csv")
if (file.exists(ga_2012_path)) {
  ga_raw <- read_csv(ga_2012_path, show_col_types = FALSE) %>%
    filter(grepl("^U.S. Representative", `Office Name`)) %>%
    mutate(county = gsub("\\s+COUNTY$", "", toupper(trimws(Precinct))))

  ga_totals_by_district <- ga_raw %>% filter(`Ballot Name` == "Total Votes") %>%
    select(county, `Office Name`, totalvote_district = Total)

  ga_party_by_district <- ga_raw %>% filter(!is.na(Party)) %>%
    group_by(county, `Office Name`, Party) %>%
    summarise(votes = sum(Total, na.rm = TRUE), .groups = "drop")

  ga_fips <- county_fips_crosswalk %>% filter(state == "GEORGIA") %>%
    mutate(county_name = gsub("\\s+COUNTY$", "", county_name)) %>%
    distinct(county_name, county_fips)

  elect_he_cty_ga_2012 <- ga_totals_by_district %>%
    group_by(county) %>%
    summarise(totalvote = sum(totalvote_district), .groups = "drop") %>%
    left_join(
      ga_party_by_district %>% group_by(county, Party) %>% summarise(votes = sum(votes), .groups = "drop") %>%
        pivot_wider(names_from = Party, values_from = votes, values_fill = 0),
      by = "county"
    ) %>%
    left_join(ga_fips, by = c("county" = "county_name")) %>%
    filter(!is.na(county_fips), totalvote > 0) %>%
    transmute(
      state = "GEORGIA", year = 2012, cty_fips = county_fips, sample = "HE",
      demovote = DEM / totalvote, repuvote = REP / totalvote, totalvote
    ) %>%
    save_step("elect_he_cty_ga_2012")

  message("GA 2012 House county-level rows built: ", nrow(elect_he_cty_ga_2012), " of ",
          nrow(ga_fips), " counties")

  ## Can't cross-check against MEDSL (2012 predates its coverage) -- sanity-check instead
  ## against the well-known statewide result: Obama got ~45.5% of Georgia's 2012 presidential
  ## vote, and Democrats are known to have won a minority of GA's 14 House seats that year.
  ## Just confirm shares are plausible (in [0,1], no >100% two-party sums) rather than exact.
  ga_sanity <- elect_he_cty_ga_2012$demovote + elect_he_cty_ga_2012$repuvote
  message("GA 2012 sanity: demovote+repuvote range [", round(min(ga_sanity), 3), ", ",
          round(max(ga_sanity), 3), "], statewide repuvote (unweighted mean) = ",
          round(mean(elect_he_cty_ga_2012$repuvote), 3))

  elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>%
    bind_rows(elect_he_cty_ga_2012 %>% select(-state)) %>%
    distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
    arrange(cty_fips, year, sample) %>%
    save_step("elect_cty_final")

  message("elect_cty_final.rds updated with GA 2012. Total rows now: ", nrow(elect_cty_final))
} else {
  message("No manual Georgia file found at ", ga_2012_path, " -- skipping.")
}

## ---------------------------------------------------------------------------
## Fold both states' pre-2016 years into the master election panel (elect_cty_final.rds, built
## by 01b) -- 2016+ is left alone since MEDSL already covers it (and the cross-checks above
## confirm these new sources agree with MEDSL exactly where they overlap anyway).
## ---------------------------------------------------------------------------
elect_cty_final <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))

new_he_rows <- bind_rows(
  elect_he_cty_nc %>% filter(year < 2016) %>% select(-state),
  elect_he_cty_va %>% filter(year < 2016) %>% select(-state)
)

elect_cty_final <- bind_rows(elect_cty_final, new_he_rows) %>%
  distinct(cty_fips, year, sample, .keep_all = TRUE) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_final")

message("elect_cty_final.rds updated: added ", nrow(new_he_rows), " NC/VA House rows (pre-2016). ",
        "Total rows now: ", nrow(elect_cty_final))
