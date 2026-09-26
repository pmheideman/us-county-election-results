## Election data from MEDSL (MIT Election Data + Science Lab), substituting for the paywalled
## CQ Press / Dave Leip's Atlas / ICPSR sources the original Files/Dofiles/Data_creation/
## 1st_Election_data.do relied on (see project notes: Election/ folder ships empty in the
## replication package, and those sources have no free equivalent).
##
## Coverage, by construction of what MEDSL publishes:
##   President: county-level directly, 2000-2024   (County Presidential Election Returns 2000-2024)
##   House:     precinct-level, aggregated to county here, 2016-2024 (even years)
##   Senate:    precinct-level, aggregated to county here, 2016-2024 (even years)
## Pre-2000 presidential and pre-2016 House/Senate are out of scope for this port (see
## conversation: MEDSL's own multi-year House/Senate series are district-/state-level, not
## county; only the single-year precinct files carry county_fips, and those only go back to
## 2016).
##
## Precinct -> county aggregation caveat (from MEDSL's own README for these files): many states
## report the same votes more than once under different `mode` values (e.g. both an
## "ELECTION DAY" row and a "TOTAL" row that already includes it), and MEDSL's own team found
## "no simple rule was successful" for resolving this that works identically across all states.
## The rule applied here -- per (state, county, precinct), keep only mode=="TOTAL" rows if any
## exist for that precinct, otherwise keep and sum all of its (non-TOTAL) mode rows -- is the
## standard heuristic used with this dataset, not a guarantee of exact official totals. Rows
## with negative "votes" (MEDSL's sentinel for privacy-masked/suppressed vote counts in a
## handful of states) are dropped before aggregating, per MEDSL's own guidance.

source(file.path("R", "00_setup.R"))
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_election")

detect_delim <- function(path) {
  first_line <- readLines(path, n = 1)
  if (lengths(regmatches(first_line, gregexpr("\t", first_line))) > lengths(regmatches(first_line, gregexpr(",", first_line)))) "\t" else ","
}

## Reads one MEDSL precinct-level file (House or Senate, one year), resolves the mode
## double-counting issue, and collapses to county x party vote totals.
## Options (added 2026-09-20; see data_corrections_log.csv):
##   party_fallback    -- when MEDSL's party_simplified is not D/R/L, fall back to substring tests on party_detailed
##                        (fixes DEMOCRATIC-NPL [ND 2020/22], REPUBLICAN/LIBERTARIAN [VT 2024], REPUBLICAN/CONSERVATIVE
##                        [NY 2024], DEMOCRAT / INDEPENDENT + DEMOCRAT / PROGRESSIVE [OR 2024]; Minnesota DFL as before).
##                        Audit of every 2018-2024 House/Senate row found exactly these 7 detailed-party strings.
##   fill_blank_party  -- candidates whose party is blank in some counties but labelled in others (NJ/OR 2024) take the
##                        party_std of their vote-weighted labelled rows (same state + district + normalized name).
##   exclude_pseudo    -- drop non-candidate rows (over/under votes, blanks/voids, CONTEST TOTAL, TOTAL VOTES CAST, CAST
##                        VOTES ...) so totalvote = votes for candidates. Without this they inflate totalvote and dilute
##                        shares (49 state-years have >0.5% of votes in such rows; NJ/OR 2024 double-count).
##   exclude_special  -- drop races flagged `special` (concurrent or off-cycle special elections). Project decision 2026-09-20: v1 covers
##                        regular general elections only. Without this, state-years with a regular AND a special race on the ballot
##                        sum two different races (Senate: AZ 2020, CA 2024, GA 2020, MN 2018, MS 2018, NE 2024, OK 2022; House: WI-8
##                        2024, MD-7 2020, MI-13 2018, TX-18 2024, NY-25 2018, IN-2 2022, KY-1 2016), doubling totals. AZ Senate 2020's only
##                        race is a special, so those rows disappear (documented as a gap: special election only).
##   exclude_other_offices -- keep only rows whose `office` is US House / US Senate. The 2016 files also contain STRAIGHT PARTY / STRAIGHT TICKET lines
##                        (AL 1.16M votes, SC 1.07M, IN 213K, KY 36K) and Arkansas "UNOPPOSED CANDIDATES" ballot rows ("FOR"), plus DC delegate /
##                        shadow offices; counting them as House/Senate votes inflated totals 1.45-1.67x in AL, SC, AR, IN (found 2026-09-20).
PSEUDO_RE <- paste0("^(OVER ?VOTES?|UNDER ?VOTES?|UNDERVOTES?[- ]VOIDS?|OVER|CONTEST TOTALS?|TOTAL VOTES( CAST)?|TOTAL BALLOTS( CAST)?|TOTAL|CAST VOTES|BALLOTS CAST|",
  "BLANK( BALLOTS| VOTES| ?/ ?VOID)?|BLANKS?|VOIDS?|REGISTERED VOTERS|AFFIDAVIT|ABSENTEE ?/ ?MILITARY|FEDERAL( BALLOTS| VOTES?)?|PUBLIC COUNTER|MANUALLY COUNTED( EMERGENCY)?|SCATTERED VOTES|NOT QUALIFIED|NOT ASSIGNED|NOT SURE|",
  "SPECIAL VOTES|SPECIAL PRESIDENTIAL|SPOILED|INVALID( VOTES)?|NO CANDIDATE|NO CONFIDENCE|NONE|",
  "TIMES BLANK VOTED|BLANK \\(2\\)|BLANK WRITE-?INS?|REJECTED( WRITE-?INS?)?|WRITE-IN: INVALID WRITE-IN)$")   # extended 2026-09-21 (Arizona 2018 'Times Blank Voted', Michigan 2022 'Rejected Write-Ins', Vermont 2022 'Blank' variants, Utah 2022 invalid write-ins); see also PSEUDO_NAME_RE in R/long_helpers.R   # extended 2026-09-21: 'Federal Votes', 'Public Counter', 'Manually Counted Emergency' (New York 2018 Senate: Bronx and Queens totals were doubled by 'Public Counter' = the machine-count subtotal; Tompkins had 38,211 'Federal Votes'; found by comparison with America Votes 33); extended 2026-09-20: Maine 2024 (New York 2018: Tompkins County had 38,211 of them in the Senate race, found by comparison with America Votes 33); extended 2026-09-20: Maine 2024 'Total Ballots Cast', NY 'Affidavit'/'Absentee/military'/'Over', ...

##   candidate_level_party -- every row of a candidate takes that candidate's vote-weighted primary party_std (state +
##                        district + normalized name). Repairs raw label misalignment (Oregon Harney 2024: Dan Ruby labelled
##                        LIBERTARIAN, Michael Stettler labelled DEMOCRAT) and consolidates fusion lines onto the candidate.
##                        Default FALSE: it is only applied where explicitly requested (NJ/OR 2024 in 01ca_medsl_fixes_apply.R).
##   by_candidate     -- return one row per county x district x candidate x party line (with the raw party label) instead of county x
##                        party_std totals. Used to build the candidate-level long table (02a_house_long_medsl.R); the pipeline default is FALSE.
read_precinct_file <- function(path, office_year, party_fallback = TRUE, fill_blank_party = TRUE, exclude_pseudo = TRUE, candidate_level_party = FALSE, exclude_special = TRUE, by_candidate = FALSE, exclude_other_offices = TRUE, decisive_round = TRUE) {
  delim <- detect_delim(path)
  cols_wanted <- c("year", "state", "state_fips", "county_fips", "county_name", "precinct",
                    "district", "candidate", "party", "party_simplified", "party_detailed", "mode", "votes", "special", "office", "stage")
  header <- names(read_delim(path, delim = delim, n_max = 0, show_col_types = FALSE))
  select_cols <- intersect(cols_wanted, header)

  raw <- read_delim(path, delim = delim, col_select = all_of(select_cols),
                     col_types = cols(votes = col_character(), county_fips = col_character(), .default = col_character()),
                     show_col_types = FALSE, progress = FALSE)

  raw <- raw %>%
    mutate(
      votes = as.numeric(votes),
      county_fips = as.integer(county_fips),
      mode = toupper(mode)
    ) %>%
    filter(!is.na(votes), votes >= 0, !is.na(county_fips)) %>%
    ## MEDSL 2016 codes Yates County, NY (36123) as 36122, which is not a county, with a blank county_name; the precincts
    ## (jurisdiction "Yates": Barrington, Benton, Italy, Jerusalem, Milo, ...) are Yates's. 2016 House and Senate only (01jp).
    mutate(county_fips = ifelse(county_fips == 36122L, 36123L, county_fips))

  if (exclude_other_offices && "office" %in% names(raw)) {
    raw <- raw %>% filter(grepl("^(US|U\\.S\\.) (HOUSE|SENATE)$", toupper(trimws(office))))
  }

  if (exclude_special && "special" %in% names(raw)) {
    raw <- raw %>% filter(!toupper(trimws(special)) %in% c("TRUE", "T", "1", "YES"))
  }

  ## decisive round (docs/DECISIONS.md, same rule as Louisiana): where a race has a general-election RUNOFF stage in the file, only the runoff rows are kept. Georgia's 2022 Senate file holds the November
  ## general AND the December runoff; summing both doubled every county total (147 of 159 counties flagged by the QA sweep, T2).
  if (decisive_round && all(c("stage", "state", "office", "district") %in% names(raw))) {
    raw <- raw %>% group_by(state, office, district) %>% mutate(.has_ro = any(grepl("RUNOFF", toupper(stage)))) %>% ungroup() %>%
      filter(!.has_ro | grepl("RUNOFF", toupper(stage))) %>% select(-.has_ro)
  }

  if (exclude_pseudo && "candidate" %in% names(raw)) {
    raw <- raw %>% filter(!grepl(PSEUDO_RE, toupper(trimws(candidate))))
  }

  # 2016 files carry `party` (lowercase values); 2018+ carry `party_simplified` (upper-case).
  # Minnesota's Democratic-Farmer-Labor party is Minnesota's actual state-affiliate name for the
  # Democratic party (not a third party) but MEDSL's own party_simplified field mis-buckets it as
  # "OTHER" in some years (confirmed: 2020, 2022 -- both have party_detailed == "DEMOCRATIC FARMER
  # LABOR"/"DEMOCRATIC-FARMER-LABOR" but party_simplified == "OTHER" for every MN row; 2018/2024
  # correctly show party_simplified == "DEMOCRAT"). Left unhandled, this zeroes out demovote for
  # every MN county in the affected years. Override via party_detailed whenever it's available.
  is_dfl <- function(x) grepl("DEMOCRATIC.?FARMER.?LABOR", toupper(x))
  if ("party_simplified" %in% names(raw)) {
    raw <- raw %>%
      mutate(party_std = case_when(
        party_simplified %in% c("DEMOCRAT", "REPUBLICAN", "LIBERTARIAN") ~ party_simplified,
        "party_detailed" %in% names(raw) & is_dfl(party_detailed) ~ "DEMOCRAT",
        # substring fallback on party_detailed (Democrat checked first, as for Welch/VT 2016)
        party_fallback & grepl("DEMOCRAT", toupper(party_detailed)) ~ "DEMOCRAT",
        party_fallback & grepl("REPUBLIC", toupper(party_detailed)) ~ "REPUBLICAN",
        TRUE ~ "OTHER"
      ))
  } else {
    # Oregon's 2016 `party` values are not always the plain "democratic"/"republican" used by
    # every other state -- some rows are bare "democrat"/"republican" (no -ic suffix) and some
    # are genuine fusion candidacies recorded as one compound, comma-joined string in a SINGLE
    # row ("republican, independent", "democrat, independent, working families") rather than
    # separate per-line rows the way New York's fusion data is structured. An exact `== "democratic"`/
    # `== "republican"` match (the original logic here) never matches ANY of these -- confirmed via
    # raw-row inspection that this zeroed out demovote for every OR county in 2016 (repuvote was
    # partially preserved only because some OR House rows happen to use the plain, non-fused
    # "republican" string). Fixed with a substring match on the "democrat"/"republican" token
    # instead of an exact match; checked this doesn't false-positive elsewhere in the 2016 file
    # (only Minnesota's already-handled DFL, Oregon, and Vermont contain "democrat"/"republican" as
    # a substring of a non-exact party value). Also fixes Vermont's 2016 "Democratic/Republican"
    # row (Peter Welch, recorded this way because he ran uncontested that year -- the same MEDSL
    # uncontested-race quirk documented for Kentucky) -- correctly resolves to DEMOCRAT since Welch
    # was the real Democratic nominee, checked democrat before republican so this one case doesn't
    # fall through to REPUBLICAN.
    raw <- raw %>%
      mutate(party_l = tolower(party)) %>%
      mutate(party_std = case_when(
        is_dfl(party) | grepl("democrat", party_l) ~ "DEMOCRAT",
        grepl("republican", party_l) ~ "REPUBLICAN",
        party_l == "libertarian" ~ "LIBERTARIAN",
        TRUE ~ "OTHER"
      )) %>%
      select(-party_l)
  }

  if (fill_blank_party && "candidate" %in% names(raw)) {
    is_blank <- if ("party_simplified" %in% names(raw)) is.na(raw$party_simplified) & is.na(raw$party_detailed) else (is.na(raw$party) | raw$party == "")
    norm_cand <- function(x) trimws(gsub("\\s+", " ", gsub("[^A-Z ]", "", toupper(x))))
    raw <- raw %>% mutate(.blank = is_blank, .ckey = norm_cand(candidate))
    lookup <- raw %>% filter(!.blank) %>%
      group_by(state, district, .ckey, party_std) %>% summarise(.v = sum(votes), .groups = "drop") %>%
      group_by(state, district, .ckey) %>% slice_max(.v, n = 1, with_ties = FALSE) %>% ungroup() %>%
      transmute(state, district, .ckey, .fill = party_std)
    # last resort: candidates with NO labelled row anywhere (whole districts blank in NJ/OR 2024) -> external table
    # (R/data/raw_election/candidate_party_overrides.csv, from Wikipedia nominees; built by 01cb_candidate_party_overrides.R)
    ov_path <- file.path(RAW_DIR, "candidate_party_overrides.csv")
    surname_key <- function(x) vapply(strsplit(x, " "), function(tk) { tk <- tk[!tk %in% c("JR", "SR", "II", "III", "IV", "V", "")]; if (length(tk)) tail(tk, 1) else "" }, "")
    raw <- raw %>% left_join(lookup, by = c("state", "district", ".ckey"))
    if (file.exists(ov_path)) {
      ovt <- read_csv(ov_path, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
        transmute(.state_u = state, district, .skey = surname_key, .ov = party_std)
      raw <- raw %>% mutate(.state_u = toupper(state), .skey = surname_key(.ckey)) %>%
        left_join(ovt, by = c(".state_u", "district", ".skey")) %>% select(-.state_u, -.skey)
    } else raw$.ov <- NA_character_
    raw <- raw %>%
      mutate(party_std = case_when(.blank & !is.na(.fill) ~ .fill, .blank & !is.na(.ov) ~ .ov, TRUE ~ party_std)) %>%
      select(-.blank, -.ckey, -.fill, -.ov)
  }

  if (candidate_level_party && "candidate" %in% names(raw)) {
    nc <- function(x) trimws(gsub("\\s+", " ", gsub("[^A-Z ]", "", toupper(x))))
    prim <- raw %>% mutate(.ck = nc(candidate)) %>% group_by(state, district, .ck, party_std) %>% summarise(.v = sum(votes), .groups = "drop") %>%
      group_by(state, district, .ck) %>% slice_max(.v, n = 1, with_ties = FALSE) %>% ungroup() %>% transmute(state, district, .ck, .prim = party_std)
    raw <- raw %>% mutate(.ck = nc(candidate)) %>% left_join(prim, by = c("state", "district", ".ck")) %>%
      mutate(party_std = .prim) %>% select(-.ck, -.prim)
  }

  ## Redundant county-rollup "precinct" rows: some states' precinct files add ONE extra row per candidate whose `precinct` field is a summary label
  ## ("COUNTY TOTAL", "Total", "Countywide", "SUBTOTAL", ...) carrying the SAME votes as the sum of that candidate's real precincts in the county --
  ## not a mode duplicate (it already has mode=="TOTAL", so the existing per-precinct dedup below does not see it as a duplicate: it IS its own precinct).
  ## Idaho 2022 House and Senate report exactly this way (confirmed: the "COUNTY TOTAL" row equals the sum of the other precincts euro for euro), which
  ## doubled every county's House and Senate total; Oregon 2018 House, Mississippi 2022 House and Michigan 2022 House do the same for some counties.
  ## Dropped ONLY when the county also has genuine, non-rollup precinct rows for that candidate (a county whose ONLY row is a "Total"/"Countywide"
  ## label, e.g. Washington, Delaware and Oregon 2024, is reporting county-level totals as its only precinct data and must be kept).
  if ("candidate" %in% names(raw)) {
    ROLLUP_PRECINCT_RE <- "^(COUNTY )?TOTALS?$|COUNTY TOTALS?|^TOTAL[- ]|COUNTYWIDE|COUNTY WIDE|SUBTOTAL|ALL PRECINCTS|GRAND TOTAL|^SUMMARY|^COUNTY$|TOTAL VOTES"
    ## Safety: a "rollup" label is only a genuine duplicate when its own vote count equals the sum of that candidate's OTHER precincts in the same county+district exactly
    ## (checked county-by-county rather than assumed from the label alone: Nebraska's tiny counties use "Countywide" as their only REAL precinct name, so a naive
    ## "label matches AND other rows exist" rule -- other rows here being a same-named "Countywide absentee" 0-vote row or a real-but-empty town -- zeroed them out).
    raw <- raw %>% mutate(.rollup = grepl(ROLLUP_PRECINCT_RE, toupper(trimws(precinct)))) %>%
      group_by(state, county_fips, district, candidate) %>%
      mutate(.other_sum = sum(votes[!.rollup]), .rollup_sum = sum(votes[.rollup]), .drop_rollup = any(!.rollup) & abs(.other_sum - .rollup_sum) < 1e-6 & .other_sum > 0) %>%
      ungroup() %>% filter(!.rollup | !.drop_rollup) %>% select(-.rollup, -.other_sum, -.rollup_sum, -.drop_rollup)
  }

  # Mode de-duplication: prefer TOTAL where a precinct reports it, else sum the sub-modes.
  precinct_key <- c("state", "county_fips", "precinct")
  has_total <- raw %>%
    group_by(across(all_of(precinct_key))) %>%
    summarise(has_total = any(mode == "TOTAL"), .groups = "drop")

  resolved <- raw %>%
    left_join(has_total, by = precinct_key) %>%
    filter(!has_total | mode == "TOTAL")

  if (by_candidate) {
    resolved <- resolved %>% mutate(party_label = if ("party_detailed" %in% names(resolved)) party_detailed else party)
    return(resolved %>%
      group_by(state, state_fips, county_fips, county_name, district, candidate, party_label, party_std) %>%
      summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
      mutate(year = office_year))
  }

  resolved %>%
    group_by(state, state_fips, county_fips, county_name, party_std) %>%
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop") %>%
    mutate(year = office_year)
}

## ---- President: already county-level, but still needs the same mode de-duplication as the
## precinct files -- 2020 and 2024 report the same votes under both a "TOTAL" mode row and
## several disaggregated mode rows (ABSENTEE, EARLY VOTING, ELECTION DAY, ...) per county;
## summing candidatevotes across all of them quadruple-counted the vote (caught via a
## repuvote+demovote > 1 sanity check on the 2024 output).
countypres_raw <- read_delim(file.path(RAW_DIR, "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  mutate(candidatevotes = as.numeric(candidatevotes), totalvotes = as.numeric(totalvotes), mode = toupper(mode))

countypres_has_total <- countypres_raw %>%
  group_by(year, county_fips) %>%
  summarise(has_total = any(mode == "TOTAL"), .groups = "drop")

countypres <- countypres_raw %>%
  left_join(countypres_has_total, by = c("year", "county_fips")) %>%
  filter(!has_total | mode == "TOTAL") %>%
  group_by(year, county_fips, party) %>%
  summarise(votes = sum(candidatevotes, na.rm = TRUE), totalvote = first(totalvotes), .groups = "drop") %>%
  mutate(party_std = case_when(
    tolower(party) == "democrat" ~ "DEMOCRAT",
    tolower(party) == "republican" ~ "REPUBLICAN",
    TRUE ~ "OTHER"
  )) %>%
  group_by(year, county_fips, party_std) %>%
  summarise(votes = sum(votes, na.rm = TRUE), totalvote = first(totalvote), .groups = "drop")

pres_wide <- countypres %>%
  group_by(year, county_fips) %>%
  summarise(
    demovote_n = sum(votes[party_std == "DEMOCRAT"], na.rm = TRUE),
    repuvote_n = sum(votes[party_std == "REPUBLICAN"], na.rm = TRUE),
    totalvote   = first(totalvote),
    .groups = "drop"
  ) %>%
  mutate(demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, sample = "PE") %>%
  select(year, cty_fips = county_fips, sample, demovote, repuvote, totalvote) %>%
  save_step("elect_pe_cty_medsl")

## ---- House and Senate: aggregate precinct files to county x year ----
office_years <- expand_grid(office = c("house", "senate"), year = c(2016, 2018, 2020, 2022, 2024))

precinct_results <- purrr::pmap(office_years, function(office, year) {
  path <- file.path(RAW_DIR, paste0(office, "_", year, ".raw"))
  message("Aggregating ", office, " ", year, " ...")
  read_precinct_file(path, year) %>% mutate(office = office)
})

county_office_totals <- bind_rows(precinct_results)
save_step(county_office_totals, "county_office_party_totals_medsl")

to_wide <- function(df, sample_code) {
  df %>%
    group_by(year, county_fips) %>%
    summarise(
      demovote_n = sum(votes[party_std == "DEMOCRAT"], na.rm = TRUE),
      repuvote_n = sum(votes[party_std == "REPUBLICAN"], na.rm = TRUE),
      totalvote   = sum(votes, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, sample = sample_code) %>%
    select(year, cty_fips = county_fips, sample, demovote, repuvote, totalvote)
}

house_wide  <- county_office_totals %>% filter(office == "house")  %>% to_wide("HE") %>% save_step("elect_he_cty_medsl")
senate_wide <- county_office_totals %>% filter(office == "senate") %>% to_wide("SE") %>% save_step("elect_se_cty_medsl")

## ---- Combined county-year-office election panel (analogous to the source's Output/elect_cty.dta) ----
elect_cty_medsl <- bind_rows(pres_wide, house_wide, senate_wide) %>%
  filter(!is.na(repuvote), !is.na(demovote), !is.na(cty_fips)) %>%
  arrange(cty_fips, year, sample) %>%
  save_step("elect_cty_medsl")
