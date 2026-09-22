## Maine county-level U.S. House results, 1990-2010 + 2014 (2012 already built in 01af from OpenElections).
##
## Source: Maine Secretary of State's official town/ward-level tabulation spreadsheets for U.S.
## Representative to Congress, already downloaded into R/data/raw_house_county_open_states/maine/
## (1990-2006, 2010, 2012, 2014 .xlsx). 2008 is NOT among those spreadsheets -- it is on
## https://www.maine.gov/sos/elections-voting/election-results-data/election-results-2008-2009
## as tabs-can-cg1-11.txt / tabs-can-cg2-11.txt (comma-delimited, one row per town/ward/precinct,
## already carries county code AND party) -- downloaded here from that page.
##
## Every one of these files reports at the town/ward level with either a 3-letter county code
## (1990-2010) or, for 2012/2014, no county column at all but printed "<County> County Totals" rows
## directly beneath each county's towns. Maine has no split towns, so town->county is never
## ambiguous; for 2014 the county is taken from the NEXT "<County> County Totals" row below each
## town row (no name-based crosswalk => no spelling-mismatch risk) and then re-verified against
## that printed county total. The two congressional districts split several counties (Kennebec,
## Waldo, Hancock etc. depending on the year), so county totals are summed across both districts.
##
## Party: 1994, 1996, 1998, 2002-2010 and 2014 spreadsheets carry party directly. 1990, 1992 and 2000
## print candidate LAST NAMES only, so party comes from a small hand-checked lookup taken from
## Wikipedia's "<year> United States House of Representatives elections" Maine section (fetched
## raw wikitext; Maine's own per-year articles for these years are only redirects). Only three
## years x <=5 candidates are needed and every one is cross-checked below against the statewide
## percentages Wikipedia prints for that race.
##
## Definition of totalvote (matches 01af's 2012 OpenElections build): sum of every named
## candidate's votes plus printed write-in/"other" votes; BLANK ballots are excluded.
## Rows with no county (STATE UOCAVA, EMERGENCY UTILITY WORKERS, "Central absentee" lines that
## have no county) cannot be attributed to a county and are dropped (tiny: <0.15% of the vote).
##
## Outputs R/output/elect_he_cty_me_historical.rds (same schema as elect_he_cty_me.rds).
## Does NOT touch elect_cty_final.rds / the coverage tracker (serialized fold-in is done separately).

source(file.path("R", "00_setup.R"))
library(readxl)
library(readr)

RAW_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "maine")
BASE_2008 <- "https://www.maine.gov/sos/sites/maine.gov.sos/files/content/assets/"

county_fips_crosswalk <- read_delim(
  file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"),
  delim = "\t", show_col_types = FALSE
) %>% distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
me_fips <- county_fips_crosswalk %>% filter(state == "MAINE", !is.na(county_fips)) %>%
  select(county_name, county_fips)

CTY_CODE <- c(AND = "ANDROSCOGGIN", ARO = "AROOSTOOK", CUM = "CUMBERLAND", FRA = "FRANKLIN",
              HAN = "HANCOCK", KEN = "KENNEBEC", KNO = "KNOX", LIN = "LINCOLN", OXF = "OXFORD",
              PEN = "PENOBSCOT", PIS = "PISCATAQUIS", SAG = "SAGADAHOC", SOM = "SOMERSET",
              WAL = "WALDO", WAS = "WASHINGTON", YOR = "YORK")

## ---- shared helpers ----------------------------------------------------------------------
read_sheet <- function(f, sheet = 1) {
  suppressMessages(read_excel(file.path(RAW_DIR, f), sheet = sheet, col_names = FALSE, col_types = "text"))
}
num <- function(x) {
  x <- trimws(as.character(x)); x[x %in% c("-0-", "")] <- "0"
  suppressWarnings(as.numeric(gsub(",", "", x)))
}
is_cty_code <- function(x) !is.na(x) & toupper(trimws(x)) %in% names(CTY_CODE)
party_std <- function(x) {
  x <- toupper(trimws(x))
  dplyr::case_when(is.na(x) ~ NA_character_,
                   startsWith(x, "DEM") ~ "DEM",
                   startsWith(x, "REP") ~ "REP",
                   TRUE ~ "OTHER")
}

## Long form: one row per (year, county, district, candidate) summed over towns/wards.
mk_long <- function(year, county, district, cand, party, votes) {
  tibble(year = year, county = toupper(trimws(county)), district = as.character(district),
         cand = toupper(trimws(cand)), party = party, votes = votes) %>%
    filter(!is.na(votes))
}

## name/residence/party/votes quadruples starting at column `first` (1996, 2002, 2004, 2006).
quad_long <- function(x, year, ccol, dcol, first, ncand, party_col_off = 2, has_res = TRUE) {
  step <- 4
  purrr::map_dfr(seq_len(ncand), function(k) {
    b <- first + (k - 1) * step
    mk_long(year, CTY_CODE[toupper(trimws(x[[ccol]]))], x[[dcol]], x[[b]], party_std(x[[b + party_col_off]]),
            num(x[[b + 3]])) %>% filter(cand != "" & !is.na(cand))
  })
}

## ---- 1990 / 1992: layout = specnum, town, county, cd, cg1, cg2, cg3, cgot, name1..name3 ----
## Names are candidate LAST NAMES only, no party -> party lookup below.
parse_1990s_a <- function(f, year) {
  x <- read_sheet(f)
  x <- x[is_cty_code(x[[3]]) & x[[4]] %in% c("1", "2"), ]
  bind_rows(
    purrr::map_dfr(1:3, function(k) mk_long(year, CTY_CODE[toupper(trimws(x[[3]]))], x[[4]], x[[8 + k]], NA_character_, num(x[[4 + k]]))),
    mk_long(year, CTY_CODE[toupper(trimws(x[[3]]))], x[[4]], rep("(OTHER/WRITE-IN)", nrow(x)), "OTHER", num(x[[8]]))
  ) %>% filter(!is.na(cand) & cand != "NA")
}

## ---- 1994: header row; county,town,dist,name1,party1,vote1,...,name4,party4,vote4,other ----
parse_1994 <- function(f, year) {
  x <- read_sheet(f)
  x <- x[-1, ]
  x <- x[is_cty_code(x[[1]]), ]
  bind_rows(
    purrr::map_dfr(0:3, function(k) mk_long(year, CTY_CODE[toupper(trimws(x[[1]]))], x[[3]], x[[4 + 3 * k]], party_std(x[[5 + 3 * k]]), num(x[[6 + 3 * k]]))),
    mk_long(year, CTY_CODE[toupper(trimws(x[[1]]))], x[[3]], rep("(OTHER/WRITE-IN)", nrow(x)), "OTHER", num(x[[16]]))
  ) %>% filter(!is.na(cand) & cand != "NA")
}

## ---- 1996: office,district,county,town,wardprec, (name,res,party,votes) x3 (+ stray "OTHER" label col) ----
parse_1996 <- function(f, year) {
  x <- read_sheet(f)
  x <- x[x[[1]] == "CG" & is_cty_code(x[[3]]), ]
  quad_long(x, year, 3, 2, 6, 3) %>% filter(!is.na(party))
}

## ---- 1998: county,town,ward,precinct,office,district, then (votes,last,first,mi,suf,rescity,pname) x3 ----
parse_1998 <- function(f, year) {
  x <- read_sheet(f)
  x <- x[x[[5]] == "CG" & is_cty_code(x[[1]]), ]
  purrr::map_dfr(0:2, function(k) {
    b <- 7 + 7 * k
    mk_long(year, CTY_CODE[toupper(trimws(x[[1]]))], trimws(x[[6]]), x[[b + 1]], party_std(x[[b + 6]]), num(x[[b]]))
  }) %>% filter(!is.na(cand) & cand != "NA" & cand != "-0-")
}

## ---- 2000: munid,county,town,wardprec,district, (votes,name) x3 ----
parse_2000 <- function(f, year) {
  x <- read_sheet(f)
  x <- x[is_cty_code(x[[2]]) & x[[5]] %in% c("1", "2"), ]
  purrr::map_dfr(0:2, function(k) mk_long(year, CTY_CODE[toupper(trimws(x[[2]]))], x[[5]], x[[7 + 2 * k]], NA_character_, num(x[[6 + 2 * k]]))) %>%
    filter(!is.na(cand) & cand != "NA")
}

## ---- 2002/2004/2006: office,district,county,town,wardprec, (fullname,rescity,party,votes) x N ----
parse_quad_sheet <- function(f, year, ncand, header) {
  x <- read_sheet(f)
  if (header) x <- x[-1, ]
  x <- x[x[[1]] == "CG" & is_cty_code(x[[3]]), ]
  quad_long(x, year, 3, 2, 6, ncand) %>% filter(!is.na(party))
}

## ---- 2008: comma-delimited text from the SOS site (has county code + party) ----
parse_2008 <- function() {
  out <- purrr::map_dfr(1:2, function(d) {
    f <- file.path(RAW_DIR, paste0("2008_cg", d, ".txt"))
    if (!file.exists(f) || file.size(f) == 0)
      system2("curl", c("-sL", "-A", shQuote("Mozilla/5.0"), "-o", shQuote(f), shQuote(paste0(BASE_2008, "tabs-can-cg", d, "-11.txt"))))
    x <- read.csv(f, header = FALSE, skip = 1, fill = TRUE, colClasses = "character",
                  col.names = paste0("V", 1:20), quote = "\"") %>% as_tibble()
    x <- x[x$V1 == "CG" & is_cty_code(x$V3), ]
    purrr::map_dfr(0:3, function(k) {
      nm <- x[[paste0("V", 6 + 3 * k)]]; pt <- x[[paste0("V", 7 + 3 * k)]]; vt <- x[[paste0("V", 8 + 3 * k)]]
      mk_long(2008, CTY_CODE[toupper(trimws(x$V3))], x$V2, nm, party_std(pt), num(vt)) %>% filter(cand != "" & !is.na(cand))
    })
  })
  out
}

## ---- 2010: office,district,county,town,wardprec, votes1..N; candidate/party in header blocks ----
parse_2010 <- function(f, year) {
  x <- read_sheet(f)
  hdr <- function(rows, ncol_) list(name = unlist(x[rows[1], 6:(5 + ncol_)]), party = unlist(x[rows[3], 6:(5 + ncol_)]))
  ## CD1 header = first 3 rows above the "OFFICE" row; CD2 header = the 4 rows just above the first CD2 data row
  cd1_rows <- 1:3
  first_cd2 <- which(x[[1]] == "Representative To Congress" & x[[2]] == "2")[1]
  cd2_rows <- (first_cd2 - 4):(first_cd2 - 2)   # name, residence, party (VOTESn label row is first_cd2 - 1)
  h1 <- list(name = unname(unlist(x[cd1_rows[1], 6:10])), party = unname(unlist(x[cd1_rows[3], 6:10])))
  h2 <- list(name = unname(unlist(x[cd2_rows[1], 6:8])), party = unname(unlist(x[cd2_rows[3], 6:8])))
  stopifnot(any(grepl("Pingree", h1$name)), any(grepl("Michaud", h2$name)),
            identical(toupper(h1$name[5]), "BLANK"), identical(toupper(h2$name[3]), "BLANK"))
  d <- x[x[[1]] == "Representative To Congress" & is_cty_code(x[[3]]), ]
  purrr::map_dfr(1:2, function(dd) {
    xd <- d[d[[2]] == as.character(dd), ]
    h <- if (dd == 1) h1 else h2
    nc <- length(h$name) - 1                       # last column is BLANK
    purrr::map_dfr(seq_len(nc), function(k) {
      mk_long(year, CTY_CODE[toupper(trimws(xd[[3]]))], dd, rep(h$name[k], nrow(xd)),
              rep(party_std(if (is.na(h$party[k])) "OTHER" else h$party[k]), nrow(xd)), num(xd[[5 + k]]))
    })
  })
}

## ---- 2012 / 2014: town rows carry no county; "<County> County Totals" rows follow each county ----
## Returns county-level long rows (summing town rows, county taken from the next totals row) and,
## separately, the printed county totals for verification.
parse_county_totals_sheet <- function(f, sheet, year, district, vote_cols, party_row, name_row, other_col = NULL,
                                      total_col) {
  x <- read_sheet(f, sheet)
  lab <- trimws(x[[1]])
  is_tot <- !is.na(lab) & grepl(" County Totals$", lab)
  cty_of_tot <- toupper(sub(" County Totals$", "", lab))
  is_town <- !is.na(lab) & !is_tot & !is.na(num(x[[vote_cols[1]]])) &
    !grepl("^(District \\d Totals|Totals|STATE UOCAVA|EMERGENCY UTILITY WORKERS)$", lab)
  ## county for each town row = county of the next totals row below it
  nxt <- rep(NA_character_, nrow(x)); cur <- NA_character_
  for (i in rev(seq_len(nrow(x)))) { if (is_tot[i]) cur <- cty_of_tot[i]; nxt[i] <- cur }
  cand <- unlist(x[name_row, vote_cols]); party <- unlist(x[party_row, vote_cols])
  tw <- x[is_town, ]; tw_cty <- nxt[is_town]
  long <- purrr::map_dfr(seq_along(vote_cols), function(k)
    mk_long(year, tw_cty, district, rep(cand[k], nrow(tw)), rep(party_std(party[k]), nrow(tw)), num(tw[[vote_cols[k]]])))
  if (!is.null(other_col))
    long <- bind_rows(long, mk_long(year, tw_cty, district, rep("(OTHER/WRITE-IN)", nrow(tw)), "OTHER", num(tw[[other_col]])))
  printed <- tibble(county = cty_of_tot[is_tot], printed_total_incl_blank = num(x[[total_col]][is_tot]),
                    printed_blank = num(x[[if (is.null(other_col)) vote_cols[length(vote_cols)] + 2 else other_col + 1]][is_tot]))
  list(long = long, printed = printed)
}

## ---- Party lookup for the last-name-only years (1990, 1992, 2000): from Wikipedia national articles ----
party_lookup <- tribble(
  ~year, ~district, ~cand,       ~party,
  1990,  "1",       "ANDREWS",   "DEM",   1990, "1", "EMERY",     "REP",
  1990,  "2",       "MCGOWAN",   "DEM",   1990, "2", "SNOWE",     "REP",
  1992,  "1",       "ANDREWS",   "DEM",   1992, "1", "BEAN",      "REP",
  1992,  "2",       "MCGOWAN",   "DEM",   1992, "2", "SNOWE",     "REP",   1992, "2", "CARTER", "OTHER",
  2000,  "1",       "ALLEN",     "DEM",   2000, "1", "AMERO",     "REP",   2000, "1", "STAPLES", "OTHER",
  2000,  "2",       "BALDACCI",  "DEM",   2000, "2", "CAMPBELL",  "REP"
)
apply_lookup <- function(df) {
  df %>% mutate(cand_key = toupper(gsub("[^A-Za-z]", "", cand))) %>%
    left_join(party_lookup %>% mutate(year = as.numeric(year)), by = c("year", "district", "cand_key" = "cand")) %>%
    mutate(party = dplyr::coalesce(party.x, party.y)) %>% select(-party.x, -party.y, -cand_key)
}

## ---- Build all years ----------------------------------------------------------------------
long_1990s <- bind_rows(
  parse_1990s_a("1990_house.xlsx", 1990),
  parse_1990s_a("1992_house.xlsx", 1992),
  parse_1994("1994_house.xlsx", 1994),
  parse_1996("1996_house.xlsx", 1996),
  parse_1998("1998_house.xlsx", 1998),
  parse_2000("2000_house.xlsx", 2000)
)
long_2000s <- bind_rows(
  parse_quad_sheet("2002_house.xlsx", 2002, 4, TRUE),
  parse_quad_sheet("2004_house.xlsx", 2004, 3, FALSE),
  parse_quad_sheet("2006_house.xlsx", 2006, 5, TRUE),
  parse_2008(),
  parse_2010("2010_house.xlsx", 2010)
)

## 2014: CG1 vote cols 2-4 (party row 3, name row 1), Others col 5, Blank col 6, Total col 7
p14_1 <- parse_county_totals_sheet("2014_house.xlsx", "CG1", 2014, "1", 2:4, party_row = 3, name_row = 1, other_col = 5, total_col = 7)
p14_2 <- parse_county_totals_sheet("2014_house.xlsx", "CG2", 2014, "2", 2:4, party_row = 3, name_row = 1, other_col = 5, total_col = 7)
long_2014 <- bind_rows(p14_1$long, p14_2$long)

long_all <- bind_rows(long_1990s, long_2000s, long_2014) %>%
  apply_lookup()

## 1992 CD1 has a placeholder "NONE" candidate slot (uncontested-third-slot filler). Only drop it after
## confirming it carries no votes at all.
stopifnot(sum(long_all$votes[long_all$cand == "NONE"]) == 0)
long_all <- long_all %>% filter(cand != "NONE")

## Guard: every candidate row must have a party by now.
if (any(is.na(long_all$party))) {
  print(long_all %>% filter(is.na(party)) %>% group_by(year, district, cand) %>% summarise(v = sum(votes), .groups = "drop"), n = 50)
  stop("candidates with no party")
}
## Guard: no pseudo-total / stray rows sneaking through as "candidates"
bad <- long_all %>% filter(grepl("TOTAL|BLANK|UOCAVA|OVERSEAS|EMERGENCY", cand))
if (nrow(bad) > 0) { print(bad); stop("pseudo-total-looking candidate rows present") }

## ---- Aggregate to county-year ----------------------------------------------------------------
me_hist <- long_all %>%
  group_by(year, county) %>%
  summarise(totalvote = sum(votes, na.rm = TRUE),
            demovote_n = sum(votes[party == "DEM"], na.rm = TRUE),
            repuvote_n = sum(votes[party == "REP"], na.rm = TRUE),
            .groups = "drop") %>%
  left_join(me_fips, by = c("county" = "county_name"))

stopifnot(!any(is.na(me_hist$county_fips)))

elect_he_cty_me_historical <- me_hist %>%
  filter(totalvote > 0) %>%
  transmute(state = "MAINE", year, cty_fips = county_fips, sample = "HE",
            demovote = demovote_n / totalvote, repuvote = repuvote_n / totalvote, totalvote) %>%
  arrange(year, cty_fips) %>%
  save_step("elect_he_cty_me_historical")

## ---- Verification -------------------------------------------------------------------------------
message("\n== Rows per year (expect 16 = all Maine counties) ==")
print(table(elect_he_cty_me_historical$year))

sanity <- elect_he_cty_me_historical$repuvote + elect_he_cty_me_historical$demovote
message("Sanity: repuvote+demovote range [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
print(elect_he_cty_me_historical %>% mutate(s = repuvote + demovote) %>% group_by(year) %>%
        summarise(min_s = min(s), max_s = max(s), statewide_total = sum(totalvote), .groups = "drop"))

## (1) Statewide per-district candidate shares vs the percentages Wikipedia prints for each race.
message("\n== Statewide district candidate shares (compare to Wikipedia's printed %) ==")
share_tbl <- long_all %>% group_by(year, district, cand, party) %>% summarise(v = sum(votes), .groups = "drop") %>%
  group_by(year, district) %>% mutate(pct = round(100 * v / sum(v), 2)) %>% ungroup() %>%
  filter(pct > 1) %>% arrange(year, district, desc(v))
print(share_tbl, n = 200)

## (2) 2014: our town-sum county totals vs the printed "<County> County Totals" rows (candidates + others,
##     i.e. printed total minus printed blanks).
message("\n== 2014: town-sum vs printed county totals ==")
chk14 <- bind_rows(mutate(p14_1$printed, district = "1"), mutate(p14_2$printed, district = "2")) %>%
  mutate(printed_valid = printed_total_incl_blank - printed_blank) %>%
  left_join(p14_1$long %>% bind_rows(p14_2$long) %>% group_by(county, district) %>% summarise(mine = sum(votes), .groups = "drop"),
            by = c("county", "district"))
print(chk14 %>% mutate(diff = mine - printed_valid) %>% filter(diff != 0 | is.na(diff)))
message("2014 county-district cells checked: ", nrow(chk14), "; mismatches: ", sum(chk14$mine != chk14$printed_valid, na.rm = TRUE))

## (3) Method validation on 2012: same county-totals parser vs the existing OpenElections-based 2012 build.
p12_1 <- parse_county_totals_sheet("2012_house_cd1.xlsx", 1, 2012, "1", c(2, 4), party_row = 6, name_row = 4, other_col = NULL, total_col = 8)
p12_2 <- parse_county_totals_sheet("2012_house_cd2.xlsx", 1, 2012, "2", c(2, 4), party_row = 6, name_row = 4, other_col = NULL, total_col = 8)
me12 <- bind_rows(p12_1$long, p12_2$long) %>% group_by(county) %>%
  summarise(totalvote = sum(votes), demo = sum(votes[party == "DEM"]) / sum(votes), .groups = "drop") %>%
  left_join(me_fips, by = c("county" = "county_name"))
oe12 <- readRDS(file.path(OUTPUT_DIR, "elect_he_cty_me.rds")) %>% select(cty_fips, oe_total = totalvote, oe_demo = demovote)
cmp12 <- me12 %>% inner_join(oe12, by = c("county_fips" = "cty_fips"))
message("\n== 2012 xlsx-parser vs OpenElections 01af: ", nrow(cmp12), " counties matched; max |total diff| = ",
        max(abs(cmp12$totalvote - cmp12$oe_total)), "; max |dem share diff| = ", round(max(abs(cmp12$demo - cmp12$oe_demo)), 6))

message("\nSaved: ", file.path(OUTPUT_DIR, "elect_he_cty_me_historical.rds"))
