## Illinois 1990 and 1992 U.S. House by county, from the Illinois State Board of Elections official-vote books (scanned, OCR text layer):
##   R/data/county_house_files/IL_1990 ge_639255245981392664.pdf (pp. 35-48 = book pp. 25-38) and IL_1992 ge_639255246078653873.pdf (pp. 57-69).
## Per district block: candidate totals + percentages (checksums), then a county table with plurality and one vote column per candidate; a county in 2+ districts appears in each block.
## Pipeline: (1) R/data/raw_house_county_open_states/illinois_official/il_parse_ocr_1990_1992.py parses the OCR text; (2) il_build_transcription_1990_1992.py cleans numbers, maps county names
## (garbled OCR names by fuzzy match + explicit overrides) and applies the 4 corrections read from page images (R/output/il_ocr_corrections_1990_1992.csv);
## (3) this script builds the long tables and runs the acceptance checks: county sums == printed candidate totals, printed percentages, county count per district, House total vs
## Senate/governor (1990) or presidential (1992) total in the panel. Illinois had 22 districts in 1990 and 20 in 1992.
## Outputs: R/output/long/he_il_1990.rds, he_il_1992.rds and R/output/elect_he_cty_il_1990.rds, elect_he_cty_il_1992.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "illinois_official")
tr <- read_csv(file.path(D, "il_1990_1992_transcription.csv"), col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), district = as.integer(district), county_fips = as.integer(county_fips), votes = as.numeric(votes))
pt <- read_csv(file.path(D, "il_1990_1992_printed_totals.csv"), col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), district = as.integer(district), printed = as.numeric(printed_total_ocr_clean))
## ---- candidate names (OCR-cleaned by hand; punctuation restored after finalize_long) ------------------------------------------------------------------------------
NAMES <- c("CHARLES A. HAYES" = "Charles A. Hayes", "BABETTE PEYTON" = "Babette Peyton", "GUS SAVAGE" = "Gus Savage", "WILLIAM T. HESPEL" = "William T. Hespel", "MARTIN A. RUSSO" = "Martin A. Russo",
  "CARLL. KLEIN" = "Carl L. Klein", "GEORGE E. SANGMEISTER" = "George E. Sangmeister", "MANNY HOFFMAN" = "Manny Hoffman", "WILLIAM 0. LIPINSKI" = "William O. Lipinski", "DAVID J. SHESTOKAS" = "David J. Shestokas",
  "RONALD (RON) BARTOS" = "Ronald (Ron) Bartos", "HENRY J. HYDE" = "Henry J. Hyde", "ROBERT J. CASSIDY" = "Robert J. Cassidy", "CARDISS COLLINS" = "Cardiss Collins", "MICHAEL DOOLEY" = "Michael Dooley",
  "DAN ROSTENKOWSKI" = "Dan Rostenkowski", "ROBERT MARSHALL" = "Robert Marshall", "SIDNEY R. YATES" = "Sidney R. Yates", "HERBERT SOHN" = "Herbert Sohn", "JOHN E. PORTER" = "John E. Porter",
  "PEG McNAMARA" = "Peg McNamara", "HERBERT L. GORRELL" = "Herbert L. Gorrell", "FRANK ANNUNZIO" = "Frank Annunzio", "'riAL TER W. DUDYCZ" = "Walter W. Dudycz", "LARRY SASKA" = "Larry Saska",
  "PHILIP M. CRANE" = "Philip M. Crane", "STEVE PEDERSEN" = "Steve Pedersen", "HARRIS W. FAWELL" = "Harris W. Fawell", "STEVEN K. THOMAS" = "Steven K. Thomas", "J. DENNIS HASTERT" = "J. Dennis Hastert",
  "DONALD J. WESTPHAL" = "Donald J. Westphal", "EDWARD R. MADIGAN" = "Edward R. Madigan", "WILLIAM DECKER, JR" = "William Decker Jr.", "JOHN W. COX, JR" = "John W. Cox Jr.", "JOHN W. HALLOCK, JR" = "John W. Hallock Jr.",
  "LANE A. EVANS" = "Lane A. Evans", "DAN LEE" = "Dan Lee", "ROBERT H. MICHEL" = "Robert H. Michel", "WALTER GILLIN" = "Walter Gillin", "ALAN J. PORT" = "Alan J. Port", "TERRY L. BRUCE" = "Terry L. Bruce",
  "ROBERT F. KERANS" = "Robert F. Kerans", "BRIAN JAMES O'NEILL II" = "Brian James O'Neill II", "RICHARD J. DURBIN" = "Richard J. Durbin", "PAUL JURGENS" = "Paul Jurgens", "JERRY F. COSTELLO" = "Jerry F. Costello",
  "ROBERT H. GAFFNER" = "Robert H. Gaffner", "GLENN POSHARD" = "Glenn Poshard", "JIM WHAM" = "Jim Wham",
  "BOBBY L. RUSH" = "Bobby L. Rush", "JAY WALKER" = "Jay Walker", "MEL REYNOLDS" = "Mel Reynolds", "RON BLACKSTONE" = "Ron Blackstone", "LOUANNER PETERS" = "Louanner Peters", "HARRY C. LEPINSKE" = "Harry C. Lepinske",
  "LUIS V. GUTIERREZ" = "Luis V. Gutierrez", "HILDEGARDE RODRIGUEZ-SCHIEMAN" = "Hildegarde Rodriguez-Schieman", "ELIAS R. \"NON-INCUMBENT\" ZENKICH" = "Elias R. \"Non-Incumbent\" Zenkich", "BLAISE C. GRENKE" = "Blaise C. Grenke",
  "BARRY W. WATKINS" = "Barry W. Watkins", "KEITH JEKYLL PETROPOULOS" = "Keith Jekyll Petropoulos", "NORMAN G. BOCCIO" = "Norman G. Boccio", "ROSE-MARIE LOVE" = "Rose-Marie Love", "GERI KNOLL McLAUCHLAN" = "Geri Knoll McLauchlan",
  "SHEILA A. SMITH" = "Sheila A. Smith", "JOE M. DILLIER" = "Joe M. Dillier", "HERB SOHN" = "Herb Sohn", "SHE I LA A. JONES" = "Sheila A. Jones", "MICHAEL J. KENNEDY" = "Michael J. Kennedy",
  "ROBERT T. HERBOLSHEIMER" = "Robert T. Herbolsheimer", "MIKE STARR" = "Mike Starr", "DENNIS MICHAEL TEMPLE" = "Dennis Michael Temple", "RALPH M. MIRON" = "Ralph M. Miron", "JONATHAN ABRAM REICH" = "Jonathan Abram Reich",
  "YVONNE DINWIDDIE" = "Yvonne Dinwiddie", "THOMAS W. EWING" = "Thomas W. Ewing", "CHARLES D. MATTIS" = "Charles D. Mattis", "GERARD ARCHIBALD" = "Gerard Archibald", "DONALD MANZULLO" = "Donald Manzullo",
  "KEN SCHLOEMER" = "Ken Schloemer", "RONALD C. HAWKINS" = "Ronald C. Hawkins", "CARL L. GIFFORD" = "Carl L. Gifford", "GLENN PO SHARD" = "Glenn Poshard", "DOUGLAS E. LEE" = "Douglas E. Lee", "PAT RIKER" = "Pat Riker",
  "JOHN M. SHIMKUS" = "John M. Shimkus", "BARRY W. WATKINS" = "Barry W. Watkins")
stopifnot(all(c(tr$candidate_ocr, pt$candidate_ocr) %in% names(NAMES)))
PARTY <- c(DEM = "Democratic", REP = "Republican", SOL = "Illinois Solidarity", LIB = "Libertarian", JWP = "Jim Wham Party", LPP = "Louanner Peters Party", ERP = "Economic Recovery Party", LAW = "Natural Law",
           ICP = "Independent Congressional Party", `W-I` = "Write-In")
tr <- tr %>% mutate(candidate = NAMES[candidate_ocr], party_group = case_when(party_code == "DEM" ~ "DEM", party_code == "REP" ~ "REP", TRUE ~ "OTHER"), party = PARTY[party_code])   # party_group BEFORE party is replaced by its label
stopifnot(!anyNA(tr$party), !anyNA(tr$candidate))
pt <- pt %>% mutate(candidate = NAMES[candidate_ocr])
## ---- acceptance checks --------------------------------------------------------------------------------------------------------------------------------------
ck <- tr %>% group_by(year, district, candidate) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% full_join(pt %>% select(year, district, candidate, printed, printed_pct_ocr), by = c("year", "district", "candidate")) %>%
  group_by(year, district) %>% mutate(share = 100 * sum / sum(sum), pct = suppressWarnings(as.numeric(gsub("[^0-9.]", "", gsub(" ", "", printed_pct_ocr)))), pct_ok = abs(share - pct) < 0.011) %>% ungroup() %>%
  mutate(tot_ok = ifelse(is.na(printed), NA, sum == printed))
cat("candidate lines:", nrow(ck), "| county sums == printed total:", sum(ck$tot_ok, na.rm = TRUE), "| printed total garbled in OCR (checked by the printed percentage instead):", sum(is.na(ck$printed) | !ck$tot_ok), "| percentage matches:", sum(ck$pct_ok, na.rm = TRUE), "\n")
print(as.data.frame(ck %>% filter(!tot_ok | is.na(tot_ok) | !pct_ok) %>% select(year, district, candidate, sum, printed, share, pct)))
cnt <- tr %>% distinct(year, district, county_fips) %>% count(year, district) %>% tidyr::pivot_wider(names_from = year, values_from = n); print(as.data.frame(cnt))
raw <- tr %>% transmute(year, county_fips, district = sprintf("%02d", district), candidate, party, party_group, votes)
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
for (y in c(1990, 1992)) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("il_", y))
  ## finalize_long strips punctuation from names: restore the hand-cleaned names (its canonical form is letters-only)
  key <- function(z) gsub("[^A-Za-z]", "", toupper(z)); m <- setNames(unname(NAMES), key(NAMES)); nm <- unique(raw$candidate[raw$year == y]); m2 <- setNames(nm, key(nm))
  long$candidate <- ifelse(key(long$candidate) %in% names(m2), m2[key(long$candidate)], long$candidate)
  save_long(long, paste0("he_il_", y))
  shares <- derive_shares(long) %>% transmute(state = "ILLINOIS", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_il_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_il_%d.rds", y))); stopifnot(all(r$pass))
  refs <- if (y == 1990) list(SE = 1990, GOV = NA) else list(PE = 1992)
  ref <- panel %>% filter(sample == ifelse(y == 1990, "SE", "PE"), year == y) %>% select(cty_fips, ref = totalvote)
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(sprintf("%d: counties %d | districts %d | split counties %d | median dem %.3f rep %.3f | House / %s total: n=%d min %.2f med %.2f max %.2f\n", y, n_distinct(long$county_fips), n_distinct(long$district),
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), median(shares$demovote), median(shares$repuvote), ifelse(y == 1990, "Senate", "presidential"), sum(!is.na(rr$ratio)),
      suppressWarnings(min(rr$ratio, na.rm = TRUE)), median(rr$ratio, na.rm = TRUE), suppressWarnings(max(rr$ratio, na.rm = TRUE))))
  print(as.data.frame(rr %>% filter(ratio < 0.8 | ratio > 1.1) %>% arrange(ratio) %>% select(cty_fips, totalvote, ref, ratio) %>% head(12)))
}
