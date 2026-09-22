## Candidate -> party overrides for MEDSL 2024 rows whose party label is BLANK for every county of a district
## (New Jersey and Oregon 2024; see data_corrections_log.csv). read_precinct_file() in 01a first tries to copy a
## candidate's party from the labelled rows of the SAME file; this table is the last resort for candidates with no
## labelled row anywhere.
##
## Source: nominees and their parties from Wikipedia's "2024 United States House of Representatives elections in New
## Jersey / Oregon" (district infoboxes, fetched 2026-09-20) plus the NJ Senate race (Andy Kim D, Curtis Bashaw R; the
## MEDSL Senate file labels both in some counties). Only Democrats and Republicans need listing; every other blank-party
## candidate stays OTHER, which is right for third-party/write-in candidates.
## Key = state + district (MEDSL's 3-digit string, or STATEWIDE) + SURNAME KEY = last token of the normalized candidate name
## after dropping JR/SR/II/III/IV (same normalization as 01a: uppercase letters and spaces only). This script checks that
## every key really occurs among the raw candidate strings, so a spelling mismatch cannot silently do nothing.
## Output: R/data/raw_election/candidate_party_overrides.csv

source(file.path("R", "00_setup.R"))
library(readr)

ov <- tribble(
  ~state, ~district, ~surname_key, ~party_std,
  # ---- New Jersey House 2024 ----
  "NEW JERSEY", "001", "NORCROSS", "DEMOCRAT",   "NEW JERSEY", "001", "LIDDELL", "REPUBLICAN",
  "NEW JERSEY", "002", "SALERNO", "DEMOCRAT",    "NEW JERSEY", "002", "DREW", "REPUBLICAN",        # Jeff Van Drew
  "NEW JERSEY", "003", "CONAWAY", "DEMOCRAT",    "NEW JERSEY", "003", "MOHAN", "REPUBLICAN",
  "NEW JERSEY", "004", "JENKINS", "DEMOCRAT",    "NEW JERSEY", "004", "SMITH", "REPUBLICAN",
  "NEW JERSEY", "005", "GOTTHEIMER", "DEMOCRAT", "NEW JERSEY", "005", "GUINCHARD", "REPUBLICAN",
  "NEW JERSEY", "006", "PALLONE", "DEMOCRAT",    "NEW JERSEY", "006", "FEGLER", "REPUBLICAN",
  "NEW JERSEY", "007", "ALTMAN", "DEMOCRAT",     "NEW JERSEY", "007", "KEAN", "REPUBLICAN",
  "NEW JERSEY", "008", "MENENDEZ", "DEMOCRAT",   "NEW JERSEY", "008", "VALDES", "REPUBLICAN",
  "NEW JERSEY", "009", "POU", "DEMOCRAT",        "NEW JERSEY", "009", "PREMPEH", "REPUBLICAN",
  "NEW JERSEY", "010", "MCIVER", "DEMOCRAT",     "NEW JERSEY", "010", "BUCCO", "REPUBLICAN",
  "NEW JERSEY", "011", "SHERRILL", "DEMOCRAT",   "NEW JERSEY", "011", "BELNOME", "REPUBLICAN",
  "NEW JERSEY", "012", "COLEMAN", "DEMOCRAT",    "NEW JERSEY", "012", "MAYFIELD", "REPUBLICAN",  # Bonnie Watson Coleman
  # ---- New Jersey Senate 2024 ----
  "NEW JERSEY", "STATEWIDE", "KIM", "DEMOCRAT",  "NEW JERSEY", "STATEWIDE", "BASHAW", "REPUBLICAN",
  # ---- Oregon House 2024 ----
  "OREGON", "001", "BONAMICI", "DEMOCRAT",       "OREGON", "001", "TODD", "REPUBLICAN",
  "OREGON", "002", "RUBY", "DEMOCRAT",           "OREGON", "002", "BENTZ", "REPUBLICAN",
  "OREGON", "003", "DEXTER", "DEMOCRAT",         "OREGON", "003", "HARBOUR", "REPUBLICAN",
  "OREGON", "004", "HOYLE", "DEMOCRAT",          "OREGON", "004", "DESPAIN", "REPUBLICAN",
  "OREGON", "005", "BYNUM", "DEMOCRAT",          "OREGON", "005", "CHAVEZDEREMER", "REPUBLICAN",
  "OREGON", "006", "SALINAS", "DEMOCRAT",        "OREGON", "006", "ERICKSON", "REPUBLICAN"
)

## ---- verify every key occurs in the raw candidate strings (same key function as 01a) ------------------------------------
norm_cand <- function(x) trimws(gsub("\\s+", " ", gsub("[^A-Z ]", "", toupper(x))))
surname_key <- function(x) vapply(strsplit(norm_cand(x), " "), function(tk) { tk <- tk[!tk %in% c("JR", "SR", "II", "III", "IV", "V", "")]; if (length(tk)) tail(tk, 1) else "" }, "")
raw_names <- bind_rows(
  read_csv(file.path(PROJECT_ROOT, "R/data/raw_election/house_2024.raw"), col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE) %>%
    filter(state %in% c("NEW JERSEY", "OREGON")) %>% distinct(state, district, candidate),
  read_csv(file.path(PROJECT_ROOT, "R/data/raw_election/senate_2024.raw"), col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE) %>%
    filter(state == "NEW JERSEY") %>% distinct(state, district, candidate)) %>%
  mutate(surname_key = surname_key(candidate))
chk <- ov %>% left_join(raw_names %>% group_by(state, district, surname_key) %>% summarise(example = first(candidate), .groups = "drop"),
                        by = c("state", "district", "surname_key"))
if (anyNA(chk$example)) { print(as.data.frame(chk %>% filter(is.na(example)))); stop("override keys that match no raw candidate") }
message("all ", nrow(ov), " override keys match a raw candidate string; e.g.:")
print(as.data.frame(head(chk %>% select(state, district, surname_key, party_std, example), 8)))
stopifnot(!anyDuplicated(ov[, c("state", "district", "surname_key")]))
write_csv(ov, file.path(PROJECT_ROOT, "R/data/raw_election/candidate_party_overrides.csv"))
message("wrote candidate_party_overrides.csv (", nrow(ov), " rows)")
