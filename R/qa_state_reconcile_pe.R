## Independent check of the PRESIDENT county rows: sum our release table by state and compare with the FEC's official state results (R/data/fec_official/).
## FEC "Federal Elections <year>" workbooks (fec.gov, U.S. government work) list every candidate's certified state total and the state's total votes; 2000 gives state totals only (2000tables.xls).
## We compare (a) the state total, (b) the votes of each official candidate with >= 1% of the state vote (matched on last name; New York uses the FEC's combined-lines column).
## Scope = the release: 48 states (no AK, HI, DC), 2000-2024 available years. Output: R/output/qa_state_reconcile_pe.csv (one row per state-year-check) and a printed summary.
source(file.path("R", "00_setup.R")); library(readxl); library(readr)
FEC <- file.path(PROJECT_ROOT, "R", "data", "fec_official")
SKIP <- c("AK", "HI", "DC")
key <- function(x) { x <- toupper(iconv(x, to = "ASCII//TRANSLIT")); gsub("[^A-Z]", "", x) }   # letters only
key_ours <- function(x) { x <- toupper(iconv(x, to = "ASCII//TRANSLIT")); x <- gsub("[.,]?\\s+(JR|SR|II|III|IV)\\.?$", "", trimws(x)); key(sub("^.*\\s", "", x)) }   # our names are "First Last": last token, suffix stripped

read_pres <- function(file, sheet, year) {
  d <- suppressMessages(read_excel(file.path(FEC, file), sheet = sheet, col_names = FALSE, .name_repair = "minimal")); hdr <- toupper(as.character(unlist(d[1, ])))
  num <- function(i) suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(d[[i]]))))
  ci <- function(pat) which(grepl(pat, hdr))[1]
  st <- ci("STATE ABBREV"); ln <- ci("^LAST NAME$"); pty <- ci("^PARTY"); gr <- ci("GENERAL RESULTS"); tv <- which(grepl("TOTAL VOTES #", hdr)); cmb <- which(grepl("COMBINED GE PARTY TOTALS", hdr))
  d <- d[-1, ]; num2 <- function(i) suppressWarnings(as.numeric(as.character(d[[i]])))
  is_tot <- grepl("Total State Votes", as.character(d[[which(grepl("LAST NAME,", hdr))[1] + 1]]), ignore.case = TRUE)   # the label sits in the column right after "LAST NAME, FIRST"
  tot_val <- ifelse(is_tot, coalesce(num2(gr), if (length(tv)) num2(tv[1]) else NA_real_, if (length(tv) > 1) num2(tv[2]) else NA_real_), NA_real_)
  cand_val <- num2(gr); cmb_val <- if (length(cmb)) num2(cmb[1]) else rep(NA_real_, nrow(d))
  out <- tibble(year = year, state_po = as.character(d[[st]]), last = as.character(d[[ln]]), party = as.character(d[[pty]]), votes = ifelse(is_tot, NA, cand_val), combined = ifelse(is_tot, NA, cmb_val), total = tot_val)
  ## the total row carries no state abbreviation on some sheets; fill down within blocks
  out <- out %>% tidyr::fill(state_po, .direction = "down")
  tots <- out %>% filter(!is.na(total)) %>% transmute(year, state_po, official_total = total)
  ## a candidate on several party lines (New York) is listed once per line: use the FEC's combined column when it exists, else add the lines
  cands <- out %>% filter(!is.na(votes), !is.na(last), !grepl("Scattered|Other", last, ignore.case = TRUE)) %>% mutate(mj = toupper(party) %in% c("D", "R", "DEM", "REP")) %>%
    group_by(state_po, last) %>% summarise(party = first(party[mj], default = first(party)), votes = ifelse(any(!is.na(combined)), max(combined, na.rm = TRUE), ifelse(n() > 1 && abs(max(votes) - (sum(votes) - max(votes))) < 0.005 * max(votes), max(votes), sum(votes))), .groups = "drop") %>% mutate(year = year) %>% transmute(year, state_po, cand_key = key(sub("\\s+(JR|SR|II|III|IV)\\.?$", "", last, ignore.case = TRUE)), cand = last, party, votes)
  list(tots = tots, cands = cands %>% filter(!state_po %in% SKIP))
}
srcs <- list(list("2004pres.xls", "2004 PRES GENERAL RESULTS", 2004), list("2008pres.xls", "2008 PRES GENERAL RESULTS", 2008), list("2012pres.xls", "2012 Pres General Results", 2012),
             list("federalelections2016.xlsx", "2016 Pres General Results", 2016), list("federalelections2020.xlsx", "9. 2020 Pres General Results", 2020))
off <- lapply(srcs, function(s) do.call(read_pres, s)); tots <- bind_rows(lapply(off, `[[`, "tots")); cands <- bind_rows(lapply(off, `[[`, "cands"))
tots <- tots %>% filter(!state_po %in% SKIP, nchar(state_po) == 2)
## 2000: state totals only (2000tables.xls)
t00 <- suppressMessages(read_excel(file.path(FEC, "2000tables.xls"), sheet = 1, col_names = FALSE)); t00 <- t00[-(1:2), 1:2]; names(t00) <- c("state_po", "official_total")
t00 <- t00 %>% filter(nchar(state_po) == 2, !is.na(official_total)) %>% transmute(year = 2000, state_po, official_total = as.numeric(official_total))
tots <- bind_rows(t00 %>% filter(!state_po %in% SKIP), tots)
## 2024: the FEC has not published a 2024 book; Wikipedia's "Results by state" table (compiled from each state's certified totals; secondary source) supplies the two nominees and the state total
w24 <- read_csv(file.path(FEC, "pres2024_wikipedia_state_totals.csv"), show_col_types = FALSE) %>% filter(!state_po %in% SKIP)
tots <- bind_rows(tots, w24 %>% transmute(year = 2024, state_po, official_total = total))
cands <- bind_rows(cands, w24 %>% transmute(year = 2024, state_po, cand_key = "TRUMP", cand = "Trump", party = "R", votes = trump), w24 %>% transmute(year = 2024, state_po, cand_key = "HARRIS", cand = "Harris", party = "D", votes = harris))
ours_years <- c(2000, 2004, 2008, 2012, 2016, 2020, 2024)
n00 <- read_csv(file.path(FEC, "pres2000_nominees.csv"), show_col_types = FALSE) %>% filter(!state_po %in% SKIP) %>% transmute(year = 2000, state_po, cand_key = toupper(last), cand = last, party, votes)   # 01dk_fec_2000_pres_parse.py (OCR text of the FEC 2000 book)
cands <- bind_rows(n00, cands)
cat("official state totals:", nrow(tots), "state-years | candidates:", nrow(cands), "\n")

rel <- read_csv(file.path(PROJECT_ROOT, "release", "v0.2.0", "us_county_results_long.csv"), col_types = cols(.default = "c", votes = "d", year = "i"), show_col_types = FALSE) %>% filter(office == "president")
ours_tot <- rel %>% group_by(year, state_po) %>% summarise(our_total = sum(votes), n_counties = n_distinct(county_fips), .groups = "drop")
ours_cand <- rel %>% mutate(cand_key = key_ours(candidate)) %>% group_by(year, state_po, cand_key) %>% summarise(our_votes = sum(votes), .groups = "drop")

chk_tot <- tots %>% full_join(ours_tot %>% filter(year %in% ours_years), by = c("year", "state_po")) %>% mutate(diff = our_total - official_total, rel = diff / official_total, check = "state_total")
big <- cands %>% filter(toupper(party) %in% c("D", "R", "DEM", "REP"))   # the two nominees; minor candidates are not comparable (MEDSL lumps most of them into "Other")
chk_cand <- big %>% left_join(ours_cand, by = c("year", "state_po", "cand_key")) %>% mutate(our_votes = coalesce(our_votes, 0), diff = our_votes - votes, rel = diff / votes, check = paste0("cand:", cand))
res <- bind_rows(chk_tot %>% transmute(year, state_po, check, official = official_total, ours = our_total, diff, rel, n_counties),
                 chk_cand %>% transmute(year, state_po, check, official = votes, ours = our_votes, diff, rel, n_counties = NA_integer_))
res <- res %>% mutate(status = case_when(is.na(ours) | is.na(official) ~ "MISSING", abs(rel) < 0.0005 ~ "ok (<0.05%)", abs(rel) < 0.005 ~ "small (<0.5%)", abs(rel) < 0.02 ~ "check (0.5-2%)", TRUE ~ "BAD (>=2%)"))
write_csv(res, file.path(OUTPUT_DIR, "qa_state_reconcile_pe.csv"))
cat("\nstate-year TOTAL check by year and status:\n"); print(as.data.frame(res %>% filter(check == "state_total") %>% count(year, status) %>% tidyr::pivot_wider(names_from = status, values_from = n, values_fill = 0)))
cat("\ncandidate checks (>=1% candidates) by status:\n"); print(as.data.frame(res %>% filter(check != "state_total") %>% count(status)))
cat("\nworst state totals:\n"); print(as.data.frame(res %>% filter(check == "state_total", status != "ok (<0.05%)") %>% arrange(desc(abs(rel))) %>% head(40) %>% mutate(rel = round(rel, 4))))
cat("\nstate-years with a total off by >= 0.5%, split into the two nominees and everybody else:\n")
nom <- res %>% filter(check != "state_total") %>% group_by(year, state_po) %>% summarise(off_nom = sum(official), our_nom = sum(ours), .groups = "drop")
dec <- res %>% filter(check == "state_total", abs(rel) >= 0.005) %>% transmute(year, state_po, off_tot = official, our_tot = ours, n_counties) %>% left_join(nom, by = c("year", "state_po")) %>%
  mutate(nominee_diff = our_nom - off_nom, other_diff = (our_tot - our_nom) - (off_tot - off_nom), off_other_pct = round(100 * (off_tot - off_nom) / off_tot, 1), our_other_pct = round(100 * (our_tot - our_nom) / our_tot, 1)) %>% arrange(year, state_po)
print(as.data.frame(dec)); write_csv(dec, file.path(OUTPUT_DIR, "qa_state_reconcile_pe_decomposition.csv"))
