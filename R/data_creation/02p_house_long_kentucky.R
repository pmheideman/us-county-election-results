## Candidate-level LONG table for Kentucky, all years 1990-2016 (source files elect_he_cty_ky.rds = union of 01f 1990-98, 01g 2000-08,
## 01i 2010, 01h 2012-16; the later three are also checked separately). Logic copied from those scripts (cached raw files only, nothing
## downloaded, nothing folded into the panel):
##  * 1990s (01f): text files, party CODES in the header; party_group by startsWith DEM/REP. Candidate names are read from the header lines
##    between the party row and the dashed rule by aligning words to the party-token columns; if the aligned name count does not equal the
##    party count for a district, the party label is used as the candidate name (reported below).
##  * 2000s (01g): "OFFICE: A04/<district>/000" blocks by county code; ambiguous codes GREE/MCCR resolved by magnitude, GREU/MCCK explicit;
##    party by last name from ky_wiki_party_lookup.csv (unmatched -> OTHER).
##  * 2010 (01i): county x party tables transcribed by hand from a scanned PDF (values embedded in 01i; parsed out of that script's text so
##    there is one copy); candidate = surname as printed on the ballot page, minor/write-in columns pooled as "Other candidates".
##  * 2012-2016 (01h): last names from the PDF text; full names from the lookup where available.
source(file.path("R", "00_setup.R")); library(readr); source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)      # SAFETY: writes only under R/output/long/
RAWK <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states")
xw <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "KENTUCKY")
KY_COUNTIES <- xw %>% distinct(county_name, county_fips)
party_lookup <- read_csv(file.path(RAWK, "kentucky_2000s", "ky_wiki_party_lookup.csv"), show_col_types = FALSE)
last_name_of <- function(x) toupper(gsub("[^A-Za-z]", "", sapply(strsplit(trimws(x), "\\s+"), function(w) w[length(w)])))
grp <- function(p) case_when(p == "DEM" ~ "DEM", p == "REP" ~ "REP", TRUE ~ "OTHER")
name_report <- list()

## ================= 1990s =================
ORDINAL_RE <- "\\b(1ST|2ND|3RD|4TH|5TH|6TH|7TH|8TH|9TH)\\b"
parse_1990s <- function(path, year) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  hdr_idx <- which(grepl(ORDINAL_RE, lines, ignore.case = TRUE) & grepl("REPRESENTATIVE", lines, ignore.case = TRUE))
  party_row_re <- "^[[:space:]]*([A-Z]{2,12}[[:space:]]*)+$"
  is_real_party_row <- function(line) grepl(party_row_re, line) && !grepl("REPRESENTATIVE|DISTRICT|UNITED STATES|GENERAL ELECTION", line)
  out <- list()
  for (h in hdr_idx) {
    district <- as.integer(regmatches(lines[h], regexpr("[1-9](?=(ST|ND|RD|TH))", toupper(lines[h]), perl = TRUE)))
    j <- h + 1
    while (j <= length(lines) && !is_real_party_row(lines[j])) { j <- j + 1; if (j > min(h + 6, length(lines))) break }
    if (j > length(lines) || !is_real_party_row(lines[j])) next
    parties <- strsplit(trimws(lines[j]), "\\s+")[[1]]
    dash_idx <- which(grepl("^-{20,}", lines[(j + 1):min(j + 20, length(lines))])); if (length(dash_idx) == 0) next
    data_start <- j + dash_idx[1]
    dash2 <- which(grepl("^-{20,}", lines[(data_start + 1):length(lines)]))
    data_end <- if (length(dash2) > 0) data_start + dash2[1] - 1 else length(lines)
    ## candidate names: words on the lines between the party row and the dashed rule, assigned to the party column at/left of the word start
    pstart <- as.integer(gregexpr("\\S+", lines[j])[[1]])
    nm <- rep("", length(parties))
    for (nl in lines[(j + 1):(data_start - 1)]) {
      if (trimws(nl) == "") next
      ws <- gregexpr("\\S+", nl)[[1]]; wt <- regmatches(nl, list(ws))[[1]]
      for (w in seq_along(wt)) { k <- max(which(pstart <= ws[w] + 4), 1); nm[k] <- trimws(paste(nm[k], wt[w])) }
    }
    ok <- all(nzchar(nm)); name_report[[length(name_report) + 1]] <<- data.frame(year = year, district = district, names_ok = ok)
    cand <- if (ok) nm else parties
    data_lines <- lines[(data_start + 1):data_end]; data_lines <- data_lines[trimws(data_lines) != ""]
    for (dl in data_lines) {
      toks <- strsplit(trimws(gsub("\t", " ", dl)), "\\s+")[[1]]
      if (length(toks) != length(parties) + 1) next
      votes <- suppressWarnings(as.numeric(gsub(",", "", toks[-1]))); if (any(is.na(votes))) next
      out[[length(out) + 1]] <- tibble(year = year, district = district, county_name = toks[1], party_raw = parties, candidate = cand, votes = votes)
    }
  }
  bind_rows(out)
}
f90 <- c(`1990` = "1990-1999_1990_90usrep.txt", `1992` = "1990-1999_1992_92usrep.txt", `1994` = "1990-1999_1994_94gen_USRep.txt",
         `1996` = "1990-1999_1996_96Gen_usrep1.txt", `1998` = "1990-1999_1998_98Gen_usrep.txt")
k90 <- bind_rows(lapply(names(f90), function(y) parse_1990s(file.path(RAWK, "kentucky", f90[[y]]), as.integer(y)))) %>%
  left_join(KY_COUNTIES, by = "county_name") %>% filter(!is.na(county_fips)) %>%
  transmute(year, county_fips, district, candidate, party = party_raw,
            party_group = case_when(startsWith(party_raw, "DEM") ~ "DEM", startsWith(party_raw, "REP") ~ "REP", TRUE ~ "OTHER"), votes)

## ================= 2000s =================
AMBIGUOUS_CODES <- c("GREE", "MCCR"); AMB <- list(GREE = c("GREEN", "GREENUP"), MCCR = c("MCCRACKEN", "MCCREARY"))
SPECIAL_CODES <- c(GREU = "GREENUP", MCCK = "MCCRACKEN")
parse_2000s <- function(path, year) {
  lines <- readLines(path, warn = FALSE, encoding = "latin1")
  any_idx <- grep("OFFICE:", lines); office_idx <- grep("OFFICE:\\s*A04/\\d+/000", lines)
  out <- list()
  for (oi in seq_along(office_idx)) {
    start <- office_idx[oi]; nxt <- any_idx[any_idx > start]; end <- if (length(nxt) > 0) min(nxt) - 1 else length(lines)
    district <- as.integer(gsub(".*A04/(\\d+)/000.*", "\\1", lines[start]))
    block <- lines[(start + 1):end]; is_blank <- trimws(block) == ""; cid <- cumsum(is_blank); cid[is_blank] <- NA
    for (chunk in split(block[!is_blank], cid[!is_blank])) {
      if (length(chunk) < 2 || !grepl("\\*[A-Z]{3,6}", chunk[1])) next
      codes <- gsub("\\*", "", regmatches(chunk[1], gregexpr("\\*[A-Z]{3,6}", chunk[1]))[[1]])
      for (row in chunk[-1]) {
        m <- regmatches(row, regexec("^(.*?)\\s+((?:[\\d,]+\\s*)+)$", row, perl = TRUE))[[1]]; if (length(m) != 3) next
        votes <- suppressWarnings(as.numeric(gsub(",", "", strsplit(trimws(m[3]), "\\s+")[[1]])))
        if (length(votes) != length(codes) || any(is.na(votes))) next
        out[[length(out) + 1]] <- tibble(year = year, district = district, county_code = codes, candidate = trimws(m[2]), votes = votes)
      }
    }
  }
  bind_rows(out)
}
f00 <- c(`2000` = "2000-2009_2000_00Gen_Statewidebycounty.txt", `2002` = "2000-2009_2002_General_20Election_2002statebycounty.txt",
         `2004` = "2000-2009_2004_General_20Election_2004statebyCOUNTY.txt", `2006` = "2000-2009_2006_General_20Election_STATEwidebycounty.txt",
         `2008` = "2000-2009_2008_General_20Election_STATEwide_20by_20candidate_20by_20county_20gen_2008.txt")
r00 <- bind_rows(lapply(names(f00), function(y) parse_2000s(file.path(RAWK, "kentucky_2000s", f00[[y]]), as.integer(y)))) %>%
  mutate(last_name = last_name_of(candidate)) %>%
  left_join(party_lookup %>% select(year, district, last_name, party), by = c("year", "district", "last_name")) %>% mutate(party = coalesce(party, "OTHER"))
all_c <- xw %>% distinct(county_name, county_fips) %>% mutate(county_code = substr(county_name, 1, 4))
pres_ref <- xw %>% filter(county_name %in% unlist(AMB)) %>% distinct(year, county_name, totalvotes) %>% group_by(county_name) %>% summarise(ref_votes = mean(totalvotes, na.rm = TRUE), .groups = "drop")
resolve <- function(code, obs) { cands <- AMB[[code]]; refs <- pres_ref$ref_votes[match(cands, pres_ref$county_name)]; cands[which.min(abs(log(obs) - log(refs)))] }
amb <- r00 %>% filter(county_code %in% AMBIGUOUS_CODES) %>% group_by(year, district, county_code) %>% summarise(obs = sum(votes), .groups = "drop") %>%
  rowwise() %>% mutate(resolved = resolve(county_code, obs)) %>% ungroup()
r00 <- r00 %>% left_join(amb %>% select(year, district, county_code, resolved), by = c("year", "district", "county_code")) %>% mutate(county_code = coalesce(resolved, county_code)) %>% select(-resolved)
ky_lookup <- bind_rows(all_c %>% filter(!county_code %in% AMBIGUOUS_CODES) %>% select(county_code, county_fips),
                       all_c %>% filter(county_name %in% unlist(AMB)) %>% transmute(county_code = county_name, county_fips),
                       all_c %>% filter(county_name %in% SPECIAL_CODES) %>% transmute(county_code = names(SPECIAL_CODES)[match(county_name, SPECIAL_CODES)], county_fips))
k00 <- r00 %>% inner_join(ky_lookup, by = "county_code") %>% transmute(year, county_fips, district, candidate, party_group = grp(party), party = NA_character_, votes)

## ================= 2010 (hand-transcribed tables, parsed out of 01i so there is a single copy) =================
src <- readLines(file.path(PROJECT_ROOT, "R", "data_creation", "01i_house_county_kentucky_2010.R"))
i0 <- grep("^d1 <- tribble", src); i1 <- grep("^ky_2010_raw <-", src) - 1
env <- new.env(); eval(parse(text = src[i0:i1]), envir = env)
names_2010 <- list(d1 = c(REP = "Whitfield", DEM = "Hatchett"), d2 = c(REP = "Guthrie", DEM = "Marksberry"), d3 = c(REP = "Lally", DEM = "Yarmuth"),
                   d4 = c(REP = "Davis", DEM = "Waltz"), d5 = c(REP = "Rogers", DEM = "Holbert"), d6 = c(REP = "Barr", DEM = "Chandler"))
k10 <- bind_rows(lapply(1:6, function(d) {
  x <- get(paste0("d", d), envir = env) %>% pivot_longer(-county_name, names_to = "col", values_to = "votes", values_drop_na = TRUE)
  nm <- names_2010[[paste0("d", d)]]
  x %>% transmute(county_name, district = d, votes, party_group = case_when(col == "REP" ~ "REP", col == "DEM" ~ "DEM", TRUE ~ "OTHER"),
                  candidate = ifelse(col %in% names(nm), nm[col], "Other candidates"), party = NA_character_)
})) %>% inner_join(KY_COUNTIES, by = "county_name") %>% filter(!is.na(votes)) %>% mutate(year = 2010) %>%
  select(year, county_fips, district, candidate, party, party_group, votes)

## ================= 2012-2016 =================
KY_COUNTY_SET <- KY_COUNTIES$county_name
parse_pdf <- function(path, year) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  dist_idx <- grep("^\\s*\\d+(st|nd|rd|th)\\s+Congressional District\\s*$", lines, ignore.case = TRUE); out <- list()
  for (di in seq_along(dist_idx)) {
    start <- dist_idx[di]; end <- if (di < length(dist_idx)) dist_idx[di + 1] - 1 else length(lines)
    district <- as.integer(gsub("\\D.*", "", trimws(lines[start]))); block <- lines[start:end]; hdr <- NULL
    for (li in seq_along(block)) {
      m <- regmatches(block[li], regexec("^\\s*([A-Za-z][A-Za-z .'-]*?)\\s+((?:[\\d,]+\\s*)+)$", block[li], perl = TRUE))[[1]]; if (length(m) != 3) next
      nm <- toupper(trimws(m[2]))
      if (nm == "TOTAL VOTES" && !is.null(hdr)) break
      if (nm == "TOTAL VOTES") next
      if (!nm %in% KY_COUNTY_SET) next
      if (is.null(hdr)) { pj <- li - 1; while (pj > 0 && trimws(block[pj]) == "") pj <- pj - 1; if (pj > 0) hdr <- strsplit(trimws(block[pj]), "\\s{2,}")[[1]] }
      if (is.null(hdr)) next
      votes <- suppressWarnings(as.numeric(gsub(",", "", strsplit(trimws(m[3]), "\\s+")[[1]]))); if (length(votes) != length(hdr) || any(is.na(votes))) next
      out[[length(out) + 1]] <- tibble(year = year, district = district, county_name = nm, header_name = hdr, last_name = last_name_of(hdr), votes = votes)
    }
  }
  bind_rows(out)
}
r10s <- bind_rows(lapply(c(2012, 2014, 2016), function(y) parse_pdf(file.path(RAWK, "kentucky_2010s", paste0(y, ".txt")), y))) %>%
  left_join(party_lookup %>% select(year, district, last_name, full_name, party), by = c("year", "district", "last_name")) %>% mutate(party = coalesce(party, "OTHER"))
k10s <- r10s %>% inner_join(KY_COUNTIES, by = "county_name") %>%
  transmute(year, county_fips, district, candidate = coalesce(full_name, stringr::str_to_title(header_name)), party_group = grp(party), party = NA_character_, votes)

long <- finalize_long(bind_rows(k90, k00, k10, k10s), "ky"); save_long(long, "he_ky")
nr <- bind_rows(name_report); message("KY long rows: ", nrow(long), " | 1990s districts with aligned names: ", sum(nr$names_ok), " of ", nrow(nr), " (rest use party label as candidate)")
print(as.data.frame(nr %>% group_by(year) %>% summarise(ok = sum(names_ok), n = n())))
for (f in c("ky", "ky_2000s", "ky_2010", "ky_2010s")) {
  yrs <- switch(f, ky = NULL, ky_2000s = c(2000, 2002, 2004, 2006, 2008), ky_2010 = 2010, ky_2010s = c(2012, 2014, 2016))
  check_long_vs_source(long, file.path(OUTPUT_DIR, paste0("elect_he_cty_", f, ".rds")), years = yrs)
}
