## Texas U.S. House, county level, general elections 1992-2018, from the official Secretary of State site (pages cached by 01cj_texas_sos_download.R).
## Page layout: header row 1 = candidate FIRST names, header row 2 = LAST names, header row 3 = party codes; then "ALL COUNTIES" (statewide total for the district)
## and one row per county in the district. Counties in 2+ districts appear once per district page (kept by district). Unopposed seats are not on the site
## (no ballot line): 1996 lists 17 races in the general + 13 in the "November 1996 Special Election" (the real November 5 election in the districts redrawn after
## Bush v. Vera; elections 57) + December runoff for D8, D9, D25 (election 58, stage "runoff"; decisive-round rule of docs/DECISIONS.md).
## 2006 is the same: districts 15, 21, 23, 25, 28 (redrawn after LULAC v. Perry) voted in the "2006 Special November Elections" (election 128, open election on Nov 7) and D23 was decided in the December runoff (election 129).
## Checks: in every county row the candidate cells add up to the printed Votes cell; county sums == printed ALL COUNTIES row for every candidate (single-county districts have no such row); rows with candidate cells blank = 0.
## Outputs: R/output/long/he_tx_sos_<year>.rds (all 1992-2018, for comparison), R/output/elect_he_cty_tx_sos_<year>.rds for the years folded into the panel
## (1992-1998, 2006, 2014-2018) and R/output/tx_sos_comparison.csv (SOS build vs the panel's current Texas rows, 2000-2018).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "texas_sos")
idx <- read.csv(file.path(DIR, "races_index.csv"), stringsAsFactors = FALSE)
## 1996 November special (57) and runoff (58): fetch race pages (elections 57/58 list their own race ids)
extra <- list()
EXTRA_YEAR <- c(`57` = 1996L, `58` = 1996L, `128` = 2006L, `129` = 2006L)
for (e in names(EXTRA_YEAR)) { e <- as.integer(e)
  h <- readLines(file.path(DIR, sprintf("elchist%d_raceselect.htm", e)), warn = FALSE, encoding = "latin1")
  o <- regmatches(h, regexec("<OPTION value=\"([0-9]+)\"[^>]*>\\s*(U\\. ?S\\. Rep[^<]*?)\\s*$", h, ignore.case = TRUE)); o <- o[lengths(o) > 0]
  extra[[as.character(e)]] <- data.frame(year = EXTRA_YEAR[[as.character(e)]], election = e, race = vapply(o, `[`, "", 2), label = trimws(vapply(o, `[`, "", 3)))
}
extra <- do.call(rbind, extra); extra <- extra[!grepl("Unexpired", extra$label), ]     # 2006 D22 "Unexpired Term" is a special election (the regular D22 race is in the general)
for (i in seq_len(nrow(extra))) { f <- file.path(DIR, sprintf("elchist%d_race%s.htm", extra$election[i], extra$race[i]))
  if (!file.exists(f) || file.size(f) < 500) { Sys.sleep(0.4); system2("curl", c("-sL", "-m", "40", "-A", shQuote("Mozilla/5.0 (research; county election data project)"), "-o", shQuote(f), shQuote(sprintf("https://elections.sos.state.tx.us/elchist%d_race%s.htm", extra$election[i], extra$race[i])))) } }
idx <- rbind(idx, extra)
## 1996: districts 8, 9, 25 were decided in the December runoff (election 58): the November round (57) is dropped for them
idx$dist <- sprintf("%02d", as.integer(sub(".*District ([0-9]+).*", "\\1", idx$label)))
idx <- idx %>% filter(!(election == 57 & dist %in% c("08", "09", "25")), !(election == 128 & dist == "23"))
idx$stage <- ifelse(idx$election %in% c(58, 129), "runoff", "general")
stopifnot(!anyDuplicated(idx[, c("year", "dist")]))

cells <- function(row, tag) { m <- gregexpr(sprintf("<%s[^>]*>(.*?)</%s>", tag, tag), row, ignore.case = TRUE, perl = TRUE); z <- regmatches(row, m)[[1]]
  trimws(gsub("&nbsp;", " ", gsub("\\s+", " ", gsub("<[^>]*>", "", z)))) }
num <- function(z) { z <- gsub("[, ]", "", z); ifelse(z == "" | z == "N/A", 0, suppressWarnings(as.numeric(z))) }
parse_page <- function(f) {
  h <- paste(readLines(f, warn = FALSE, encoding = "latin1"), collapse = "\n"); rows <- strsplit(h, "(?i)<TR", perl = TRUE)[[1]][-1]
  hdr <- rows[grepl("<TH", rows, ignore.case = TRUE)]; stopifnot(length(hdr) == 3)
  r1 <- cells(hdr[1], "TH"); r2 <- cells(hdr[2], "TH"); r3 <- cells(hdr[3], "TH")
  stopifnot(r3[1] == "County"); iv <- which(r3 == "Votes"); pc <- r3[2:(iv - 1)]                    # party code columns
  nm <- trimws(paste(r1[2:(iv - 1)], r2[2:(iv - 1)])); nm <- gsub("\\s+", " ", nm)
  d <- rows[grepl("<TD", rows, ignore.case = TRUE)]
  tb <- lapply(d, function(r) { z <- cells(r, "TD"); c(z[1], z[2:(iv - 1)], z[iv]) })
  out <- do.call(rbind, lapply(tb, function(z) data.frame(county = z[1], cand = nm, party_code = pc, votes = num(z[2:(length(z) - 1)]), row_total = num(z[length(z)]), stringsAsFactors = FALSE)))
  list(all = out %>% filter(county == "ALL COUNTIES"), cty = out %>% filter(county != "ALL COUNTIES"))
}
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "TEXAS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 254)
PARTY <- c(DEM = "Democratic", REP = "Republican", LIB = "Libertarian", GRN = "Green", NLP = "Natural Law", `W-I` = "Write-In", IND = "Independent", `NON` = "Nonpartisan")
res <- list(); tie <- list()
for (i in seq_len(nrow(idx))) {
  f <- file.path(DIR, sprintf("elchist%d_race%s.htm", idx$election[i], idx$race[i])); p <- parse_page(f)
  pp <- p$cty %>% mutate(year = idx$year[i], district = idx$dist[i], stage = idx$stage[i]); res[[i]] <- pp
  s <- pp %>% group_by(cand, party_code) %>% summarise(s = sum(votes), .groups = "drop") %>% left_join(p$all %>% select(cand, party_code, tot = votes), by = c("cand", "party_code"))
  rt <- pp %>% group_by(county) %>% summarise(v = sum(votes), t = first(row_total), .groups = "drop")             # each county row: candidate cells add up to the printed Votes cell
  tie[[i]] <- data.frame(year = idx$year[i], district = idx$dist[i], n_cand = nrow(s), has_all = nrow(p$all) > 0, rows_ok = all(rt$v == rt$t), all_ok = all(is.na(s$tot) | s$s == s$tot))
}
raw <- bind_rows(res); tie <- bind_rows(tie)
cat("pages parsed:", nrow(tie), "| every county row adds up to its printed Votes cell:", sum(tie$rows_ok), "| county sums equal the ALL COUNTIES row (where printed, ", sum(tie$has_all), " pages):", sum(tie$all_ok[tie$has_all]), "\n"); print(as.data.frame(tie %>% filter(!rows_ok | !all_ok)))
stopifnot(all(tie$rows_ok), all(tie$all_ok))
raw <- raw %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
raw <- raw %>% transmute(year, county_fips, district, stage, candidate = cand, party = ifelse(party_code %in% names(PARTY), PARTY[party_code], party_code),
                         party_group = case_when(party_code == "DEM" ~ "DEM", party_code == "REP" ~ "REP", TRUE ~ "OTHER"), votes)
saveRDS(raw, file.path(OUTPUT_DIR, "tx_sos_raw_candidates.rds"))
cat("\nparty codes:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")
for (y in sort(unique(raw$year))) {
  r <- raw %>% filter(year == y)
  long <- finalize_long(r %>% select(-stage), paste0("tx_sos_", y))
  long$stage <- r$stage[match(paste(long$county_fips, long$district), paste(r$county_fips, r$district))]
  save_long(long, paste0("he_tx_sos_", y))
}

## ---- shares files for the years folded into the panel (01ck_texas_apply.R) + acceptance test ------------------------------------------------------
## 1992-1998: no Texas House county data existed before. 2014, 2016, 2018: the SOS official numbers differ from the panel for 2 / 45 / 43 counties (2014: El Paso and Ellis
## were swapped; 2016 and 2018: MEDSL precinct totals, e.g. Harris 2016 short by 22,666 votes); 2000-2012 are identical to the panel already (2006's 49 missing counties
## are districts with no contested race, so no ballot line) and are not re-folded.
for (y in sort(unique(raw$year))) {                   # every year 1992-2018 (2000-2012 are identical to the panel already; their SOS shares files make the SOS the recorded source)
  long <- readRDS(file.path(LONG_DIR, sprintf("he_tx_sos_%d.rds", y)))
  shares <- derive_shares(long) %>% transmute(state = "TEXAS", year, cty_fips, sample, demovote, repuvote, totalvote)
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_tx_sos_%d.rds", y)))
  print(check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_tx_sos_%d.rds", y))) %>% select(source, keys_source, matched, mismatched, pass))
}
