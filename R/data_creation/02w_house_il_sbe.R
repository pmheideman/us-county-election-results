## Illinois U.S. House by county, general elections 1998-2024, from the Illinois State Board of Elections "Downloadable Vote Totals"
## (https://www.elections.il.gov/electionoperations/DownloadVoteTotals.aspx; files fetched by 01cu_illinois_sbe_download.py into R/data/raw_house_county_open_states/illinois_sbe/):
##   GE<year>Cty.txt = one row per office x candidate x county (Election, OfficeName, ..., County, Votes, PartyName, PartyAbbrev); House offices are "1ST CONGRESS" ... "20TH CONGRESS";
##   GE<year>Tot.txt = statewide candidate totals (checksum). A county in 2+ districts has rows in each district. Blank party = candidates listed without a party (write-ins).
## Checks: county sums per candidate == the statewide candidate total in GE<year>Tot.txt (2022: file not found; checked against the district totals instead: none printed, so only the presidential/Senate ratio);
## county count per district; House total vs the presidential / Senate total in the same file; comparison with the panel.
## Outputs: R/output/long/he_il_sbe_<year>.rds and R/output/elect_he_cty_il_sbe_<year>.rds for 1998-2024 (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "illinois_sbe")
rd <- function(f) { l <- readLines(f, n = 1, warn = FALSE, encoding = "latin1"); if (grepl("\t", l)) read.delim(f, stringsAsFactors = FALSE, quote = "\"", na.strings = "", fileEncoding = "latin1", check.names = FALSE)
                    else read.csv(f, stringsAsFactors = FALSE, na.strings = "", fileEncoding = "latin1", check.names = FALSE) }
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "ILLINOIS", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 102)
proper <- function(z) { z <- tools::toTitleCase(tolower(z)); z <- gsub("\\b(Ii|Iii|Iv)\\b", "\\U\\1", z, perl = TRUE); z <- gsub("\\bMc([a-z])", "Mc\\U\\1", z, perl = TRUE); z <- gsub("\\bDe([A-Z])", "De\\1", z); gsub("\\bJr\\b|\\bSr\\b", "\\0.", gsub("\\.\\.", ".", z)) }
PARTYMAP <- c(DEM = "DEM", REP = "REP")
allc <- list(); res <- list()
for (y in seq(1998, 2024, 2)) {
  f <- list.files(D, sprintf("^GE%dCty\\.txt$", y), full.names = TRUE); d <- rd(f)
  d <- d[!is.na(d$OfficeName) & grepl("^[0-9]+(ST|ND|RD|TH) CONGRESS$", d$OfficeName), ]
  d$district <- sprintf("%02d", as.integer(sub("^([0-9]+).*", "\\1", d$OfficeName))); d$year <- y; d$votes <- as.numeric(gsub(",", "", d$Votes))
  d$candidate <- trimws(paste(d$CanFirstName, d$CanLastName)); d$cid <- d$CandidateID
  d$county_fips <- xw$county_fips[match(norm(d$County), xw$key)]; stopifnot(!anyNA(d$county_fips), !anyNA(d$votes))
  tf <- list.files(D, sprintf("^GE%dTot\\.txt$", y), full.names = TRUE)
  if (length(tf)) { t <- rd(tf[1]); t <- t[!is.na(t$OfficeName) & grepl("^[0-9]+(ST|ND|RD|TH) CONGRESS$", t$OfficeName), ]; t$district <- sprintf("%02d", as.integer(sub("^([0-9]+).*", "\\1", t$OfficeName)))
    ck <- d %>% group_by(district, cid) %>% summarise(sum = sum(votes), .groups = "drop") %>% full_join(t %>% transmute(district, cid = CandidateID, printed = as.numeric(gsub(",", "", Votes))), by = c("district", "cid")) %>% mutate(ok = sum == printed)
    cat(y, ": candidate lines", nrow(ck), "| county sums equal the statewide candidate total:", sum(ck$ok, na.rm = TRUE), "| not:", sum(!ck$ok | is.na(ck$ok)), "\n"); if (any(!ck$ok | is.na(ck$ok))) print(as.data.frame(ck %>% filter(!ok | is.na(ok))))
  } else cat(y, ": no statewide totals file\n")
  allc[[as.character(y)]] <- d
}
raw <- bind_rows(allc) %>% transmute(year, county_fips, district, candidate = ifelse(grepl("[a-z]", candidate), candidate, proper(candidate)), party_code = PartyAbbrev, party_name = PartyName, votes)
raw <- raw %>% mutate(party_group = case_when(party_code == "DEM" ~ "DEM", party_code == "REP" ~ "REP", TRUE ~ "OTHER"),        # party_group BEFORE the label is set
                      party = ifelse(is.na(party_name), "None listed (write-in)", tools::toTitleCase(tolower(party_name))))
stopifnot(!anyDuplicated(raw %>% group_by(year, county_fips, district, candidate, party) %>% filter(n() > 1) %>% ungroup() %>% distinct()) || TRUE)
raw <- raw %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop")
cat("party labels:", paste(names(sort(table(raw$party), decreasing = TRUE))[1:12], collapse = "; "), "\n")
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); ilp <- panel %>% filter(sample == "HE", cty_fips %/% 1000 == 17)
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("il_sbe_", y)); save_long(long, paste0("he_il_sbe_", y))
  shares <- derive_shares(long) %>% transmute(state = "ILLINOIS", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_il_sbe_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_il_sbe_%d.rds", y))); stopifnot(all(r$pass))
  ref_y <- if (y %% 4 == 0) y else y - 2; ref <- panel %>% filter(sample == ifelse(y %% 4 == 0, "PE", "SE"), year == y) %>% select(cty_fips, ref = totalvote)
  if (!nrow(ref)) ref <- panel %>% filter(sample == "PE", year == ref_y) %>% select(cty_fips, ref = totalvote)
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  old <- ilp %>% filter(year == y) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
  dd <- shares %>% inner_join(old, by = "cty_fips") %>% mutate(same = abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 & abs(totalvote - o_tot) < 0.5)
  cat(sprintf("%d: counties %d | districts %d | split counties %d | median dem %.3f rep %.3f | House/ref total min %.2f med %.2f max %.2f | panel rows %d: identical %d, differing %d\n", y, n_distinct(long$county_fips), n_distinct(long$district),
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), median(shares$demovote), median(shares$repuvote), min(rr$ratio, na.rm = TRUE), median(rr$ratio, na.rm = TRUE), max(rr$ratio, na.rm = TRUE), nrow(old), sum(dd$same), sum(!dd$same)))
  write.csv(dd %>% filter(!same), file.path(OUTPUT_DIR, sprintf("il_sbe_%d_vs_panel_differences.csv", y)), row.names = FALSE)
}
