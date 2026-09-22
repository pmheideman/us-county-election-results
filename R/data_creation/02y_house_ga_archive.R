## Georgia U.S. House by county, general elections 1990-1998, from the Georgia Secretary of State's archived results pages (Wayback Machine copies of sos.georgia.gov, July 2008;
## fetched by 01da_georgia_sos_archive_download.py into R/data/raw_house_county_open_states/georgia_sos_archive/). One page per congressional district (10 districts in 1990, 11 in 1992-1998) listing
## the district's counties with each candidate's votes (a county in 2+ districts appears in each with its part) and the district's printed candidate totals and percentages.
## 1990-1994 layout: header lines (names, party tags "(Dem)", totals, percents) then "Vote by County" and county name followed by one number per candidate.
## 1996-1998 layout: candidate names, party "(D)", totals, percents, then rows "COUNTY PR TP votes...".
## Checks: county sums == the printed candidate totals; printed percentages; county count; House total vs the presidential/Senate/governor total in the panel or in the same archive.
## Outputs: R/output/long/he_ga_archive_<year>.rds, R/output/elect_he_cty_ga_archive_<year>.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "georgia_sos_archive")
txt <- function(f) { h <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"); h <- gsub("(?is)<script.*?</script>|<style.*?</style>", "", h, perl = TRUE); t <- gsub("<[^>]+>", "\n", h)
  t <- gsub("&amp;", "&", gsub("&nbsp;| ", " ", t)); l <- trimws(unlist(strsplit(t, "\n"))); l[nzchar(l)] }
num <- function(z) as.numeric(gsub("[,%]", "", z))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "GEORGIA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 159)
PT <- c(Dem = "DEM", D = "DEM", Rep = "REP", R = "REP")
parse_old <- function(l, y, d) {                                        # 1990-1994
  i0 <- which(grepl("^Vote by County$", l, ignore.case = TRUE))[1]; hdr <- l[seq_len(i0 - 1)]
  pt <- which(grepl("^\\([A-Za-z. ]+\\)$", hdr)); n <- length(pt); stopifnot(n >= 1)
  names_c <- hdr[(pt[1] - n):(pt[1] - 1)]; parties <- gsub("[()]", "", hdr[pt]); after <- hdr[(pt[n] + 1):length(hdr)]
  tot <- num(after[seq_len(n)]); pct <- num(after[n + seq_len(n)])
  body <- l[(i0 + 1):length(l)]; rows <- list(); j <- 1
  while (j <= length(body)) { if (!grepl("^[0-9][0-9,]*\\s*$", body[j]) && j + n <= length(body) && all(grepl("^[0-9][0-9,]*\\s*$", body[(j + 1):(j + n)]))) {
      rows[[length(rows) + 1]] <- data.frame(county = body[j], candidate = names_c, party = parties, votes = num(body[(j + 1):(j + n)])); j <- j + n + 1 } else j <- j + 1 }
  list(rows = bind_rows(rows), tot = data.frame(candidate = names_c, printed = tot, pct = pct)) }
parse_new <- function(l, y, d) {                                        # 1996-1998: header = surnames line, party line "(D) (R)", totals line, percents line
  i0 <- which(grepl("^County\\s+PR\\s+TP", l, ignore.case = TRUE))[1]; stopifnot(!is.na(i0))
  ip <- which(grepl("^(\\([A-Za-z]+\\)\\s*)+$", l[seq_len(i0 - 1)])); ip <- ip[length(ip)]; stopifnot(length(ip) == 1)
  parties <- regmatches(l[ip], gregexpr("\\(([A-Za-z]+)\\)", l[ip]))[[1]]; parties <- gsub("[()]", "", parties); n <- length(parties)
  nm <- strsplit(l[ip - 1], "\\s{2,}")[[1]]; if (length(nm) != n) nm <- strsplit(l[ip - 1], "\\s+")[[1]]; if (length(nm) != n) stop("name count ", length(nm), " != party count ", n, " in: ", l[ip - 1])
  tot <- num(strsplit(l[ip + 1], "\\s+")[[1]]); pct <- num(strsplit(l[ip + 2], "\\s+")[[1]]); stopifnot(length(tot) == n, length(pct) == n)
  body <- l[(i0 + 1):length(l)]; rows <- list()
  for (b in body) { m <- regmatches(b, regexec("^([A-Z][A-Z .'-]+?)\\s+([0-9]+)\\s+([0-9]+)\\s+((?:[0-9][0-9,]*\\s*)+)$", b, perl = TRUE))[[1]]
    if (length(m)) { v <- num(strsplit(trimws(m[5]), "\\s+")[[1]]); if (length(v) == n) rows[[length(rows) + 1]] <- data.frame(county = m[2], candidate = nm, party = parties, votes = v) } }
  list(rows = bind_rows(rows), tot = data.frame(candidate = nm, printed = tot, pct = pct)) }
res <- list(); tie <- list()
for (y in c(1990, 1992, 1994, 1996, 1998)) {
  files <- list.files(D, sprintf("^%d_(district|page)[0-9]+\\.html$", y), full.names = TRUE)
  for (f in files) { l <- txt(f); dist <- if (y <= 1994) as.integer(sub(".*district([0-9]+)\\.html", "\\1", f)) else as.integer(sub("^.*UNITED STATES REPRESENTATIVE - ([0-9]+).*$", "\\1", toupper(paste(l[grepl("UNITED STATES REPRESENTATIVE - ", toupper(l))][1]))))
    p <- if (y <= 1994) parse_old(l, y, dist) else parse_new(l, y, dist); p$rows$year <- y; p$rows$district <- sprintf("%02d", dist)
    ck <- p$rows %>% group_by(candidate) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(p$tot, by = "candidate")
    tie[[length(tie) + 1]] <- data.frame(year = y, district = dist, cand = nrow(ck), n_cty = max(ck$n_cty), ok = all(ck$sum == ck$printed, na.rm = TRUE) && !anyNA(ck$printed), off = sum(ck$sum - ck$printed, na.rm = TRUE))
    res[[length(res) + 1]] <- p$rows } }
tie <- bind_rows(tie); print(as.data.frame(tie %>% filter(!ok))); cat("district pages parsed:", nrow(tie), "| county sums equal the printed candidate totals:", sum(tie$ok), "\n")
stopifnot(all(tie$ok))
raw <- bind_rows(res) %>% mutate(county = ifelse(county == "Hill", "Ben Hill", ifelse(county == "Davis", "Jeff Davis", county))) %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); if (anyNA(raw$county_fips)) { print(unique(raw$county[is.na(raw$county_fips)])); stop("unmatched county names") }
raw <- raw %>% transmute(year, county_fips, district, candidate = tools::toTitleCase(tolower(candidate)), party_group = case_when(party %in% c("Dem", "D") ~ "DEM", party %in% c("Rep", "R") ~ "REP", TRUE ~ "OTHER"),
                         party = case_when(party %in% c("Dem", "D") ~ "Democratic", party %in% c("Rep", "R") ~ "Republican", party %in% c("Lib", "L") ~ "Libertarian", TRUE ~ party), votes)
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds"))
for (y in sort(unique(raw$year))) {
  long <- finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ga_archive_", y)); save_long(long, paste0("he_ga_archive_", y))
  shares <- derive_shares(long) %>% transmute(state = "GEORGIA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ga_archive_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ga_archive_%d.rds", y))); stopifnot(all(r$pass))
  ref <- panel %>% filter(sample == ifelse(y %% 4 == 0, "PE", "SE"), year == y) %>% select(cty_fips, ref = totalvote); if (!nrow(ref)) ref <- panel %>% filter(sample == "PE", year == ifelse(y %% 4 == 0, y, y - 2)) %>% select(cty_fips, ref = totalvote)
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(sprintf("%d: counties %d | districts %d | split counties %d | median dem %.3f rep %.3f | House/ref total min %.2f med %.2f max %.2f | panel GA rows %d\n", y, n_distinct(long$county_fips), n_distinct(long$district),
      sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), median(shares$demovote), median(shares$repuvote), suppressWarnings(min(rr$ratio, na.rm = TRUE)), suppressWarnings(median(rr$ratio, na.rm = TRUE)), suppressWarnings(max(rr$ratio, na.rm = TRUE)), sum(panel$sample == "HE" & panel$year == y & panel$cty_fips %/% 1000 == 13)))
}
