## Colorado U.S. House, county level, November general elections 1990-2000 (6 years x 6 districts =
## 36 contests, downloaded by 01ea_colorado_download.R from historicalelectiondata.coloradosos.gov).
## Layout: header row 1 = candidate/column names, row 2 = parties, then a "U.S. Congressperson
## District" total row and one "County" row per county in that district (no precinct summing
## needed -- same shape as Virginia's "Congressional District"/"Locality" rows, see 02r_house_va.R,
## which this script's structure follows closely).
##
## Pseudo-columns are identified BY NAME ("Registered Voters", "Blank Votes", "Total Votes Cast",
## "Total Ballots Cast"), not by a positional cutoff before "Total Votes Cast" the way Virginia's
## script does -- Colorado's "Registered Voters" column sits BEFORE any write-in aggregate column
## in 1998's contests (`Candidate1, Candidate2, Candidate3, Registered Voters, Write In, Blank
## Votes, Total Votes Cast, Total Ballots Cast`), so a positional "everything before Total Votes
## Cast is a candidate" rule would have wrongly treated "Registered Voters" as a candidate. "Write
## In" itself IS a real (small) aggregate candidate column when present (party label "Unaffiliated"
## in the source) and is kept, same treatment as Virginia's aggregated "Write-Ins" column.
##
## Two REAL party-mislabeling bugs found in the source and fixed here, both isolated to 1990 (the
## only year affected; every other year's header row was checked and is clean): contest 9240 (CD1)
## lists BOTH Schroeder and her opponent "Gloria Gonzales Roemer" as "Democratic"; contest 9243
## (CD6) lists BOTH Jarrett and "Dan Schaefer" as "Democratic". Roemer and Schaefer were both
## Republicans (confirmed: Schaefer was CO-6's Republican incumbent 1983-1999 per GovTrack; Roemer
## was the GOP's 1990 nominee against Schroeder per Colorado Politics' contemporaneous coverage) --
## corrected via an explicit override rather than trusting the source's party column for these two
## specific rows.
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "colorado_elections")
idx <- read.csv(file.path(DIR, "contests_index.csv"), stringsAsFactors = FALSE) %>% filter(keep) %>%
  mutate(district = sprintf("%02d", as.integer(sub("U.S. Congressperson District ", "", division))))
stopifnot(!anyDuplicated(idx[, c("year", "district")]), nrow(idx) == 36)

xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "COLORADO") %>% distinct(county_fips, county_name) %>% mutate(nm = toupper(trimws(county_name)))
fips_of <- function(z) xw$county_fips[match(toupper(trimws(z)), xw$nm)]

## (year, district, candidate) -> corrected party label; see header note.
PARTY_OVERRIDE <- tribble(
  ~year, ~district, ~candidate,               ~party,
  1990,  "01",      "Gloria Gonzales Roemer",  "Republican",
  1990,  "06",      "Dan Schaefer",            "Republican"
)

## 1998's contests (only) add a "Write In" aggregate column whose votes are consistently EXCLUDED
## from the source's own printed "Total Votes Cast" (confirmed: e.g. contest 8362/CD1, Adams county
## candidate cells sum to 9,510 but "Total Votes Cast" reads 9,492 -- exactly the county's 18 write-
## in votes short; every other 1998 discrepancy found below is explained the same way). "Write In"
## is still a real candidate column and is KEPT in the output (`cc`, OTHER party) -- only the
## verification below excludes it (`cc_check`), matching what the source's own total actually counts.
PSEUDO_COLS <- c("Registered Voters", "Blank Votes", "Total Votes Cast", "Total Ballots Cast")
res <- list(); tie <- list()
for (i in seq_len(nrow(idx))) {
  d <- read.csv(file.path(DIR, sprintf("contest_%d.csv", idx$contest_id[i])), header = FALSE, colClasses = "character", check.names = FALSE, fill = TRUE)
  nm <- unlist(d[1, -(1:2)]); pt <- unlist(d[2, -(1:2)])
  it <- which(nm == "Total Votes Cast"); stopifnot(length(it) == 1)
  cc <- which(!nm %in% PSEUDO_COLS)                                           # candidate + write-in columns, identified by name, kept in the output
  cc_check <- which(!nm %in% c(PSEUDO_COLS, "Write In"))                      # candidate-only columns, used for the arithmetic check (see note above)
  body <- d[-(1:2), ]; num <- function(m) suppressWarnings(as.numeric(gsub(",", "", m)))
  V <- sapply(seq_len(ncol(d) - 2), function(k) num(body[[k + 2]]))
  if (is.null(dim(V))) V <- matrix(V, nrow = nrow(body))
  is_cty <- body$V1 == "County"; is_dist <- body$V1 == "U.S. Congressperson District"
  cty <- V[is_cty, , drop = FALSE]; dist <- V[is_dist, ]
  ## The district row's own "Write In" cell is unreliable where present (confirmed: 1998 CD3's
  ## district row shows 5,600 for Write In -- an exact duplicate of the Libertarian candidate's own
  ## column, clearly a copy-paste error in the source -- while the 36 county rows sum to the
  ## correct, self-consistent 38). Tie-checked on cc_check + it only, same columns as the row check;
  ## the 36 county-level write-in values are still real and used in the output, just not verified
  ## against this one unreliable district cell.
  ok_rows <- all(rowSums(cty[, cc_check, drop = FALSE]) == cty[, it]); ok_tie <- all(colSums(cty[, c(cc_check, it)]) == dist[c(cc_check, it)])
  tie[[i]] <- data.frame(year = idx$year[i], district = idx$district[i], n_cty = nrow(cty), rows_ok = ok_rows, tie_ok = ok_tie)
  fp <- fips_of(body$V2[is_cty]); stopifnot(!anyNA(fp))
  keep_cty <- cty[, it] > 0; cty <- cty[keep_cty, , drop = FALSE]; fp <- fp[keep_cty]   # a few rows are all zero (a county whose CD-slice has no residents)
  party_i <- pt[cc]
  ov <- PARTY_OVERRIDE %>% filter(year == idx$year[i], district == idx$district[i])
  for (k in seq_along(cc)) { hit <- ov$candidate == nm[cc[k]]; if (any(hit)) party_i[k] <- ov$party[hit][1] }
  res[[i]] <- do.call(rbind, lapply(seq_along(cc), function(k) data.frame(year = idx$year[i], county_fips = fp, district = idx$district[i], candidate = nm[cc[k]], party = party_i[k], votes = unname(cty[, cc[k]]), stringsAsFactors = FALSE)))
}
tie <- bind_rows(tie); cat("contests:", nrow(tie), "| every county row adds up to Total Votes Cast:", sum(tie$rows_ok), "| county sums equal the printed district row:", sum(tie$tie_ok), "\n")
print(as.data.frame(tie %>% filter(!rows_ok | !tie_ok)))
stopifnot(all(tie$rows_ok), all(tie$tie_ok))

raw <- bind_rows(res) %>% mutate(party = ifelse(is.na(party) | party == "", "Write-In", party),
                                  party_group = case_when(startsWith(toupper(party), "DEM") ~ "DEM", startsWith(toupper(party), "REP") ~ "REP", TRUE ~ "OTHER"))
stopifnot(!anyDuplicated(raw[, c("year", "county_fips", "district", "candidate", "party")]))
cat("party labels:", paste(names(table(raw$party)), table(raw$party), collapse = "; "), "\n")

for (y in sort(unique(raw$year))) save_long(finalize_long(raw %>% filter(year == y) %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("co_", y)), paste0("he_co_", y))
saveRDS(raw, file.path(OUTPUT_DIR, "co_1990_2000_raw_candidates.rds"))

## ---- shares files for every year + acceptance test (each year folded into the panel by 01ec_colorado_apply.R) ----
for (y in seq(1990, 2000, 2)) {
  long <- readRDS(file.path(LONG_DIR, sprintf("he_co_%d.rds", y)))
  n_counties <- n_distinct(long$county_fips)
  message(y, ": ", n_counties, " counties")
  shares <- derive_shares(long) %>% transmute(state = "COLORADO", year, cty_fips, sample, demovote, repuvote, totalvote)
  stopifnot(nrow(shares) == 63)   # pre-Broomfield county count
  saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_co_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_co_%d.rds", y))); stopifnot(all(r$pass))
}

## ---- sanity range ----
all_shares <- purrr::map_dfr(seq(1990, 2000, 2), ~ readRDS(file.path(OUTPUT_DIR, sprintf("elect_he_cty_co_%d.rds", .x))))
sanity <- all_shares$demovote + all_shares$repuvote
message("sanity range demovote+repuvote: [", round(min(sanity), 3), ", ", round(max(sanity), 3), "]")
stopifnot(all(sanity >= 0 & sanity <= 1.001))
