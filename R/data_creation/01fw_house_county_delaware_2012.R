## Delaware U.S. House (at-large) 2012, county level, from the Department of Elections' results archive (https://elections.delaware.gov/elections/resultsarchive/elect12/elect12_general/data/stwoff_kns.txt,
## saved as R/data/raw_house_county_open_states/delaware/stwoff_kns_2012.txt): "Representative in Congress" by New Castle / Kent / Sussex with an Office Total row. Check: county columns add up to the printed total
## of every candidate. Party: Carney D, Kovach R (the Democratic and Republican nominees), the two others OTHER. Outputs: he_dede_2012 long and elect_he_cty_dede_2012 (folded in by 01fx_delaware_apply.R).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
L <- readLines(file.path(PROJECT_ROOT, "R/data/raw_house_county_open_states/delaware/stwoff_kns_2012.txt"), warn = FALSE)
i <- grep("^REPRESENTATIVE IN CONGRESS", L); j <- which(grepl("Office Total", L) & seq_along(L) > i)[1]
rows <- L[(i + 1):(j - 1)]; num <- function(x) as.numeric(gsub("[, ]", "", x))
parts <- lapply(rows, function(r) trimws(strsplit(r, ";")[[1]])); stopifnot(all(lengths(parts) >= 5))
tot <- num(trimws(strsplit(L[j], ";")[[1]])[2:5])
m <- do.call(rbind, lapply(parts, function(p) c(num(p[2:5])))); nm <- vapply(parts, `[`, "", 1)
stopifnot(all(colSums(m) == tot), all(rowSums(m[, 1:3]) == m[, 4]))
counties <- c("NEW CASTLE", "KENT", "SUSSEX")
de <- read.delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab")) %>% filter(state == "DELAWARE", !is.na(county_fips)) %>% transmute(county_fips, county = toupper(trimws(county_name))) %>% distinct(county, .keep_all = TRUE); stopifnot(all(counties %in% de$county))
nice <- c("CARNEY JR. JOHN C." = "John C. Carney Jr.", "KOVACH THOMAS H." = "Thomas H. Kovach", "AUGUST BERNARD" = "Bernard August", "GESTY SCOTT" = "Scott Gesty")
raw <- do.call(rbind, lapply(seq_along(nm), function(k) data.frame(year = 2012L, county = counties, candidate = unname(nice[trimws(nm[k])]), votes = m[k, 1:3], stringsAsFactors = FALSE))) %>% inner_join(de, by = "county") %>%
  transmute(year, county_fips, district = "00", candidate, party = ifelse(candidate == "John C. Carney Jr.", "Democratic", ifelse(candidate == "Thomas H. Kovach", "Republican", "Other")),
            party_group = ifelse(candidate == "John C. Carney Jr.", "DEM", ifelse(candidate == "Thomas H. Kovach", "REP", "OTHER")), votes)
long <- finalize_long(raw, "dede_2012"); save_long(long, "he_dede_2012")
shares <- derive_shares(long) %>% transmute(state = "DELAWARE", year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(nrow(shares) == 3)
saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_dede_2012.rds")); r <- check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_dede_2012.rds")); stopifnot(all(r$pass))
message("2012: ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
