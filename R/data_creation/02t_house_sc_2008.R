## South Carolina 2008 U.S. House by county, from the State Election Commission's Election Night Reporting archive (official post-election snapshot, timestamp 06/01/2009):
##   https://www.enr-scvotes.org/SC/8562/15723/en/select-county.html ; the snapshot file https://www.enr-scvotes.org/SC/8562/15723/reports/detailxml.zip is saved as
##   R/data/raw_house_county_open_states/south_carolina_enr_8562_15723_detail.xml (President, U.S. Senate, U.S. House D1-6, state races; one <Precinct> element per COUNTY).
## Contest "U.S. House of Representatives District n": one <Choice> per candidate (party code REP/DEM/GRN/CON/NON) with votes per county; counties in 2+ districts appear once per district.
## Checks: county votes per candidate add up to the printed totalVotes; the county count per district; county House total vs the county's presidential total (2008) and Senate total in the same file.
## Outputs: R/output/long/he_sc_2008.rds, R/output/elect_he_cty_sc_2008.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr); library(xml2)
x <- read_xml(file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "south_carolina_enr_8562_15723_detail.xml"))
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  filter(state == "SOUTH CAROLINA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 46)
PARTY <- c(REP = "Republican", DEM = "Democratic", GRN = "Green", CON = "Constitution", LIB = "Libertarian", IND = "Independent", UNI = "United Citizens", NON = "Write-In")
contest_votes <- function(node) { ch <- xml_find_all(node, "./Choice")
  purrr::map_dfr(ch, function(c) { p <- xml_find_all(c, ".//Precinct")
    tibble(candidate = xml_attr(c, "text"), pcode = xml_attr(c, "party"), total = as.numeric(xml_attr(c, "totalVotes")), county = xml_attr(p, "name"), votes = as.numeric(xml_attr(p, "votes"))) }) %>%
    group_by(candidate, pcode, total, county) %>% summarise(votes = sum(votes), .groups = "drop") }
hn <- xml_find_all(x, "//Contest[starts-with(@text, 'U.S. House of Representatives')]"); stopifnot(length(hn) == 6)
res <- purrr::map_dfr(hn, function(n) contest_votes(n) %>% mutate(district = sprintf("%02d", as.integer(sub(".*District\\s*([0-9]+).*", "\\1", xml_attr(n, "text"))))))
chk <- res %>% group_by(district, candidate) %>% summarise(sum = sum(votes), printed = first(total), .groups = "drop"); print(as.data.frame(chk))
stopifnot(all(chk$sum == chk$printed))
cat("counties per district:", paste(names(table(res$district[!duplicated(res[, c("district", "county")])])), table(res$district[!duplicated(res[, c("district", "county")])]), sep = "=", collapse = ", "), "\n")
res <- res %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(res$county_fips))
raw <- res %>% transmute(year = 2008L, county_fips, district, candidate = tools::toTitleCase(tolower(candidate)), party = PARTY[pcode], party_group = case_when(pcode == "DEM" ~ "DEM", pcode == "REP" ~ "REP", TRUE ~ "OTHER"), votes)
## the ENR file prints names without punctuation ("Henry E Brown Jr", "James E Jim Clyburn"): restored by hand from the official names
NAMES <- c("Henry E Brown Jr" = "Henry E. Brown Jr.", "J Gresham Barrett" = "J. Gresham Barrett", "Albert F Spencer" = "Albert F. Spencer", "C Faye Walters" = "C. Faye Walters",
           "James E Jim Clyburn" = "James E. \"Jim\" Clyburn")

long <- finalize_long(raw, "sc_2008")     # (finalize_long strips punctuation from names, so the hand-restored names are applied after it)
hit <- long$candidate %in% names(NAMES); long$candidate[hit] <- NAMES[long$candidate[hit]]; long$candidate[toupper(long$candidate) == "WRITE-IN"] <- "Write-In"
save_long(long, "he_sc_2008")
shares <- derive_shares(long) %>% transmute(state = "SOUTH CAROLINA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_sc_2008.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_sc_2008.rds")) %>% select(source, keys_source, matched, mismatched, pass))
cat("counties:", n_distinct(long$county_fips), " split counties:", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "\n")
## sanity: House total vs presidential total (2008) and Senate total in the same file
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == 2008) %>% select(cty_fips, pe = totalvote)
r <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe); cat("House 2008 / presidential 2008 total: min", round(min(r$ratio, na.rm = TRUE), 3), "median", round(median(r$ratio, na.rm = TRUE), 3), "max", round(max(r$ratio, na.rm = TRUE), 3), "\n")
## vs the partial rows already in the panel
old <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", year == 2008, cty_fips %/% 1000 == 45) %>% select(cty_fips, o_dem = demovote, o_rep = repuvote, o_tot = totalvote)
d <- shares %>% inner_join(old, by = "cty_fips") %>% mutate(same = abs(demovote - o_dem) < 1e-9 & abs(repuvote - o_rep) < 1e-9 & abs(totalvote - o_tot) < 0.5)
cat("panel's existing 2008 SC rows:", nrow(old), "; identical to this build:", sum(d$same), "; differing:", sum(!d$same), "\n"); print(as.data.frame(d %>% filter(!same) %>% select(cty_fips, totalvote, o_tot, demovote, o_dem)))
