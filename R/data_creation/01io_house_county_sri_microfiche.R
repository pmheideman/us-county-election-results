## U.S. House county results transcribed from official state election books on the Internet Archive's Statistical Reference Index (SRI) microfiche, 2026-09-24:
## every R/data/county_house_files/sri/<state>_house_county_<year>.csv written by a 01if-01in *_transcribe.py script (each stops unless the printed totals and the
## FEC / Clerk of the House district results tie). Builds one source per state and year, label sri<po>_<year> (e.g. srioh_1990).
## Split counties are summed over districts. Party: D -> DEM, R -> REP, else OTHER. Folded in by 01ip_sri_microfiche_apply.R.
## Usage: Rscript R/data_creation/01io_house_county_sri_microfiche.R [state_po ...]   (no argument = every CSV present)
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
args <- toupper(commandArgs(trailingOnly = TRUE))
cp <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(!is.na(county_fips)) %>%
  transmute(state, state_po, county_fips, ckey = gsub("[^A-Z]", "", toupper(county_name))) %>% distinct(state, ckey, .keep_all = TRUE)
files <- list.files(file.path(PROJECT_ROOT, "R/data/county_house_files/sri"), pattern = "^[a-z_]+_house_county_[0-9]{4}\\.csv$", full.names = TRUE)
for (f in files) {
  st <- toupper(gsub("_", " ", sub("_house_county_[0-9]{4}\\.csv$", "", basename(f)))); yr <- as.integer(sub(".*_([0-9]{4})\\.csv$", "\\1", basename(f)))
  xw <- cp %>% filter(state == st, !is.na(state_po), county_fips < 60000); sf <- as.integer(names(which.max(table(xw$county_fips %/% 1000))))
  xw <- xw %>% filter(county_fips %/% 1000 == sf); po <- names(which.max(table(xw$state_po)))
  if (length(args) && !po %in% args) next
  n_cty <- n_distinct(xw$county_fips)
  d <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>% mutate(year = as.integer(year), votes = as.numeric(votes), district = sprintf("%02d", as.integer(district)), ckey = gsub("[^A-Z]", "", toupper(county)))
  stopifnot(all(d$year == yr), all(d$ckey %in% xw$ckey), all(d$party_code %in% c("D", "R", "I")), !anyNA(d$votes))
  if (n_distinct(d$ckey) != n_cty) message("NOTE ", st, " ", yr, ": ", n_distinct(d$ckey), " of ", n_cty, " counties")
  raw <- d %>% inner_join(xw %>% select(ckey, county_fips), by = "ckey") %>% transmute(year, county_fips, district, candidate, party = case_when(party_code == "D" ~ "Democratic", party_code == "R" ~ "Republican", TRUE ~ "Independent"),
    party_group = case_when(party_code == "D" ~ "DEM", party_code == "R" ~ "REP", TRUE ~ "OTHER"), votes) %>% group_by(year, county_fips, district, candidate, party, party_group) %>% summarise(votes = sum(votes), .groups = "drop") %>% filter(votes > 0)
  lab <- sprintf("sri%s_%d", tolower(po), yr)
  long <- finalize_long(raw, lab); save_long(long, paste0("he_", lab))
  shares <- derive_shares(long) %>% transmute(state = st, year, cty_fips, sample, demovote, repuvote, totalvote); stopifnot(median(shares$demovote + shares$repuvote) > 0.2)
  out <- file.path(OUTPUT_DIR, paste0("elect_he_cty_", lab, ".rds")); saveRDS(shares, out); r <- check_long_vs_source(long, out); stopifnot(all(r$pass))
  message(st, " ", yr, ": ", nrow(shares), " counties, ", nrow(long), " long rows, votes ", format(sum(long$votes), big.mark = ","))
}
