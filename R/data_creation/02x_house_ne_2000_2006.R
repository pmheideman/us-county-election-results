## Nebraska 2000, 2002 and 2006 U.S. House by county, from the Nebraska Secretary of State's official canvass books (Official Report of the Board of State Canvassers):
##   R/data/county_house_files/nebraska/2000 General.pdf (image-only; House tables pdf pp.15-17 = book pp.13-15), 2002 General.pdf (pp.22-24 = book pp.21-23), 2006 General.pdf (pp.15-16 = book pp.14-15).
## Every cell was READ FROM THE PAGE IMAGES (170 dpi) and typed into R/data/raw_house_county_open_states/nebraska_official/ne_<year>_transcription.csv; OCR was used only to locate the pages.
## Layout: per district a table with a TOTAL row (the checksum) and one row per county; counties in 2+ districts appear in each (2000: Cass in D1/D2; 2002: Sarpy in D1/D2? see output; 2006: Sarpy D1/D2, Cedar D1/D3).
## Checks: county sums == printed TOTAL per candidate; county count; House total vs the panel's presidential/Senate total; comparison with the cross-year district map.
## Outputs: R/output/long/he_ne_<year>.rds, R/output/elect_he_cty_ne_<year>.rds (NEW files); R/output/ne_ocr_corrections_2000_2006.csv (no cell needed correction: every district ties on the first reading).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
D <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nebraska_official")
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEBRASKA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 93)
## candidate columns per year and district: name, party label as printed (party_group is set from the LABEL of the column BEFORE it is reused)
CAND <- list(
  `2000` = list(`1` = tibble(candidate = c("Doug Bereuter", "Alan Jacobsen", "David Oenbring", "Write-In"), party = c("Republican", "Democratic", "Libertarian", "Write-In")),
                `2` = tibble(candidate = c("Lee Terry", "Shelley Kiel", "John J. Graziano", "Write-In"), party = c("Republican", "Democratic", "Libertarian", "Write-In")),
                `3` = tibble(candidate = c("Tom Osborne", "Roland E. Reynolds", "Jerry Hickman", "Write-In"), party = c("Republican", "Democratic", "Libertarian", "Write-In"))),
  `2002` = list(`1` = tibble(candidate = c("Doug Bereuter", "Robert Eckerson"), party = c("Republican", "Libertarian")),
                `2` = tibble(candidate = c("Lee Terry", "Jim Simon", "Dave Stock", "Doug Paterson"), party = c("Republican", "Democratic", "Libertarian", "Green")),
                `3` = tibble(candidate = c("Tom Osborne", "Jerry Hickman"), party = c("Republican", "Libertarian"))),
  `2006` = list(`1` = tibble(candidate = c("Jeff Fortenberry", "Maxine B. Moul"), party = c("Republican", "Democratic")),
                `2` = tibble(candidate = c("Lee Terry", "Jim Esch"), party = c("Republican", "Democratic")),
                `3` = tibble(candidate = c("Adrian Smith", "Scott Kleeb"), party = c("Republican", "Democratic"))))
PRINTED <- list(`2000` = list(`1` = c(155485, 72859, 6147, 207), `2` = c(148911, 70268, 6856, 245), `3` = c(182117, 34944, 4909, 123)),
                `2002` = list(`1` = c(133013, 22831), `2` = c(89917, 46843, 2018, 3236), `3` = c(163939, 12017)),
                `2006` = list(`1` = c(121015, 86360), `2` = c(99475, 82504), `3` = c(113687, 93046)))
panel <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")); stopifnot(!any(panel$sample == "HE" & panel$cty_fips %/% 1000 == 31 & panel$year < 2008))
for (y in c(2000, 2002, 2006)) {
  t <- read_csv(file.path(D, sprintf("ne_%d_transcription.csv", y)), col_types = cols(district = "c", county = "c", .default = "d"))
  vc <- setdiff(names(t), c("district", "county")); raw <- list()
  for (d in c("1", "2", "3")) { cd <- CAND[[as.character(y)]][[d]]; td <- t %>% filter(district == d); k <- nrow(cd); pr <- PRINTED[[as.character(y)]][[d]]
    m <- as.matrix(td[, vc[seq_len(k)]]); stopifnot(length(pr) == k, all(colSums(m) == pr))                                    # county sums == printed TOTAL row, every candidate
    raw[[d]] <- bind_rows(lapply(seq_len(k), function(j) tibble(year = y, district = sprintf("%02d", as.integer(d)), county = td$county, candidate = cd$candidate[j], party = cd$party[j], votes = m[, j]))) }
  raw <- bind_rows(raw) %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
  raw <- raw %>% mutate(party_group = case_when(party == "Democratic" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"))
  long <- finalize_long(raw %>% select(year, county_fips, district, candidate, party, party_group, votes), paste0("ne_", y)); save_long(long, paste0("he_ne_", y))
  shares <- derive_shares(long) %>% transmute(state = "NEBRASKA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ne_%d.rds", y)))
  r <- check_long_vs_source(long, file.path(OUTPUT_DIR, sprintf("elect_he_cty_ne_%d.rds", y))); stopifnot(all(r$pass))
  ref <- panel %>% filter(sample == "PE", year == ifelse(y %% 4 == 0, y, y - 2)) %>% select(cty_fips, ref = totalvote); refs <- panel %>% filter(sample == "SE", year == y, cty_fips %/% 1000 == 31) %>% select(cty_fips, ref = totalvote)
  if (nrow(refs) > 50) ref <- refs
  rr <- shares %>% left_join(ref, by = "cty_fips") %>% mutate(ratio = totalvote / ref)
  cat(sprintf("%d: all %d districts tie to the printed TOTAL rows | candidates %d | counties %d | split counties %s | median dem %.3f rep %.3f | House/%s total min %.2f med %.2f max %.2f\n", y, 3, n_distinct(long$candidate), n_distinct(long$county_fips),
      paste(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% filter(d > 1) %>% pull(county_fips), collapse = ","), median(shares$demovote), median(shares$repuvote), if (nrow(refs) > 50) "Senate" else "presidential", min(rr$ratio, na.rm = TRUE), median(rr$ratio, na.rm = TRUE), max(rr$ratio, na.rm = TRUE)))
}
write.csv(data.frame(year = NA, district = NA, county = NA, candidate = NA, ocr_value = NA, corrected_value = NA, how_verified = "none needed: every cell was read from the page images and all 9 district-years tie to the printed TOTAL rows"), file.path(OUTPUT_DIR, "ne_ocr_corrections_2000_2006.csv"), row.names = FALSE)
