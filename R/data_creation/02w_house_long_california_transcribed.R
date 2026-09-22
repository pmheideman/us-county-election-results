## Long tables for California 1990/1992/1994/1996 (01bf/01bg/01bh/01be): hand-transcribed county x district vote columns
## (R/data/county_house_files/ca_manual_transcription/<year>_raw.csv). The CSVs carry NO candidate names: per the original logic column 1 = DEM,
## column 2 = REP, the rest OTHER (in printed order). Candidate labels are therefore placeholders ("Democratic candidate", "Republican candidate",
## "Other candidate n"); real names would need re-reading the PDFs. District is kept.
source(file.path("R", "data_creation", "02w_common.R"))
ca_fips <- fips_of("CALIFORNIA"); stopifnot(nrow(ca_fips) == 58)
one <- function(year) {
  L <- readLines(file.path(PROJECT_ROOT, "R", "data", "county_house_files", "ca_manual_transcription", paste0(year, "_raw.csv"))); L <- L[nchar(trimws(L)) > 0]
  sp <- strsplit(L, ",", fixed = TRUE); mx <- max(lengths(sp))
  raw <- as_tibble(do.call(rbind, lapply(sp, function(x) { length(x) <- mx; x })), .name_repair = "minimal"); names(raw) <- paste0("X", seq_len(mx))
  raw <- raw %>% mutate(across(-X2, as.numeric))
  raw %>% rename(district = X1, county = X2) %>% pivot_longer(-c(district, county), values_to = "votes", names_to = "col") %>% filter(!is.na(votes)) %>%
    group_by(district, county) %>% mutate(rank = row_number()) %>% ungroup() %>%
    mutate(year = year, party_group = case_when(rank == 1 ~ "DEM", rank == 2 ~ "REP", TRUE ~ "OTHER"),
           candidate = case_when(rank == 1 ~ "Democratic candidate", rank == 2 ~ "Republican candidate", TRUE ~ paste("Other candidate", rank - 2)),
           party = case_when(rank == 1 ~ "Democratic", rank == 2 ~ "Republican", TRUE ~ "Other")) %>%
    select(year, county, district, candidate, party, party_group, votes)
}
for (y in c(1990, 1992, 1994, 1996)) {
  long <- one(y) %>% inner_join(ca_fips, by = c("county" = "county_name")) %>% select(-county)
  long <- finalize_long(long, paste0("ca_", y)); save_long(long, paste0("he_ca_", y))
  message("CA ", y, " long rows: ", nrow(long), " | districts: ", n_distinct(long$district)); print(check_long_vs_source(long, SRC(paste0("ca_", y))))
}
