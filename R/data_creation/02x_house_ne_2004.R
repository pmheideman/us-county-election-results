## Nebraska 2004 U.S. House by county, from the Nebraska Secretary of State "2004 General Election" official canvass book
## (R/data/county_house_files/nebraska/2004 General.pdf, native text; House tables on pages 10-11: "Member of the U.S. House of Representatives", blocks District One/Two/Three, columns
## County + one column per candidate with the party on the next line, a TOTAL row and one row per county; Sarpy County is in districts 1 and 2).
## Checks: county sums == the printed TOTAL row per candidate; county count; House total vs the presidential total in the same book (page 8-9) and the panel.
## Outputs: R/output/long/he_ne_2004.rds, R/output/elect_he_cty_ne_2004.rds (NEW files).
source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R")); library(readr)
PDF <- file.path(PROJECT_ROOT, "R", "data", "county_house_files", "nebraska", "2004 General.pdf")
TXT <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states", "nebraska_official", "NE_2004_General.txt"); dir.create(dirname(TXT), showWarnings = FALSE, recursive = TRUE)
if (!file.exists(TXT) || file.size(TXT) == 0) system2("pdftotext", c("-layout", shQuote(PDF), shQuote(TXT)))
pg <- strsplit(paste(readLines(TXT, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\f")[[1]]
i0 <- which(grepl("Member of the U.S. House of Representatives", pg) & !grepl("\\.{6}", pg))[1]; stopifnot(!is.na(i0))
L <- unlist(strsplit(paste(pg[i0:(i0 + 1)], collapse = "\n"), "\n")); L <- L[!grepl("^\\s*[0-9]+\\s*$", L)]
xw <- read_delim(file.path(PROJECT_ROOT, "R/data/raw_election/countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>% filter(state == "NEBRASKA", !is.na(county_fips)) %>% distinct(county_name, county_fips)
norm <- function(z) gsub("[^A-Z]", "", toupper(z)); xw <- xw %>% mutate(key = norm(county_name)) %>% distinct(key, county_fips); stopifnot(nrow(xw) == 93)
DW <- c(One = "01", Two = "02", Three = "03"); rows <- list(); tot <- list(); cur <- NULL; names_c <- NULL; parties <- NULL; state <- "none"
for (ln in L) { t <- trimws(ln)
  if (grepl("^District (One|Two|Three)$", t)) { cur <- DW[[sub("District ", "", t)]]; state <- "await_names"; next }
  if (is.null(cur)) next
  if (state == "await_names" && grepl("^County", t)) { names_c <- strsplit(sub("^County\\s+", "", t), "\\s{2,}")[[1]]; state <- "await_party"; next }
  if (state == "await_party") { parties <- strsplit(t, "\\s{2,}")[[1]]; stopifnot(length(parties) == length(names_c)); state <- "rows"; next }
  if (state == "rows") { m <- regmatches(t, regexec("^(.+?)\\s+((?:[0-9]+\\s*)+)$", t, perl = TRUE))[[1]]; if (!length(m)) next
    v <- as.numeric(strsplit(trimws(m[3]), "\\s+")[[1]]); if (length(v) != length(names_c)) stop("cell count mismatch in: ", t)
    if (m[2] == "TOTAL") tot[[length(tot) + 1]] <- data.frame(district = cur, candidate = names_c, printed = v)
    else rows[[length(rows) + 1]] <- data.frame(district = cur, county = m[2], candidate = names_c, party = parties, votes = v) } }
raw <- bind_rows(rows); tot <- bind_rows(tot)
ck <- raw %>% group_by(district, candidate) %>% summarise(sum = sum(votes), n_cty = n(), .groups = "drop") %>% left_join(tot, by = c("district", "candidate")) %>% mutate(ok = sum == printed)
cat("candidate lines:", nrow(ck), "| county sums equal the printed TOTAL:", sum(ck$ok), "\n"); print(as.data.frame(ck))
stopifnot(all(ck$ok))
raw <- raw %>% mutate(county_fips = xw$county_fips[match(norm(county), xw$key)]); stopifnot(!anyNA(raw$county_fips))
raw <- raw %>% transmute(year = 2004L, county_fips, district, candidate, party_group = case_when(party == "Democrat" ~ "DEM", party == "Republican" ~ "REP", TRUE ~ "OTHER"),   # party_group BEFORE the label
                         party = case_when(party == "Democrat" ~ "Democratic", party == "Write In" ~ "Write-In", party == "Nebraska" ~ "Nebraska Party", TRUE ~ party), votes)
long <- finalize_long(raw, "ne_2004"); save_long(long, "he_ne_2004")
shares <- derive_shares(long) %>% transmute(state = "NEBRASKA", year, cty_fips, sample, demovote, repuvote, totalvote); saveRDS(shares, file.path(OUTPUT_DIR, "elect_he_cty_ne_2004.rds"))
print(check_long_vs_source(long, file.path(OUTPUT_DIR, "elect_he_cty_ne_2004.rds")) %>% select(source, keys_source, matched, mismatched, pass))
cat("counties:", n_distinct(long$county_fips), "| split counties:", sum(long %>% group_by(county_fips) %>% summarise(d = n_distinct(district)) %>% pull(d) > 1), "| median dem", round(median(shares$demovote), 3), "rep", round(median(shares$repuvote), 3), "\n")
pe <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "PE", year == 2004) %>% select(cty_fips, pe = totalvote)
r <- shares %>% left_join(pe, by = "cty_fips") %>% mutate(ratio = totalvote / pe); cat("House / presidential 2004 total: min", round(min(r$ratio, na.rm = TRUE), 3), "median", round(median(r$ratio, na.rm = TRUE), 3), "max", round(max(r$ratio, na.rm = TRUE), 3), "\n")
old <- readRDS(file.path(OUTPUT_DIR, "elect_cty_final.rds")) %>% filter(sample == "HE", year == 2004, cty_fips %/% 1000 == 31); cat("panel rows for NE 2004:", nrow(old), "\n")
