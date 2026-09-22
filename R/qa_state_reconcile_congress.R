## Independent check of the HOUSE (by district) and SENATE (by state) county rows against the FEC's official results (R/data/fec_official/).
## Ours = release/v0.2.0/us_county_results_long.csv summed over counties. Official = FEC "Federal Elections <year>" congressional result sheets (2004-2022; header-driven reader,
## the layout differs by year). 2000 has no candidate table here (OCR book only) and 2024 has no FEC book: both are outside this check.
## House: per (year, state, district): Democratic votes, Republican votes (party-line based: FEC party D/DFL/DNL vs our DEM party_group; R vs REP) and the printed "District Votes" total.
## Senate: per (year, state): the two nominees (matched on last name; fusion states use the FEC combined column) and the state total; states with 2 Senate races in a year are skipped (special elections).
## Runoff states: where the FEC prints GE-runoff votes (GA, LA) the runoff votes replace the first-round votes (our long table keeps the decisive round).
## Outputs: R/output/qa_state_reconcile_house.csv, qa_state_reconcile_senate.csv (+ printed summary).
source(file.path("R", "00_setup.R")); library(readxl); library(readr)
FEC <- file.path(PROJECT_ROOT, "R", "data", "fec_official"); SKIP <- c("AK", "HI", "DC")
DSET <- c("D", "DEM", "DFL", "DNL", "D-NPL", "DNPL"); RSET <- c("R", "REP")
key_letters <- function(x) gsub("[^A-Z]", "", toupper(iconv(x, to = "ASCII//TRANSLIT")))
key_ours <- function(x) { x <- toupper(iconv(x, to = "ASCII//TRANSLIT")); x <- gsub("[.,]?\\s+(JR|SR|II|III|IV)\\.?$", "", trimws(x)); key_letters(sub("^.*\\s", "", x)) }
key_off <- function(x) key_letters(sub("\\s+(JR|SR|II|III|IV)\\.?$", "", trimws(x), ignore.case = TRUE))

read_cong <- function(file, sheet, year) {
  d <- suppressWarnings(suppressMessages(read_excel(file.path(FEC, file), sheet = sheet, col_names = FALSE, .name_repair = "minimal", guess_max = 10000))); hdr <- toupper(trimws(as.character(unlist(d[1, ]))))
  ci <- function(pat, excl = NULL) { i <- which(grepl(pat, hdr)); if (!is.null(excl)) i <- i[!grepl(excl, hdr[i])]; i[1] }
  st <- ci("STATE ABBREVIATION"); di <- ci("^(DISTRICT|D)$"); ln <- ci("(LAST NAME$|NAME \\(LAST\\)$)", "LAST NAME,"); pty <- ci("^PARTY$"); tv <- ci("^TOTAL VOTES$")
  gv <- ci("^GENERAL( VOTES)?$"); ru <- ci("^GE RUNOFF( ELECTION)?( VOTES)?( \\(.*\\))?$", "%"); cm <- ci("^COMBINED GE PARTY TOTALS")
  stopifnot(!anyNA(c(st, di, ln, pty, tv, gv)))
  d <- d[-1, ]; g <- function(i) if (is.na(i)) rep(NA_real_, nrow(d)) else suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(d[[i]]))))
  tibble(year = year, state_po = as.character(d[[st]]), district = trimws(as.character(d[[di]])), last = as.character(d[[ln]]), party = trimws(as.character(d[[pty]])), label = trimws(as.character(d[[tv]])),
         gen = g(gv), runoff = g(ru), combined = g(cm))
}
srcs <- list(list("2004congresults.xls", "2004 US HOUSE & SENATE RESULTS", 2004), list("FederalElections2002_House.xlsx", NA, 2002), list("results06.xls", "2006 US House & Senate Results", 2006),
             list("2008congresults.xls", "2008 House and Senate Results", 2008), list("results10.xls", "2010 US House & Senate Results", 2010), list("2012congresults.xls", "2012 US House & Senate Results", 2012),
             list("results14.xls", "2014 US House Results by State", 2014), list("results14.xls", "2014 US Senate Results by State", 2014),
             list("federalelections2016.xlsx", "2016 US House Results by State", 2016), list("federalelections2016.xlsx", "2016 US Senate Results by State", 2016),
             list("federalelections2018.xlsx", "2018 US House Results by State", 2018), list("federalelections2018.xlsx", "2018 US Senate Results by State", 2018),
             list("federalelections2020.xlsx", "13. US House Results by State", 2020), list("federalelections2020.xlsx", "12. US Senate Results by State", 2020),
             list("federalelections2022.xlsx", "8. US House Results by State", 2022), list("federalelections2022.xlsx", "7. US Senate Results by State", 2022))
srcs <- Filter(function(s) !is.na(s[[2]]), srcs)
off <- bind_rows(lapply(srcs, function(s) read_cong(s[[1]], s[[2]], s[[3]]))) %>% filter(!is.na(state_po), nchar(state_po) == 2, !state_po %in% SKIP)
cat("official rows read:", nrow(off), "| district codes seen (non-numeric):", paste(sort(unique(off$district[!grepl("^[0-9]+$", off$district)])), collapse = " | "), "\n")
saveRDS(off, file.path(OUTPUT_DIR, "fec_congress_rows.rds"))

## ---- normalise official districts: regular election only ---------------------------------------------------------------------------------------------------------------
off <- off %>% mutate(dc = toupper(gsub("[ *]", "", district)), office = ifelse(grepl("^S", dc), "senate", "house")) %>%
  filter(!grepl("UNEXPIRED|^SUN", dc), !dc %in% c("H", "DISTRICT", "")) %>%
  mutate(district = ifelse(office == "house", sprintf("%02d", suppressWarnings(as.integer(sub("-?FULLTERM$", "", dc)))), "S"))
stopifnot(!anyNA(off$district))
pcls <- function(p) { b <- toupper(sub("[^A-Za-z-].*$", "", sub("^[^A-Za-z]+", "", iconv(ifelse(is.na(p), "", p), to = "ASCII//TRANSLIT")))); ifelse(b %in% DSET, "D", ifelse(b %in% RSET, "R", "O")) }   # cut at the first character that is not a letter or hyphen (space, NBSP, "/", "*", ",")   # "D/IP" -> D, "R*" -> R
cand <- off %>% filter(is.na(label) | label == "", !is.na(last), !grepl("Scattered|^Other|Write", last, ignore.case = TRUE))
## one row per candidate: the FEC prints a party-less candidate TOTAL row above the per-party-line rows for candidates on several lines (NY, CT, OR, SC ...); else the combined column; else the row itself
cand_tot <- function(d, votecol) d %>% mutate(v = .data[[votecol]]) %>% filter(!is.na(v)) %>% group_by(year, state_po, district, ck = key_off(last)) %>%
  summarise(cand = first(last), cls = { c <- pcls(party); ifelse(any(c == "D"), "D", ifelse(any(c == "R"), "R", "O")) }, off_votes = { np <- is.na(party) | party == ""; ifelse(any(np), max(v[np]), ifelse(any(!is.na(combined)), max(combined, na.rm = TRUE), sum(v))) }, .groups = "drop")
stat <- function(rel) case_when(is.na(rel) ~ "MISSING", abs(rel) < 0.0005 ~ "ok (<0.05%)", abs(rel) < 0.005 ~ "small (<0.5%)", abs(rel) < 0.02 ~ "check (0.5-2%)", TRUE ~ "BAD (>=2%)")

rel <- read_csv(file.path(PROJECT_ROOT, "release", "v0.2.0", "us_county_results_long.csv"), col_types = cols(.default = "c", votes = "d", year = "i"), show_col_types = FALSE) %>% filter(office %in% c("house", "senate"), year >= 2004)

## ---- HOUSE ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ch_ <- cand %>% filter(office == "house") %>% group_by(year, state_po, district) %>% mutate(use_runoff = any(!is.na(runoff))) %>% ungroup() %>% mutate(vv = ifelse(use_runoff, runoff, gen))
oh <- cand_tot(ch_, "vv") %>% group_by(year, state_po, district) %>% summarise(off_dem = sum(off_votes[cls == "D"]), off_rep = sum(off_votes[cls == "R"]), off_sum = sum(off_votes), .groups = "drop")
odt <- off %>% filter(office == "house", label == "District Votes:") %>% group_by(year, state_po, district) %>% summarise(off_total = max(coalesce(ifelse(any(!is.na(runoff)), runoff, NA_real_), gen), na.rm = TRUE), .groups = "drop") %>% filter(is.finite(off_total))
oh <- oh %>% left_join(odt, by = c("year", "state_po", "district")) %>% mutate(off_total = coalesce(off_total, off_sum))
at_large <- oh %>% group_by(year, state_po) %>% summarise(al = all(district == "00"), .groups = "drop") %>% filter(al) %>% select(-al)   # states with a single district: any of our codes ("1", "01", "00") means the same
hl <- rel %>% filter(office == "house", year %in% unique(oh$year)) %>% mutate(district = ifelse(is.na(district) | district == "", "", ifelse(grepl("^[0-9]+$", district), sprintf("%02d", as.integer(district)), district))) %>%
  left_join(at_large %>% mutate(al = TRUE), by = c("year", "state_po")) %>% mutate(district = ifelse(!is.na(al) & district != "", "00", district), ck = key_ours(candidate))
## our nominee votes = ALL votes of every candidate that has a DEM (REP) row, so a candidate on a second party line (Working Families, Conservative ...) counts once, like the FEC's candidate total
hl <- hl %>% group_by(year, state_po, district, ck) %>% mutate(cand_dem = any(party_group == "DEM"), cand_rep = any(party_group == "REP")) %>% ungroup()
uh <- hl %>% filter(district != "") %>% group_by(year, state_po, district) %>%
  summarise(our_dem = sum(votes[cand_dem]), our_rep = sum(votes[cand_rep & !cand_dem]), our_total = sum(votes), n_counties = n_distinct(county_fips), .groups = "drop")
ch <- oh %>% full_join(uh, by = c("year", "state_po", "district")) %>%
  mutate(rel_dem = (our_dem - off_dem) / pmax(off_dem, 1), rel_rep = (our_rep - off_rep) / pmax(off_rep, 1), rel_tot = (our_total - off_total) / pmax(off_total, 1),
         s_dem = stat(ifelse(off_dem == 0 & our_dem == 0, 0, rel_dem)), s_rep = stat(ifelse(off_rep == 0 & our_rep == 0, 0, rel_rep)), s_tot = stat(rel_tot))
## state-years whose rows carry NO district (some county-only sources): compared at state level against the sum of the FEC districts
nd <- hl %>% filter(district == "") %>% group_by(year, state_po) %>% summarise(our_dem = sum(votes[cand_dem]), our_rep = sum(votes[cand_rep & !cand_dem]), our_total = sum(votes), n_counties = n_distinct(county_fips), .groups = "drop")
chs <- nd %>% inner_join(oh %>% group_by(year, state_po) %>% summarise(off_dem = sum(off_dem), off_rep = sum(off_rep), off_total = sum(off_total), .groups = "drop"), by = c("year", "state_po")) %>%
  mutate(rel_dem = (our_dem - off_dem) / off_dem, rel_rep = (our_rep - off_rep) / off_rep, rel_tot = (our_total - off_total) / off_total, s_dem = stat(rel_dem), s_rep = stat(rel_rep), s_tot = stat(rel_tot))
ch <- ch %>% anti_join(nd, by = c("year", "state_po")) %>% bind_rows(chs %>% mutate(district = "ALL"))   # districts of those state-years would only show as "official only"
write_csv(chs, file.path(OUTPUT_DIR, "qa_state_reconcile_house_blank_district.csv"))
write_csv(ch, file.path(OUTPUT_DIR, "qa_state_reconcile_house.csv"))
cat("\n=== HOUSE districts compared:", sum(!is.na(ch$off_total) & !is.na(ch$our_total)), "| official only (ours missing):", sum(is.na(ch$our_total)), "| ours only (no FEC match):", sum(is.na(ch$off_total)), "\n")
cat("D+R checks by status:\n"); print(as.data.frame(bind_rows(ch %>% transmute(s = s_dem), ch %>% transmute(s = s_rep)) %>% count(s)))
cat("district TOTAL by year x status:\n"); print(as.data.frame(ch %>% count(year, s_tot) %>% tidyr::pivot_wider(names_from = s_tot, values_from = n, values_fill = 0)))

## ---- SENATE ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
os_ <- cand %>% filter(office == "senate") %>% group_by(year, state_po) %>% mutate(use_runoff = any(!is.na(runoff))) %>% ungroup() %>% mutate(vv = ifelse(use_runoff, runoff, gen))
osn <- cand_tot(os_, "vv") %>% group_by(year, state_po) %>% summarise(off_dem = sum(off_votes[cls == "D"]), off_rep = sum(off_votes[cls == "R"]), off_sum = sum(off_votes), .groups = "drop")
ost <- off %>% filter(office == "senate", grepl("Votes", label), grepl("Total|District", label)) %>% group_by(year, state_po) %>% summarise(off_total = sum(coalesce(ifelse(!is.na(runoff), runoff, NA_real_), gen), na.rm = TRUE), .groups = "drop")
sl <- rel %>% filter(office == "senate", year %in% unique(osn$year)) %>% mutate(fk = key_letters(sub(",?\\s+(JR|SR|II|III|IV)\\.?$", "", trimws(candidate), ignore.case = TRUE))) %>%
  group_by(year, state_po, fk) %>% mutate(cand_dem = any(party_group == "DEM"), cand_rep = any(party_group == "REP")) %>% ungroup()   # a candidate on two party lines counts once, under the major-party line
us <- sl %>% group_by(year, state_po) %>% summarise(our_dem = sum(votes[cand_dem]), our_rep = sum(votes[cand_rep & !cand_dem]), our_total = sum(votes), n_counties = n_distinct(county_fips), .groups = "drop")
cs <- osn %>% left_join(ost, by = c("year", "state_po")) %>% mutate(off_total = ifelse(is.na(off_total) | off_total == 0, off_sum, off_total)) %>% inner_join(us, by = c("year", "state_po")) %>%
  mutate(rel_dem = (our_dem - off_dem) / pmax(off_dem, 1), rel_rep = (our_rep - off_rep) / pmax(off_rep, 1), rel_tot = (our_total - off_total) / off_total,
         s_dem = stat(ifelse(off_dem == 0 & our_dem == 0, 0, rel_dem)), s_rep = stat(ifelse(off_rep == 0 & our_rep == 0, 0, rel_rep)), s_tot = stat(rel_tot))
write_csv(cs, file.path(OUTPUT_DIR, "qa_state_reconcile_senate.csv"))
cat("\n=== SENATE state-years compared:", nrow(cs), "| ours without an FEC match:", nrow(anti_join(us, osn, by = c("year", "state_po"))), "| FEC without ours:", nrow(anti_join(osn, us, by = c("year", "state_po"))), "\n")
cat("D+R checks by status:\n"); print(as.data.frame(bind_rows(cs %>% transmute(s = s_dem), cs %>% transmute(s = s_rep)) %>% count(s)))
cat("state TOTAL by status:\n"); print(as.data.frame(count(cs, s_tot)))
