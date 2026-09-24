## Washington U.S. House 2010, county x congressional district x candidate totals, from the Secretary of State's 2010 General Election data download
## (https://www.sos.wa.gov/sites/default/files/2022-05/2010-general-data.zip -> "Precinct Data/2010Gen by-precinct data (by county).zip" -> one zip per county, saved unpacked under
## R/data/raw_house_county_open_states/washington_archive/2010/counties/<County>/). The counties' files come in five layouts, all read here:
##   A  "Contest_title / candidate_name / total_votes" per precinct (Adams, Asotin, Benton, Chelan, Clallam, Clark, Columbia, Cowlitz, Ferry, Garfield, Island, Kittitas, Klickitat, Lewis, Lincoln, Mason,
##      Okanogan, Pacific, San Juan, Skagit, Skamania, Stevens, Wahkiakum, Yakima)
##   B  "Contest Title / Candidate Name / Votes" (Douglas, Grant, Grays Harbor, Jefferson, Pend Oreille, Spokane, Thurston, Walla Walla, Whitman)
##   C  "CONTEST_FULL_NAME / CANDIDATE_FULL_NAME / TOTAL" (Franklin, Snohomish, Whatcom)
##   D  King: CumulativeCanvass.txt (Race, CGD, CounterGroup "Total", CounterType = candidate, SumOfCount)
##   E  report-style sheets: Pierce (one sheet per congressional district, two rows per precinct: vote by mail + polling place) and Kitsap (one sheet per district, "Grand Totals" row).
## Why: the earlier 2010 rows came from the OpenElections statewide precinct file, which (besides misspelling two counties) has errors: King's CD9 candidates are swapped (Smith 39,757 / Muri 59,201; the SOS
## file has Smith 59,201 / Muri 39,757), and several districts differ from the FEC's official results (CD2, CD3, CD6, CD9). This build reads the SOS files directly and is tested against the FEC (below).
## Party labels: the SOS files carry none, so parties come from a fixed table (Democratic / Republican; Bob Jeffers-Schroeder is the Independent in CD7), checked against FEC results.
## Outputs: R/output/wa_2010_sos_county_district_candidate.rds (+ a printed comparison with the OpenElections rows and with the FEC district results).
source(file.path("R", "data_creation", "02w_common.R"))
library(readxl)
WA <- file.path(RAW_ROOT, "washington_archive", "2010", "counties")
stopifnot(dir.exists(WA))
key <- function(x) gsub("[^A-Z]", "", toupper(x))
is_rep <- function(x) { u <- toupper(x); grepl("CONGRESS|UNITED STATES REP|U\\.?\\s*S\\.?\\s*REP", u) & !grepl("LEGISLATIVE|STATE REP|STATE SEN|SENATOR", u) }
first_file <- function(d) list.files(d, pattern = "\\.(csv|xls|txt)$", full.names = TRUE)[1]
read_any <- function(f, sheet = 1) if (grepl("\\.csv$", f)) read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, colClasses = "character", fileEncoding = "latin1") else suppressMessages(read_excel(f, sheet = sheet, .name_repair = "minimal", col_types = "text"))
pick <- function(d, pats) { for (p in pats) { i <- which(tolower(names(d)) == tolower(p)); if (length(i)) return(d[[i[1]]]) }; stop("column not found: ", paste(pats, collapse = "/")) }

std_AB <- function(f, sheet = 1) { d <- read_any(f, sheet)
  ct <- pick(d, c("Contest_title", "Contest Title")); cn <- pick(d, c("candidate_name", "Candidate Name")); v <- suppressWarnings(as.numeric(pick(d, c("total_votes", "Votes")))); cd <- pick(d, "Cong_Dist")
  tibble(cong = suppressWarnings(as.integer(cd)), candidate = sub("^ALL - ", "", trimws(cn)), contest = ct, votes = v) %>% filter(is_rep(contest), !is.na(votes)) }
std_C <- function(f, sheet = 1) { d <- read_any(f, sheet)
  tibble(cong = suppressWarnings(as.integer(pick(d, "Cong_Dist"))), candidate = sub("^ALL - ", "", trimws(pick(d, "CANDIDATE_FULL_NAME"))), contest = pick(d, "CONTEST_FULL_NAME"), votes = suppressWarnings(as.numeric(pick(d, "TOTAL")))) %>% filter(is_rep(contest), !is.na(votes)) }
std_D <- function(f) { d <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "latin1")
  d <- d %>% filter(grepl("United States Representative", Race), CounterGroup == "Total", !grepl("Registered Voters|Times|Write-in|Undervote|Overvote", CounterType))
  tibble(cong = as.integer(d$CGD), candidate = trimws(d$CounterType), contest = d$Race, votes = as.numeric(d$SumOfCount)) %>% group_by(cong, candidate, contest) %>% summarise(votes = sum(votes), .groups = "drop") }
std_pierce <- function(f) { out <- list()
  for (sh in excel_sheets(f)) { d <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE, .name_repair = "minimal", col_types = "text")); if (nrow(d) < 5) next
    t <- as.character(unlist(d[1:8, ])); m <- t[grepl("U\\.S\\. Rep\\. \\d+(st|nd|rd|th) Congressional", t)]; if (!length(m)) next
    cong <- as.integer(sub("^.*Rep\\. (\\d+).*$", "\\1", m[1])); hr <- which(apply(d, 1, function(r) any(grepl("^Registered$", r))))[1]; nmrow <- hr
    hdr <- as.character(unlist(d[nmrow, ])); ci <- which(!is.na(hdr) & !hdr %in% c("Registered", "Ballots Cast", "Turnout (%)", "Write-In", "Over Votes", "Under Votes", "Blank Ballots"))
    body <- d[(nmrow + 1):nrow(d), ]; tot <- body[trimws(as.character(body[[1]])) %in% "Contest Total", ]; stopifnot(nrow(tot) == 1)   # precinct cells are "***" where suppressed for voter privacy: the printed Contest Total is used
    for (i in ci) out[[length(out) + 1]] <- tibble(cong = cong, candidate = trimws(hdr[i]), contest = m[1], votes = as.numeric(tot[[i]])) }
  bind_rows(out) }
std_kitsap <- function(f) { out <- list()
  for (sh in excel_sheets(f)) { if (!grepl("^US Rep Cong", sh)) next; d <- suppressMessages(read_excel(f, sheet = sh, .name_repair = "minimal", col_types = "text")); cong <- as.integer(sub("^.*Cong ", "", sh))
    g <- d[which(d$name == "Grand Totals"), ]; stopifnot(nrow(g) == 1); cn <- setdiff(names(d)[10:ncol(d)], c("Write In", "Undervotes", "Overvotes", "Blank Ballots"))
    for (c in cn) out[[length(out) + 1]] <- tibble(cong = cong, candidate = trimws(c), contest = sh, votes = as.numeric(g[[c]])) }
  bind_rows(out) }

fmt <- c(Adams = "A", Asotin = "A", Benton = "A", Chelan = "A", Clallam = "A", Clark = "A", Columbia = "A", Cowlitz = "A", Ferry = "A", Garfield = "A", Island = "A", Kittitas = "A", Klickitat = "A", Lewis = "A", Lincoln = "A",
         Mason = "A", Okanogan = "A", Pacific = "A", `San Juan` = "A", Skagit = "A", Skamania = "A", Stevens = "A", Wahkiakum = "A", Yakima = "A",
         Douglas = "B", Grant = "B", `Grays Harbor` = "B", Jefferson = "B", `Pend Oreille` = "B", Spokane = "B", Thurston = "B", `Walla Walla` = "B", Whitman = "B",
         Franklin = "C", Snohomish = "C", Whatcom = "C", King = "D", Pierce = "E", Kitsap = "E")
stopifnot(length(fmt) == 39, setequal(names(fmt), list.files(WA)))
res <- list()
for (cty in names(fmt)) {
  d <- file.path(WA, cty); fs <- list.files(d, full.names = TRUE); fs <- fs[!grepl("RV,cast\\.csv$", fs) | length(fs) == 1]
  r <- switch(fmt[[cty]],
    A = , B = { f <- fs[1]; if (grepl("\\.xls$", f)) std_AB(f, 1) else std_AB(f) },
    C = { f <- fs[1]; std_C(f, 1) },
    D = std_D(fs[1]),
    E = if (cty == "Pierce") std_pierce(fs[1]) else std_kitsap(fs[1]))
  res[[cty]] <- r %>% mutate(county = toupper(cty))
}
CANDS <- tribble(~pat, ~candidate, ~party,
  "INSLEE", "Jay Inslee", "Democratic", "WATKINS", "James Watkins", "Republican", "LARSEN", "Rick Larsen", "Democratic", "KOSTER", "John Koster", "Republican",
  "HECK", "Denny Heck", "Democratic", "HERRERA", "Jaime Herrera Beutler", "Republican", "HASTINGS", "Doc Hastings", "Republican", "CLOUGH", "Jay Clough", "Democratic",
  "RODGERS", "Cathy McMorris Rodgers", "Republican", "ROMEYN", "Daryl Romeyn", "Democratic", "DICKS", "Norm Dicks", "Democratic", "CLOUD", "Doug Cloud", "Republican",
  "MCDERMOTT", "Jim McDermott", "Democratic", "JEFFERS", "Bob Jeffers-Schroeder", "Independent", "REICHERT", "Dave Reichert", "Republican", "DELBENE", "Suzan DelBene", "Democratic",
  "SMITH", "Adam Smith", "Democratic", "MURI", "Dick Muri", "Republican")
canon <- function(x) { k <- key(x); hit <- vapply(k, function(z) { h <- which(vapply(CANDS$pat, function(p) grepl(p, z, fixed = TRUE), NA)); if (length(h) == 1) h else NA_integer_ }, 1L); hit }
sos <- bind_rows(res) %>% mutate(ix = canon(candidate)) %>% filter(!is.na(ix), !grepl("WRITE", toupper(candidate))) %>% mutate(candidate = CANDS$candidate[ix], party = CANDS$party[ix]) %>%
  group_by(county, cong, candidate, party) %>% summarise(votes = sum(votes), .groups = "drop")
saveRDS(sos, file.path(OUTPUT_DIR, "wa_2010_sos_county_district_candidate.rds"))
cat("SOS 2010 rows:", nrow(sos), "| counties:", n_distinct(sos$county), "| districts:", paste(sort(unique(sos$cong)), collapse = ","), "\n")
## test against the FEC's official district results (from R/qa_state_reconcile_congress.R output; Democratic and Republican votes per district)
fec <- read.csv(file.path(OUTPUT_DIR, "qa_state_reconcile_house.csv")) %>% filter(state_po == "WA", year == 2010) %>% transmute(cong = as.integer(district), off_dem, off_rep)
cmp <- sos %>% group_by(cong) %>% summarise(dem = sum(votes[party == "Democratic"]), rep = sum(votes[party == "Republican"]), .groups = "drop") %>% left_join(fec, by = "cong") %>% mutate(d_dem = dem - off_dem, d_rep = rep - off_rep)
cat("SOS district totals minus FEC official (Democratic, Republican):\n"); print(as.data.frame(cmp))
## and with the OpenElections rows the earlier build used
oe <- read_csv(file.path(RAW_ROOT, "washington", "2010_general_precinct.csv"), show_col_types = FALSE, col_types = cols(.default = "c")) %>% filter(trimws(office) == "US House", !is.na(candidate), trimws(candidate) != "") %>%
  distinct(county, precinct, district, candidate, party, votes) %>% mutate(county = toupper(trimws(county)), county = ifelse(county == "WAHKIUKUM", "WAHKIAKUM", ifelse(county == "PEND O'REILLE", "PEND OREILLE", county)), votes = as.numeric(votes)) %>%
  filter(!is.na(votes)) %>% group_by(county, cong = as.integer(sub("^CD", "", district)), k = key(candidate)) %>% summarise(oe = sum(votes), .groups = "drop")
dd <- sos %>% mutate(k = key(candidate)) %>% mutate(k = ifelse(k == "CATHYMCMORRISRODGERS", "CATHYMCMORRISRODGERS", k)) %>% full_join(oe %>% mutate(k = ifelse(grepl("DICKMURI", k), "DICKMURI", ifelse(grepl("JAIMEHERRERA", k), "JAIMEHERRERABEUTLER", ifelse(grepl("JEFFERS", k), "BOBJEFFERSSCHROEDER", k)))), by = c("county", "cong", "k"))
cat("county x district x candidate cells where the SOS files and the OpenElections rows differ by more than 5 votes:\n")
print(as.data.frame(dd %>% filter(is.na(votes) | is.na(oe) | abs(votes - oe) > 5) %>% select(county, cong, k, sos = votes, oe)))
