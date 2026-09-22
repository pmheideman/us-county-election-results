## Full candidate names for House rows whose source printed only a surname or a placeholder ("Democratic Candidate", "Other Candidate 2", ...).
## Names come from Wikipedia's "<year> United States House of Representatives elections in <State>" pages (district sections with election boxes:
## party, candidate, votes), fetched as raw wikitext and cached. Only NAMES are changed; votes and party groups are untouched.
##
## Matching (per year + state + district; every rename must be UNIQUE, and the method and evidence are recorded):
##   vote_total   our candidate's district vote total equals exactly one Wikipedia candidate's votes in that district (strongest; works for placeholders)
##   surname      our name is a surname (or partial name) that equals the last token of exactly one Wikipedia candidate in the district, and the party
##                group is compatible (DEM<->Democratic, REP<->Republican; minor parties only need to be "not D/R")
##   party_D_R    placeholder "Democratic/Republican Candidate": the district has exactly one Democratic (Republican) candidate on Wikipedia and our vote
##                total is within 10% of it (Wikipedia has a few typos, e.g. CA-3 1992 Fazio 112,149 vs the official 122,149)
## Unmatched candidates keep their old name and stay flagged. Wikipedia pages that are redirects to the national page (e.g. Maine 1990) are
## reported as such and handled by the national-page pass below (surname only, statewide).
## Output: R/data/raw_election/candidate_name_overrides.csv (year, state_fips, district, candidate_old, candidate_new, method, evidence),
##         R/output/candidate_name_matching_report.csv (every flagged candidate with the outcome).
## The overrides are applied by 02z_house_long_assemble.R.

source(file.path("R", "00_setup.R")); source(file.path("R", "long_helpers.R"))
library(readr)
UA <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
WP_DIR <- file.path(PROJECT_ROOT, "R", "data", "raw_election", "wikipedia_house"); dir.create(WP_DIR, showWarnings = FALSE, recursive = TRUE)

long <- readRDS(file.path(LONG_DIR, "house_long_raw.rds"))     # BEFORE names are applied (written by 02z); never read house_long_all.rds here
STATE_NAME <- c(setNames(state.name, state.abb)); po_of_fips <- c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56)
names(po_of_fips) <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
fips_to_po <- setNames(names(po_of_fips), po_of_fips)

## ---- which candidates need a name --------------------------------------------------------------------------------------------------------------
is_writein <- grepl("^\\[?write[- ]?ins?\\]?\\**$|scatter|^misc|^blank|^void|^over ?votes?|^under ?votes?", long$candidate, ignore.case = TRUE)
ph <- grepl("(Democratic|Republican|Other|Minor) Candidate( [0-9]+)?$|^Unnamed|Other candidates|^(Democrat(ic|s)?|Republicans?)$", long$candidate, ignore.case = TRUE)
agg <- grepl("^(Others?|Unnamed|Other candidates|Misc\\.?)( |$)", long$candidate, ignore.case = TRUE)          # generic aggregates (pooled write-ins), not missing names
sn <- !ph & !is_writein & !grepl(" ", long$candidate)
cand <- long %>% mutate(kind = ifelse(ph, "placeholder", ifelse(sn, "surname", NA))) %>% filter(!is.na(kind)) %>%
  group_by(year, state_fips, district, candidate, party_group, kind) %>% summarise(votes = sum(votes), .groups = "drop") %>%
  mutate(po = fips_to_po[as.character(state_fips)])
message("candidates needing a name: ", nrow(cand), " in ", nrow(distinct(cand, year, state_fips)), " state-years")
need_pages <- cand %>% distinct(year, po) %>% arrange(year, po)

## ---- fetch + parse Wikipedia state pages ---------------------------------------------------------------------------------------------------------------
fetch_page <- function(title) {
  f <- file.path(WP_DIR, paste0(gsub("[^A-Za-z0-9]+", "_", title), ".txt"))
  if (!file.exists(f) || file.size(f) == 0) {
    system2("curl", c("-sL", "-A", shQuote(UA), "-o", shQuote(f), shQuote(paste0("https://en.wikipedia.org/w/index.php?title=", URLencode(gsub(" ", "_", title)), "&action=raw"))))
    Sys.sleep(0.4)
  }
  if (!file.exists(f)) return(character()); readLines(f, warn = FALSE, encoding = "UTF-8")
}
clean_name <- function(x) {
  x <- gsub("\\[\\[(?:[^\\]|]*\\|)?([^\\]]*)\\]\\]", "\\1", x, perl = TRUE)
  x <- gsub("<ref[^>]*>.*?</ref>|<ref[^>]*/>|<br ?/?>|'''|''|\\{\\{[^}]*\\}\\}", " ", x, perl = TRUE)
  x <- gsub("\\((?:incumbent|inc\\.?|w/i|write[- ]?in)[^)]*\\)|\\*+|&nbsp;", " ", x, perl = TRUE, ignore.case = TRUE)
  x <- gsub("\\(([A-Za-z]{2,})\\)", "\"\\1\"", x, perl = TRUE)                      # nickname in parentheses -> "Nickname"
  trimws(gsub("\\s+", " ", x))
}
district_of_heading <- function(h) {
  h <- gsub("\\[\\[(?:[^\\]|]*\\|)?([^\\]]*)\\]\\]", "\\1", h, perl = TRUE)
  m <- regmatches(h, regexpr("[Dd]istrict\\s*([0-9]+)", h)); if (length(m)) return(sprintf("%02d", as.integer(sub(".*?([0-9]+)$", "\\1", m))))
  m <- regmatches(h, regexpr("([0-9]+)(st|nd|rd|th)\\s+(congressional\\s+)?[Dd]istrict", h)); if (length(m)) return(sprintf("%02d", as.integer(sub("^([0-9]+).*", "\\1", m))))
  if (grepl("[Aa]t-large", h)) return("00")
  NA_character_
}
parse_state_page <- function(lines, year, po) {
  if (!length(lines) || grepl("^#REDIRECT", lines[1], ignore.case = TRUE)) return(NULL)
  txt <- paste(lines, collapse = "\n")
  heads <- gregexpr("(?m)^={2,5}[^=\n][^\n]*?={2,5}\\s*$", txt, perl = TRUE)[[1]]
  if (heads[1] == -1) return(NULL)
  hl <- attr(heads, "match.length"); starts <- as.integer(heads); ends <- c(starts[-1] - 1, nchar(txt))
  cur <- NA_character_; out <- list()
  for (i in seq_along(starts)) {
    h <- substr(txt, starts[i], starts[i] + hl[i] - 1); d <- district_of_heading(h)
    if (!is.na(d)) cur <- d else if (grepl("^={2}[^=]", h)) cur <- NA_character_
    body <- substr(txt, starts[i] + hl[i], ends[i])
    if (is.na(cur) || grepl("(?i)primary|runoff|convention", h, perl = TRUE)) next
    boxes <- regmatches(body, gregexpr("(?s)\\{\\{Election box begin.*?\\{\\{Election box end\\}\\}", body, perl = TRUE))[[1]]
    for (b in boxes) {
      title <- sub("(?s).*?title\\s*=\\s*([^|}\n]*).*", "\\1", b, perl = TRUE)
      if (grepl("(?i)primary|runoff|convention|special", title, perl = TRUE)) next
      cs <- regmatches(b, gregexpr("(?s)\\{\\{Election box (?:winning )?candidate[^{}]*?\\}\\}", b, perl = TRUE))[[1]]
      if (!length(cs)) {   # wikitable-row boxes (e.g. Texas 2016-2024): "|- class=vcard" rows with a party cell, a class=fn name cell and a votes cell
        rows <- strsplit(b, "\n\\|-")[[1]]
        for (r1 in rows[grepl("class=\"?fn\"?", rows)]) {
          ln <- strsplit(r1, "\n")[[1]]; ln <- ln[grepl("^\\|", ln)]
          nm_l <- ln[grepl("class=\"?fn\"?", ln)][1]; nm <- clean_name(sub(".*class=\"?fn\"?\\s*\\|", "", nm_l))
          pt_l <- ln[grepl("class=\"?org\"?", ln)][1]; pty <- clean_name(sub("^.*\\|", "", pt_l))
          vt <- ln[grepl("[0-9]{1,3}(,[0-9]{3})+", ln)][1]
          v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", regmatches(vt, regexpr("[0-9]{1,3}(,[0-9]{3})+", vt)))))
          if (!is.na(nm) && nzchar(nm)) out[[length(out) + 1]] <- tibble(year = year, po = po, district = cur, wp_name = nm, wp_party = pty, wp_votes = v, wp_pct = NA_real_)
        }
      }
      for (c1 in cs) {
        ## collapse wiki links to their display text BEFORE splitting fields on "|" ([[Frank Lucas (Oklahoma politician)|Frank Lucas]] -> Frank Lucas)
        c1 <- gsub("\\[\\[[^]|]*\\|([^]]*)\\]\\]", "\\1", c1); c1 <- gsub("\\[\\[([^]|]*)\\]\\]", "\\1", c1)
        g <- function(k) { m <- regmatches(c1, regexpr(paste0("\\|\\s*", k, "\\s*=\\s*[^|\n}]*"), c1)); if (length(m)) trimws(sub("^\\|\\s*[a-z]+\\s*=\\s*", "", m)) else NA_character_ }
        nm <- clean_name(g("candidate")); if (is.na(nm) || !nzchar(nm)) next
        v <- suppressWarnings(as.numeric(gsub("[^0-9]", "", g("votes"))))
        out[[length(out) + 1]] <- tibble(year = year, po = po, district = cur, wp_name = nm, wp_party = clean_name(g("party")), wp_votes = v, wp_pct = NA_real_)
      }
    }
  }
  bind_rows(out)
}
wp_list <- list(); redirects <- character()
for (i in seq_len(nrow(need_pages))) {
  y <- need_pages$year[i]; po <- need_pages$po[i]
  title <- paste0(y, " United States House of Representatives elections in ", STATE_NAME[po])
  lines <- fetch_page(title); p <- parse_state_page(lines, y, po)
  if (is.null(p) || !nrow(p)) redirects <- c(redirects, paste(y, po)) else wp_list[[paste(y, po)]] <- p
}
## ---- national-page pass for state-years whose state page is a redirect / has no usable boxes -------------------------------------------------------------
## The national page has one wikitable per state: a row per district ({{ushr|State|N|X}}) and candidates as bullets "Name (Party) 56.0%" (no vote totals).
parse_national_state <- function(lines, year, po) {
  txt <- paste(lines, collapse = "\n"); nm <- STATE_NAME[po]
  m <- regexpr(paste0("(?m)^==+ *", nm, " *==+\\s*$"), txt, perl = TRUE); if (m[1] == -1) return(NULL)
  rest <- substr(txt, m[1] + attr(m, "match.length"), nchar(txt)); nxt <- regexpr("(?m)^== *[^=]", rest, perl = TRUE)
  sec <- if (nxt[1] > 0) substr(rest, 1, nxt[1] - 1) else rest
  rows <- strsplit(sec, "\n\\|-")[[1]]; out <- list()
  for (r1 in rows) {
    dm <- regmatches(r1, regexpr("(?i)\\{\\{ushr\\|[^|}]*\\|([^|}]*)", r1, perl = TRUE)); if (!length(dm)) next
    dnum <- sub(".*\\|", "", dm)
    d <- if (grepl("^[0-9]+$", dnum)) sprintf("%02d", as.integer(dnum)) else if (grepl("(?i)at.large|AL", dnum, perl = TRUE)) "00" else NA_character_
    bl <- strsplit(r1, "\n")[[1]]; bl <- bl[grepl("^\\*", bl)]
    for (b1 in bl) {
      b2 <- gsub("\\{\\{[^}]*\\}\\}", "", b1)
      b2 <- gsub("\\[\\[(?:[^]|]*\\|)?([^]]*)\\]\\]", "\\1", b2, perl = TRUE)
      b2 <- gsub("'{2,3}|<[^>]*>", "", b2)
      mm <- regmatches(b2, regexec("^\\*\\s*(.*?)\\s*\\(([^)]*)\\)\\s*([0-9.]+)\\s*%", b2))[[1]]
      if (length(mm) == 4) out[[length(out) + 1]] <- tibble(year = year, po = po, district = d, wp_name = clean_name(mm[2]), wp_party = mm[3], wp_votes = NA_real_, wp_pct = as.numeric(mm[4]))
    }
  }
  bind_rows(out)
}
nat_list <- list()
for (r in redirects) {
  y <- as.integer(sub(" .*", "", r)); po <- sub(".* ", "", r)
  lines <- fetch_page(paste0(y, " United States House of Representatives elections")); pn <- parse_national_state(lines, y, po)
  if (!is.null(pn) && nrow(pn)) nat_list[[r]] <- pn
}
message("national-page candidates parsed: ", sum(vapply(nat_list, nrow, 1L)), " from ", length(nat_list), " of ", length(redirects), " state-years")
wp <- bind_rows(c(wp_list, nat_list))
message("Wikipedia candidates parsed: ", nrow(wp), " from ", length(wp_list), " state-year pages; pages with no usable district boxes (redirect or other format): ", length(redirects))
if (length(redirects)) message("  ", paste(redirects, collapse = ", "))
saveRDS(wp, file.path(OUTPUT_DIR, "wikipedia_house_candidates.rds"))

## ---- matching -------------------------------------------------------------------------------------------------------------------------------------------------
wp_group <- function(p) ifelse(grepl("Democratic", p), "DEM", ifelse(grepl("Republican", p), "REP", "OTHER"))
last_tok <- function(x) { x <- iconv(x, to = "ASCII//TRANSLIT"); t <- strsplit(toupper(gsub("[^A-Za-z' -]", " ", x)), "[ ]+")[[1]]; t <- t[!t %in% c("JR", "SR", "II", "III", "IV", "")]; if (length(t)) tail(t, 1) else "" }
dv_tab <- long %>% group_by(year, state_fips, district) %>% summarise(dv = sum(votes), .groups = "drop")
res <- list()
for (i in seq_len(nrow(cand))) {
  c1 <- cand[i, ]; w <- wp %>% filter(year == c1$year, po == c1$po, district == c1$district) %>% mutate(grp = wp_group(wp_party))
  if (is.na(c1$district) || !nrow(w)) { res[[i]] <- c1 %>% mutate(new = NA_character_, method = "no_wikipedia_district", evidence = NA_character_); next }
  new <- NA_character_; method <- "unmatched"; ev <- NA_character_
  m1 <- w %>% filter(!is.na(wp_votes), wp_votes == c1$votes)
  if (nrow(m1) == 1 && (c1$kind == "placeholder" || last_tok(m1$wp_name) == toupper(c1$candidate) || TRUE)) { new <- m1$wp_name; method <- "vote_total"; ev <- paste0("wikipedia votes ", m1$wp_votes) }
  if (is.na(new) && c1$kind == "surname") {
    m2s <- w %>% filter(vapply(wp_name, last_tok, "") == toupper(gsub("[^A-Za-z' -]", "", c1$candidate)))
    m2 <- m2s %>% filter(grp == c1$party_group | (c1$party_group == "OTHER" & grp == "OTHER"))
    if (nrow(m2) == 1) { new <- m2$wp_name; method <- "surname"; ev <- paste0("wikipedia party ", m2$wp_party) }
    ## the surname is unique in the district: a cross-endorsement line (e.g. a Democrat's Working Families line) carries a different party group, so accept it
    else if (nrow(m2s) == 1) { new <- m2s$wp_name; method <- "surname_unique"; ev <- paste0("only candidate with this surname in the district; wikipedia party ", m2s$wp_party) }
  }
  ## share rule (national-page candidates have a vote share but no vote total): unique candidate, compatible party, share within 0.6 pp
  if (is.na(new) && "wp_pct" %in% names(w) && any(!is.na(w$wp_pct))) {
    dv <- dv_tab$dv[dv_tab$year == c1$year & dv_tab$state_fips == c1$state_fips & dv_tab$district == c1$district][1]; sh <- 100 * c1$votes / dv
    m4 <- w %>% filter(!is.na(wp_pct), abs(wp_pct - sh) <= 0.6, grp == c1$party_group)
    if (c1$kind == "surname") m4 <- m4 %>% filter(vapply(wp_name, last_tok, "") == toupper(gsub("[^A-Za-z' -]", "", c1$candidate)))
    if (nrow(m4) == 1) { new <- m4$wp_name; method <- "vote_share"; ev <- paste0("wikipedia share ", m4$wp_pct, "% vs ours ", round(sh, 1), "%") }
  }
  ## pooled minor-party column: our OTHER placeholder equals the sum of 2-3 minor candidates on Wikipedia
  if (is.na(new) && c1$kind == "placeholder" && c1$party_group == "OTHER") {
    mo <- w %>% filter(grp == "OTHER", !is.na(wp_votes))
    if (nrow(mo) >= 2) for (k in 2:min(3, nrow(mo))) {
      hit <- Filter(function(ix) sum(mo$wp_votes[ix]) == c1$votes, combn(nrow(mo), k, simplify = FALSE))
      if (length(hit) == 1) { new <- paste(mo$wp_name[hit[[1]]], collapse = " / "); method <- "pooled_minor_sum"; ev <- paste0("sum of ", k, " wikipedia candidates = ", c1$votes); break }
    }
  }
  if (is.na(new) && c1$kind == "placeholder" && c1$party_group %in% c("DEM", "REP")) {
    m3 <- w %>% filter(grp == c1$party_group)
    if (nrow(m3) == 1 && !is.na(m3$wp_votes) && abs(m3$wp_votes - c1$votes) / max(m3$wp_votes, 1) <= 0.10) { new <- m3$wp_name; method <- "party_D_R"; ev <- paste0("wikipedia votes ", m3$wp_votes, " vs ours ", c1$votes) }
  }
  res[[i]] <- c1 %>% mutate(new = new, method = method, evidence = ev)
}
rep <- bind_rows(res)

## ---- pass 2: extra rules for the candidates still unnamed ---------------------------------------------------------------------------------------------
PARTY_WORD <- "^(Democrat(ic|s)?|Republicans?)$"
norm_ascii <- function(x) toupper(gsub("[^A-Za-z' -]", "", iconv(x, to = "ASCII//TRANSLIT")))
page_text <- function(y, po) paste(fetch_page(paste0(y, " United States House of Representatives elections in ", STATE_NAME[po])), collapse = "\n")
pt_cache <- list()
for (i in which(is.na(rep$new))) {
  c1 <- rep[i, ]; new <- NA_character_; method <- c1$method; ev <- c1$evidence
  wy <- wp %>% filter(year == c1$year, po == c1$po) %>% mutate(grp = wp_group(wp_party))
  wd <- if (!is.na(c1$district)) wy %>% filter(district == c1$district) else wy[0, ]
  nm_up <- toupper(gsub("\\s+", " ", c1$candidate))
  ## R1: generic aggregate (pooled write-ins / unnamed): not a missing name, give it the standard label
  if (grepl("^(Others?|Unnamed|Other candidates|Misc\\.?)( |$)", c1$candidate, ignore.case = TRUE)) { new <- "Other / write-in candidates"; method <- "aggregate_label"; ev <- "generic pooled or unnamed row" }
  ## R2: same name, different case / spelling of case only (Rob bishop -> Rob Bishop): unique in the district, or in the state-year if the district is unknown
  if (is.na(new) && grepl(" ", c1$candidate)) { pool <- if (nrow(wd)) wd else wy; m <- pool %>% filter(toupper(wp_name) == nm_up)
    if (length(unique(m$wp_name)) == 1) { new <- m$wp_name[1]; method <- "exact_case_insensitive"; ev <- "same name, different capitalisation" } }
  ## R3: fuzzy surname in the district: ours is a truncated prefix (>= 5 letters) of the Wikipedia surname, or one letter off (Aderhold/Aderholt)
  if (is.na(new) && c1$kind == "surname" && nrow(wd)) {
    sur <- norm_ascii(c1$candidate); lt <- vapply(wd$wp_name, last_tok, "")
    ok <- (nchar(sur) >= 5 & startsWith(lt, sur)) | (nchar(sur) >= 5 & drop(utils::adist(sur, lt)) <= 1)
    m <- wd[ok & (wd$grp == c1$party_group | c1$party_group == "OTHER" | wd$grp == "OTHER"), ]
    if (length(unique(m$wp_name)) == 1) { new <- m$wp_name[1]; method <- "surname_fuzzy"; ev <- paste0("close surname match; wikipedia party ", m$wp_party[1]) } }
  ## R4: no usable district (district unknown or no Wikipedia box): unique surname across the whole state-year
  if (is.na(new) && c1$kind == "surname" && !nrow(wd) && nrow(wy)) {
    m <- wy[vapply(wy$wp_name, last_tok, "") == norm_ascii(c1$candidate), ]
    if (length(unique(m$wp_name)) == 1) { new <- m$wp_name[1]; method <- "surname_statewide"; ev <- "only candidate with this surname in the state-year" } }
  ## R5: party-word row ("Democrats"/"Republican") = the district's single Wikipedia candidate of that party (a fragment can be at most the whole total)
  if (is.na(new) && grepl(PARTY_WORD, c1$candidate, ignore.case = TRUE) && c1$party_group %in% c("DEM", "REP") && nrow(wd)) {
    m <- wd %>% filter(grp == c1$party_group)
    if (nrow(m) == 1 && (is.na(m$wp_votes) || c1$votes <= 1.1 * m$wp_votes)) { new <- m$wp_name; method <- "party_word"; ev <- paste0("only ", c1$party_group, " candidate on wikipedia for the district") } }
  ## R6: unopposed / no district box: the surname appears with ONE full name in the state page text (>= 2 times)
  if (is.na(new) && c1$kind == "surname") {
    key <- paste(c1$year, c1$po); if (is.null(pt_cache[[key]])) pt_cache[[key]] <- page_text(c1$year, c1$po)
    t <- gsub("\\[\\[[^]|]*\\|([^]]*)\\]\\]", "\\1", pt_cache[[key]]); t <- gsub("\\[\\[([^]]*)\\]\\]", "\\1", t)
    sur <- gsub("[^A-Za-z' -]", "", c1$candidate)
    hits <- regmatches(t, gregexpr(paste0("\\b(?:[A-Z][a-z]+\\.?\\s(?:[A-Z]\\.?\\s)?)", sur, "\\b"), t, perl = TRUE))[[1]]
    tb <- sort(table(trimws(hits)), decreasing = TRUE)
    if (length(tb) >= 1 && tb[1] >= 2 && (length(tb) == 1 || tb[2] * 3 <= tb[1])) { new <- names(tb)[1]; method <- "page_text"; ev <- paste0("'", names(tb)[1], "' appears ", tb[1], " times on the page") } }
  rep$new[i] <- new; rep$method[i] <- method; rep$evidence[i] <- ev
}
message("pass 2 named ", sum(!is.na(rep$new)) - sum(!is.na(bind_rows(res)$new)), " more candidates")
write_csv(rep %>% select(year, state_fips, po, district, kind, candidate, party_group, votes, new, method, evidence), file.path(OUTPUT_DIR, "candidate_name_matching_report.csv"))
## cross-check of OUR district totals against Wikipedia's votes for candidates matched by surname: differences > 0.5% are worth a look
vd <- rep %>% filter(method == "surname") %>% inner_join(wp %>% select(year, po, district, wp_name, wp_votes), by = c("year", "po", "district", "new" = "wp_name")) %>%
  filter(!is.na(wp_votes)) %>% mutate(diff = votes - wp_votes, rel = round(abs(diff) / wp_votes, 4)) %>% filter(rel > 0.005) %>% arrange(desc(rel))
write_csv(vd %>% select(year, po, district, new, ours = votes, wikipedia = wp_votes, diff, rel), file.path(OUTPUT_DIR, "wikipedia_vote_differences.csv"))
message("candidates matched by surname whose vote total differs from Wikipedia by > 0.5%: ", nrow(vd))
ov <- rep %>% filter(!is.na(new), new != candidate, !grepl("^write[- ]?ins?$", new, ignore.case = TRUE)) %>%   # never rename a named row to a generic write-in total
   transmute(year, state_fips, district, candidate_old = candidate, candidate_new = new, method, evidence)
## the same old name can appear on several party lines (each matched separately): keep one override if they all agree, drop the name if they disagree (ambiguous)
amb <- ov %>% group_by(year, state_fips, district, candidate_old) %>% filter(n_distinct(candidate_new) > 1) %>% ungroup()
if (nrow(amb)) message("ambiguous names left unrenamed (party lines matched to different people): ", nrow(distinct(amb, year, state_fips, district, candidate_old)))
ov <- ov %>% anti_join(amb, by = c("year", "state_fips", "district", "candidate_old")) %>% distinct(year, state_fips, district, candidate_old, .keep_all = TRUE)
stopifnot(!anyDuplicated(ov[, c("year", "state_fips", "district", "candidate_old")]))
write_csv(ov, file.path(PROJECT_ROOT, "R", "data", "raw_election", "candidate_name_overrides.csv"))
message("\n== outcome by method =="); print(as.data.frame(rep %>% count(kind, method) %>% arrange(kind, desc(n))))
message("\n== share of flagged candidates named, weighted by district vote share >= 1% ==")
tot <- long %>% group_by(year, state_fips, district) %>% summarise(dv = sum(votes), .groups = "drop")
imp <- rep %>% left_join(tot, by = c("year", "state_fips", "district")) %>% filter(votes / dv >= 0.01)
message("candidates with >= 1% of the district vote: ", nrow(imp), "; named: ", sum(!is.na(imp$new)), " (", round(100 * mean(!is.na(imp$new)), 1), "%)")
