## Helpers for the candidate-level LONG table (the project's source of truth from 2026-09-20; see docs/DECISIONS.md).
##
## One row per year x county x district x candidate x party line. Columns (LONG_COLS):
##   year, office ("house"), state_fips, county_fips (integer, e.g. 1001; zero-pad to 5 characters in the CSV release),
##   district (character, 2-digit; "00" = at-large / statewide; NA only when a source could not give it), stage ("general" unless a
##   source says otherwise), candidate (canonical spelling), party (party label as reported for that line), party_group ("DEM",
##   "REP" or "OTHER" -- the SAME grouping the shares panel used, so shares derive exactly), votes, source (build name).
## The shares panel is derived from the long table: totalvote = sum(votes); demovote / repuvote = DEM / REP votes over totalvote.
## Every build must pass check_long_vs_source(): its derived shares must equal the build's own shares file exactly.

OFFICE_SAMPLE <- c(house = "HE", senate = "SE", president = "PE")           # shares-panel sample code per office (generalized 2026-09-21 for President and Senate)
LONG_COLS <- c("year", "office", "state_fips", "county_fips", "district", "stage", "candidate", "party", "party_group", "votes", "source")

## Convert a district value ("001", 1, "1.0", "STATEWIDE", NA) to the 2-digit character convention.
norm_district <- function(x) {
  x <- toupper(trimws(as.character(x)))
  n <- suppressWarnings(as.numeric(x))
  out <- ifelse(!is.na(n), sprintf("%02d", as.integer(n)), ifelse(x %in% c("STATEWIDE", "AT LARGE", "AT-LARGE", "AL"), "00", NA_character_))
  ## Added (backward compatible: only touches values that were NA above): ordinals such as "1st", "10th", "2ND" (Massachusetts).
  ord <- suppressWarnings(as.integer(sub("^([0-9]+)(ST|ND|RD|TH)$", "\\1", x)))
  ord[!grepl("^[0-9]+(ST|ND|RD|TH)$", x)] <- NA_integer_
  ifelse(is.na(out) & !is.na(ord), sprintf("%02d", ord), out)
}

## Title-case an upper-case candidate name (Mc/Mac/O'/hyphen aware; suffixes keep their form).
pretty_name <- function(x) {
  x <- tolower(trimws(gsub("\\s+", " ", x)))
  x <- gsub("(^|[ '\\-\\.\"])([a-z])", "\\1\\U\\2", x, perl = TRUE)
  x <- gsub("\\bMc([a-z])", "Mc\\U\\1", x, perl = TRUE)
  x <- gsub("\\b(Ii|Iii|Iv)\\b", "\\U\\1", x, perl = TRUE)
  x
}

## Validate / coerce a data frame to the long schema. `df` must already contain the LONG_COLS except office/stage/source.
make_long <- function(df, source, stage = "general", office = "house") {
  df$office <- office; if (!"stage" %in% names(df)) df$stage <- stage; df$source <- source
  missing <- setdiff(LONG_COLS, names(df)); if (length(missing)) stop("missing long columns: ", paste(missing, collapse = ", "))
  df <- df[, LONG_COLS]
  df$year <- as.integer(df$year); df$county_fips <- as.integer(df$county_fips)
  df$state_fips <- as.integer(df$state_fips); df$votes <- as.numeric(df$votes)
  stopifnot(!anyNA(df$year), !anyNA(df$county_fips), !anyNA(df$votes), all(df$votes >= 0),
            all(df$party_group %in% c("DEM", "REP", "OTHER")), all(df$state_fips == df$county_fips %/% 1000),
            !anyNA(df$candidate), all(nzchar(df$candidate)))
  df
}

## Shares derived from a long table (same shape as the panel: year, cty_fips, sample, demovote, repuvote, totalvote).
derive_shares <- function(long) {
  smp <- if ("office" %in% names(long) && length(unique(long$office)) == 1) unname(OFFICE_SAMPLE[unique(long$office)]) else "HE"
  long %>% dplyr::group_by(year, cty_fips = county_fips) %>%
    dplyr::summarise(totalvote = sum(votes), dem = sum(votes[party_group == "DEM"]), rep = sum(votes[party_group == "REP"]), .groups = "drop") %>%
    dplyr::filter(totalvote > 0) %>%
    dplyr::transmute(year, cty_fips, sample = smp, demovote = dem / totalvote, repuvote = rep / totalvote, totalvote)
}

## The acceptance test for a build: derived shares must equal the build's own shares file (elect_he_cty_<x>.rds) row for row.
## Returns a one-row summary; prints mismatches. `years` restricts the comparison (default: all years in the long table).
check_long_vs_source <- function(long, source_rds, years = NULL, tol_share = 1e-9, tol_total = 0.5) {
  d <- derive_shares(long); src <- readRDS(source_rds) %>% dplyr::filter(sample == unique(d$sample))
  if (is.null(years)) years <- sort(unique(d$year))
  d <- d %>% dplyr::filter(year %in% years); src <- src %>% dplyr::filter(year %in% years)
  j <- dplyr::full_join(d, src, by = c("year", "cty_fips", "sample"), suffix = c(".long", ".src"))
  only_long <- j %>% dplyr::filter(is.na(totalvote.src)); only_src <- j %>% dplyr::filter(is.na(totalvote.long))
  both <- j %>% dplyr::filter(!is.na(totalvote.src), !is.na(totalvote.long))
  bad <- both %>% dplyr::filter(!(abs(demovote.long - demovote.src) < tol_share & abs(repuvote.long - repuvote.src) < tol_share &
                                    abs(totalvote.long - totalvote.src) < tol_total))
  res <- data.frame(source = basename(source_rds), years = paste(range(years), collapse = "-"), keys_source = nrow(src), keys_long = nrow(d),
                    matched = nrow(both) - nrow(bad), mismatched = nrow(bad), only_in_long = nrow(only_long), only_in_source = nrow(only_src))
  res$pass <- res$mismatched == 0 & res$only_in_long == 0 & res$only_in_source == 0
  if (!res$pass) { message("CHECK FAILED for ", basename(source_rds)); if (nrow(bad)) print(as.data.frame(head(bad, 8)))
    if (nrow(only_long)) message("  keys only in long: ", nrow(only_long)); if (nrow(only_src)) message("  keys only in source: ", nrow(only_src)) }
  else message("CHECK PASSED: ", basename(source_rds), " (", res$matched, " keys identical)")
  invisible(res)
}

LONG_DIR <- file.path(PROJECT_ROOT, "R", "output", "long")
save_long <- function(long, name) { dir.create(LONG_DIR, showWarnings = FALSE, recursive = TRUE); saveRDS(long, file.path(LONG_DIR, paste0(name, ".rds"))); invisible(long) }

## ---- display standardization shared by every build ------------------------------------------------------------------------------------------
pretty_party <- function(x) { x <- tolower(trimws(x)); gsub("(^|[ /\\-])([a-z])", "\\1\\U\\2", x, perl = TRUE) }

## Normalize a party label for display. Blank/NA stays NA (finalize_long fills it). party_group is NOT derived from this; it comes from the
## build's own logic so that shares reproduce exactly.
standard_party_label <- function(x) {
  y <- tolower(trimws(as.character(x))); y[y %in% c("", "na", "n/a")] <- NA_character_
  dplyr::case_when(
    is.na(y) ~ NA_character_,
    grepl("^(democrat(ic)?|dem|d)$", y) ~ "Democratic",
    grepl("^(republican|rep|r|gop)$", y) ~ "Republican",
    grepl("^(libertarian|lib|lbt|l)$", y) ~ "Libertarian",
    grepl("^(independent|ind|i)$", y) ~ "Independent",
    grepl("^(green|grn)$", y) ~ "Green",
    grepl("^(nonpartisan|nopty|no party|np|npa)$", y) ~ "Nonpartisan",
    TRUE ~ pretty_party(x))
}

## Turn a build's candidate-level rows into the standard long table.
## `df` needs: year, county_fips, candidate (raw), party_group ("DEM"/"REP"/"OTHER"), votes; optional: district, party (raw label), stage.
## - district -> 2-digit convention (NA if the source has none); candidate -> one canonical, title-cased spelling per year+state+district+
##   letters-only key (the most-voted raw spelling); party label -> standardized; a blank label takes the candidate's most-voted label,
##   else the party group's name. Rows are summed over identical (county, district, candidate, party, group) keys.
## Non-vote rows that some sources print as if they were candidates: blank / void / over- / under-votes, New York's "Blank, Void & Scattering" and its "Bvs Subtotal", "Times Blank Voted", rejected or invalid write-ins.
## They are not votes for anybody, so they are never part of a candidate table or of a county total (docs/DECISIONS.md; the FEC totals exclude them). \b keeps names such as "Blankenship" safe.
PSEUDO_NAME_RE <- "^(bvs\\b|blanks?\\b|voids?\\b|(over|under) ?votes?\\b|times blank voted|rejected\\b|write-in: invalid|invalid write)"
finalize_long <- function(df, source, stage = "general", office = "house") {
  df <- df[!grepl(PSEUDO_NAME_RE, trimws(df$candidate), ignore.case = TRUE), ]
  if (!"district" %in% names(df)) df$district <- NA
  if (!"party" %in% names(df)) df$party <- NA_character_
  df <- df %>% dplyr::mutate(county_fips = as.integer(county_fips), state_fips = county_fips %/% 1000L, district = norm_district(district),
                             candidate = ifelse(is.na(candidate) | !nzchar(trimws(candidate)), "Unnamed (name missing in source)", trimws(candidate)),
                             ckey = gsub("[^A-Z]", "", toupper(candidate)), lab = standard_party_label(party))
  canon <- df %>% dplyr::group_by(year, state_fips, district, ckey, candidate) %>% dplyr::summarise(v = sum(votes), .groups = "drop") %>%
    dplyr::group_by(year, state_fips, district, ckey) %>% dplyr::slice_max(v, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
    dplyr::transmute(year, state_fips, district, ckey, cand_canon = pretty_name(candidate))
  prim <- df %>% dplyr::filter(!is.na(lab)) %>% dplyr::group_by(year, state_fips, district, ckey, lab) %>% dplyr::summarise(v = sum(votes), .groups = "drop") %>%
    dplyr::group_by(year, state_fips, district, ckey) %>% dplyr::slice_max(v, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
    dplyr::transmute(year, state_fips, district, ckey, lab_primary = lab)
  df %>% dplyr::left_join(canon, by = c("year", "state_fips", "district", "ckey")) %>% dplyr::left_join(prim, by = c("year", "state_fips", "district", "ckey")) %>%
    dplyr::mutate(party = dplyr::coalesce(lab, lab_primary, dplyr::case_when(party_group == "DEM" ~ "Democratic", party_group == "REP" ~ "Republican", TRUE ~ "Other")),
                  candidate = cand_canon) %>%
    dplyr::group_by(year, state_fips, county_fips, district, candidate, party, party_group, stage = if ("stage" %in% names(df)) stage else stage) %>%
    dplyr::summarise(votes = sum(votes), .groups = "drop") %>% dplyr::arrange(year, county_fips, district, dplyr::desc(votes)) %>%
    make_long(source, stage = stage, office = office)
}
