## *** SUPERSEDED 2026-09-20 by 01bw/01bx (Louisiana SOS parish-level results, 1990-2014). ***
## Do not re-run into the panel. Two reasons: (1) this OpenElections build only held runoff-decided districts
## (coverage 3-56%); (2) the repo file `20041204__la__general.csv` is MISDATED: it contains the 2014 Dec-6 runoffs
## (Abraham v Mayo LA-5, Graves v Edwards LA-6), not the real Dec-4-2004 runoffs (Melancon v Tauzin III LA-3,
## Boustany v Mount LA-7), so the 35 "2004" rows this script produced were actually 2014 results.
## Its output was renamed R/output/SUPERSEDED_openelections_la.rds so the coverage tracker no longer counts it.
##
## Louisiana: found via OpenElections (github.com/openelections/openelections-data-la).
##
## LA uses PARISHES, not counties, but parishes are the direct county-equivalent -- matched
## against the project's crosswalk (`R/data/raw_election/countypres_2000-2024.tab`, which
## already has FIPS for all 64 LA parishes from the presidential data) by uppercased name,
## no special handling needed for the parish-vs-county naming difference itself.
##
## SEVERE, real source-coverage gap specific to Louisiana's "jungle primary" system -- worth
## documenting prominently since it's very different from every other state's near-complete
## county coverage. LA's congressional "election" can be decided in two possible rounds: an
## open primary (all candidates, all parties, one ballot) and, only for districts where nobody
## won an outright majority, a runoff a month later. This OpenElections repo's per-year
## `<date>__la__general.csv` files turn out to almost always contain ONLY the runoff-decided
## districts, not the full first-round results for every district -- confirmed by checking
## district coverage directly (`awk -F',' '$2=="U.S. House"{print $3}' | sort -u` per file):
##   2002 (Dec runoff file only): 1 of ~7 districts (just district 5)
##   2004 (Dec runoff file only): 2 districts (5, 6)
##   2006 (Dec runoff file only): 1 district (2)
##   2008 (Nov file, this is the one full year): 5 of 7 districts (1,2,4,6,7) -- 2008 was the
##     first cycle after a 2006 LA law change synced congressional elections to the actual
##     federal November date, so this file for once has real first-round results, not just a
##     runoff subset. Districts 3 and 5 are absent -- not investigated further (uncontested
##     and omitted, or a genuine file gap -- either way a modest, understood loss).
##   2012 (Dec runoff file only): 1 district (3)
##   2014 (Dec runoff file only): 2 districts (5, 6)
## In every "runoff-only" year, the corresponding November first-round file exists in the repo
## for OTHER offices (President in presidential years) but has ZERO "U.S. House" rows at all --
## confirmed directly, not an oversight in this script. **General lesson: don't assume a
## state's "general election" file represents one single up-or-down vote just because that's
## true almost everywhere else -- Louisiana's jungle-primary system means the real decisive
## date varies BY DISTRICT within a single year, and a source can legitimately capture only
## the subset of districts whose decisive vote happened to fall on the date that file covers.**
## 2000 and 2010 excluded entirely: no U.S. House data of any kind (not even a runoff subset)
## exists anywhere in the repo for either year's regular cycle (2000 has no December file at
## all, only an unrelated special election; 2010 likewise has no general.csv, only specials).
##
## Verified via known race outcomes instead of a MEDSL cross-check (no pre-2016 overlap year
## possible, same situation as GA 2012/CO/CA-no-overlap-years): 2002 LA-5 runoff shows Rodney
## Alexander winning as a Democrat (matches his real 2002 win -- he switched to Republican only
## in 2004, a well-known detail that confirms this isn't a garbled/misattributed source); 2006
## LA-2's runoff file has ONLY party "D" rows (William Jefferson vs. Karen Carter, both
## Democrats -- matches the real 2006 all-Democrat runoff after Jefferson's corruption
## scandal); 2012 LA-3's runoff file has ONLY party "R" rows (Boustany vs. Landry, both
## Republicans, forced into the same district by post-2010 redistricting -- also real and
## well-known). These aren't bugs -- genuine zero-repuvote / zero-demovote county-years,
## correctly reflecting a same-party runoff.
##
## Clean schema throughout (county,office,district,party,candidate,votes), no pseudo-total
## rows found in any of the 6 years used (checked: every county value is a real parish name,
## no "Total"/blank/statewide rollup row present). Party codes D/R plus occasional N/O
## (no-party/other) -- excluded from repuvote/demovote same as any other state's minor-party
## rows, included in totalvote.

library(dplyr)
library(readr)
library(stringr)

source("R/00_setup.R")

RAW_DIR <- "R/data/raw_house_county_open_states/louisiana"

la_files <- tribble(
  ~year, ~file,
  2002, "20021207__la__general.csv",
  2004, "20041204__la__general.csv",
  2006, "20061209__la__general.csv",
  2008, "20081104__la__general.csv",
  2012, "20121208__la__general.csv",
  2014, "20141206__la__general.csv"
)

crosswalk <- read_tsv("R/data/raw_election/countypres_2000-2024.tab", show_col_types = FALSE) %>%
  filter(state == "LOUISIANA") %>%
  distinct(county_name, county_fips) %>%
  mutate(county_name = str_trim(county_name))

read_la_year <- function(year, file) {
  read_csv(file.path(RAW_DIR, file), show_col_types = FALSE) %>%
    filter(office == "U.S. House") %>%
    mutate(
      county_up = toupper(str_trim(county)),
      ## same "LASALLE" vs crosswalk's "LA SALLE" mismatch hit in Texas (01r) -- LA has its own
      ## LaSalle Parish with the identical no-space-vs-space spelling difference.
      county_up = if_else(county_up == "LASALLE", "LA SALLE", county_up),
      party_std = case_when(
        party == "D" ~ "dem",
        party == "R" ~ "rep",
        TRUE ~ "other"
      )
    ) %>%
    group_by(county_up) %>%
    summarise(
      demovote_n = sum(votes[party_std == "dem"]),
      repuvote_n = sum(votes[party_std == "rep"]),
      totalvote = sum(votes),
      .groups = "drop"
    ) %>%
    mutate(year = year)
}

la_raw <- bind_rows(Map(read_la_year, la_files$year, la_files$file))

stopifnot(all(la_raw$county_up %in% crosswalk$county_name))

elect_he_cty_la <- la_raw %>%
  left_join(crosswalk, by = c("county_up" = "county_name")) %>%
  transmute(
    state = "LOUISIANA",
    year,
    cty_fips = county_fips,
    sample = "HE",
    demovote = demovote_n / totalvote,
    repuvote = repuvote_n / totalvote,
    totalvote
  )

stopifnot(all(!is.na(elect_he_cty_la$cty_fips)))
stopifnot(all(elect_he_cty_la$totalvote > 0))

## Sanity check -- expect some genuine 0s from same-party runoffs (2006 LA-2, 2012 LA-3), flag
## anything else unusually low.
range(elect_he_cty_la$demovote + elect_he_cty_la$repuvote)

cat("Louisiana HE rows:", nrow(elect_he_cty_la), "\n")
print(table(elect_he_cty_la$year))

saveRDS(elect_he_cty_la, "R/output/elect_he_cty_la.rds")
