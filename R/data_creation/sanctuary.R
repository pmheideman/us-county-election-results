## Port of Files/Dofiles/Data_creation/Sanctuary.do
##
## Unlike the other Data_creation files, the source Sanctuary.do has no `use`/`clear` header --
## it's a fragment meant to be run against whatever county/CZ-level dataset is already loaded in
## 8th_Election_census_combine.do (which must already have `state`, `cty_fips`, `czone`
## columns). Ported here as a function of the same shape, to be called from
## 08_election_census_combine.R once that file exists.
##
## Source: Wikipedia (2019), "Sanctuary city" -- a hand-coded list of pre-2010 sanctuary
## counties/commuting zones (cities annotated "from 2016/2017 onwards" in the source comments
## are deliberately left un-flagged, since the paper's sample predates those policies).

add_sanctuary_flags <- function(df) {
  state_names <- c(
    `1` = "Alabama", `2` = "Alaska", `4` = "Arizona", `5` = "Arkansas", `6` = "California",
    `8` = "Colorado", `9` = "Connecticut", `10` = "Delaware", `11` = "District of Columbia",
    `12` = "Florida", `13` = "Georgia", `15` = "Hawaii", `16` = "Idaho", `17` = "Illinois",
    `18` = "Indiana", `19` = "Iowa", `20` = "Kansas", `21` = "Kentucky", `22` = "Louisiana",
    `23` = "Maine", `24` = "Maryland", `25` = "Massachusetts", `26` = "Michigan",
    `27` = "Minnesota", `28` = "Mississippi", `29` = "Missouri", `30` = "Montana",
    `31` = "Nebraska", `32` = "Nevada", `33` = "New Hampshire", `34` = "New Jersey",
    `35` = "New Mexico", `36` = "New York", `37` = "North Carolina", `38` = "North Dakota",
    `39` = "Ohio", `40` = "Oklahoma", `41` = "Oregon", `42` = "Pennsylvania",
    `44` = "Rhode Island", `45` = "South Carolina", `46` = "South Dakota", `47` = "Tennessee",
    `48` = "Texas", `49` = "Utah", `50` = "Vermont", `51` = "Virginia", `53` = "Washington",
    `54` = "West Virginia", `55` = "Wisconsin", `56` = "Wyoming"
  )

  sanctuary_cty <- c(
    4019, 9003, 17031, 17019, 26163, 26161, 27053,
    34017, 34013, 34031, 34023,       # NJ (34017/34013 each listed twice in the source; harmless)
    36001, 36109, 36005, 36047, 36061, 36081, 36085, 36071, 36055, 36067,
    42015, 42017, 42029, 42031, 42045, 42049, 42055, 42075, 42077, 42081,
    42091, 42093, 42099, 42101, 42103, 42129,
    53033
  )
  sanctuary_czone <- c(
    35100, 20901, 24300, 23500, 11600, 21501, 19600,
    18600, 18100, 19400, 19300, 18000, 17700,
    18900, 19700, 16500, 17400, 19100, 19000, 18700, 19200, 18800, 16300,
    39400
  )

  df %>%
    mutate(
      statefip = as.integer(state),
      state_name = unname(state_names[as.character(statefip)]),
      san_cty = as.integer(cty_fips %in% sanctuary_cty | state_name %in% c("California", "Maine", "Oregon")),
      san_cz   = as.integer(czone %in% sanctuary_czone | state_name %in% c("California", "Maine", "Oregon"))
    )
}
