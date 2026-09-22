## Port of Files/Dofiles/Data_creation/4th_County_state_codes.do
##
## Builds a county (fips_tc) -> state crosswalk from the Census cartographic county
## shapefile's attribute table. The source do-file uses Stata's `shp2dta` to convert the
## whole shapefile (geometry + attributes); we only need the attribute table, so this reads
## the .dbf directly instead of round-tripping through the .shp/.shx geometry.

source(file.path("R", "00_setup.R"))

county_attrs <- foreign::read.dbf(file.path(INPUT_DIR, "cb_2015_us_county_5m", "cb_2015_us_county_5m.dbf"),
                                   as.is = TRUE)

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

county_state <- county_attrs %>%
  mutate(fips_tc = as.integer(paste0(STATEFP, COUNTYFP))) %>%
  filter(STATEFP != "15", STATEFP != "02", fips_tc <= 60000) %>%
  mutate(
    statefip = as.integer(STATEFP),
    state_name = unname(state_names[as.character(statefip)])
  ) %>%
  select(state_name, statefip, fips_tc) %>%
  save_step("County_state")
