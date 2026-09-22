## Shared prelude for the West-batch long-table scripts (02w_house_long_*.R). Sources setup + helpers, disables save_step so no shares
## file / panel is ever written, and provides a crosswalk helper.
source(file.path("R", "00_setup.R"))
library(readr)
source(file.path("R", "long_helpers.R"))
save_step <- function(df, name) invisible(df)          # SAFETY: nothing but save_long() may write
RAW_ROOT <- file.path(PROJECT_ROOT, "R", "data", "raw_house_county_open_states")
xw_all <- read_delim(file.path(PROJECT_ROOT, "R", "data", "raw_election", "countypres_2000-2024.tab"), delim = "\t", show_col_types = FALSE) %>%
  distinct(state, county_name, county_fips) %>% mutate(county_name = toupper(trimws(county_name)))
fips_of <- function(state) xw_all %>% filter(state == !!state) %>% select(county_name, county_fips)
SRC <- function(x) file.path(PROJECT_ROOT, "R", "output", paste0("elect_he_cty_", x, ".rds"))
