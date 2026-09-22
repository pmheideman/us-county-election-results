# Paths and packages shared by every script in R/.
# Source this at the top of each data_creation / regressions / figures script.

library(haven)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(purrr)
library(tibble)

PROJECT_ROOT <- "/home/paul/Stats/substack_projects/immigration"
INPUT_DIR    <- file.path(PROJECT_ROOT, "119372-V1/Files/Data/Input")   # original replication inputs (read-only)
IPUMS_DIR    <- file.path(INPUT_DIR, "IPUMS")
OUTPUT_DIR   <- file.path(PROJECT_ROOT, "R/output")                     # R-native intermediates (.rds)

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

read_stata <- function(path) {
  read_dta(path) %>% mutate(across(where(is.labelled), ~ as.numeric(.x)))
}

save_step <- function(df, name) {
  saveRDS(df, file.path(OUTPUT_DIR, paste0(name, ".rds")))
  invisible(df)
}
