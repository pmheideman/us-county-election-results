## Download the U.S. Representative results from the Indiana Election Division's Election Night Reporting ARCHIVE (certified results),
##   https://enr.indianavoters.in.gov/archive/<YEAR>General/index.html   (an AngularJS app that reads static JSON files under .../data/)
## Files used (found in the app's own scripts): data/settings.json (-> VersionType), data/statewideElectionsC_<version>.json (office list with OFFICECATEGORYID),
## data/OffCatC_<OFFICECATEGORYID>_<version>.json (district summaries plus one race per county with candidate votes). Archives exist for 2016, 2018, 2020, 2022 and 2024
## (2008-2014 return "BlobNotFound"). A handful of requests per year, cached in R/data/raw_house_county_open_states/indiana_enr/ (re-runs skip existing files).
library(jsonlite)
DIR <- file.path("R", "data", "raw_house_county_open_states", "indiana_enr"); dir.create(DIR, showWarnings = FALSE, recursive = TRUE)
UA <- shQuote("Mozilla/5.0 (research; county election data project)")
get <- function(u, f) { if (!file.exists(f) || file.size(f) < 50) { Sys.sleep(1); system2("curl", c("-sL", "--compressed", "-m", "60", "-A", UA, "-o", shQuote(f), shQuote(u))) }; fromJSON(f, simplifyVector = FALSE) }
for (y in c(2016, 2018, 2020, 2022, 2024)) {
  B <- sprintf("https://enr.indianavoters.in.gov/archive/%dGeneral/data/", y)
  st <- get(paste0(B, "settings.json"), file.path(DIR, sprintf("settings_%d.json", y)))$Root; v <- st$VersionType
  ol <- get(sprintf("%sstatewideElectionsC_%s.json", B, v), file.path(DIR, sprintf("statewideElectionsC_%d.json", y)))$Root$List
  items <- unlist(lapply(ol, function(h) { it <- h$Items$Item; if (!is.null(it$OFFICECATEGORYID)) list(it) else it }), recursive = FALSE)
  nm <- vapply(items, function(i) i$OFFICE_CATEGORY_NAME, ""); id <- vapply(items, function(i) i$OFFICECATEGORYID, "")
  hit <- id[grepl("^US Representative", nm)]; stopifnot(length(hit) == 1)
  get(sprintf("%sOffCatC_%s_%s.json", B, hit, v), file.path(DIR, sprintf("usrep_%d.json", y)))
  hs <- id[grepl("^US Senator", nm)]                                     # U.S. Senate (2016, 2018, 2022, 2024 have a race; 2020 has none)
  if (length(hs) == 1 && y != 2020) get(sprintf("%sOffCatC_%s_%s.json", B, hs, v), file.path(DIR, sprintf("ussen_%d.json", y)))
  message(y, ": election ", st$CurrentElection, ", certified = ", st$Certified, ", version ", st$VersionCode, ", U.S. Representative category ", hit)
}
