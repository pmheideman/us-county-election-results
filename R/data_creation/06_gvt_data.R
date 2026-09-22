## Port of Files/Dofiles/Data_creation/6th_GvtData.do
##
## Reshapes the Census Annual Survey of State & Local Government Finances (line item LOG315,
## general expenditure) from wide (one column per survey year) to a county-by-year panel, used
## downstream to build the "welfare channel" variable. Survey years 1982/1992/2002 are relabeled
## 1980/1990/2000 to align with the decennial Census years used throughout the rest of the
## pipeline. Only the LOG315 line item is kept -- the source Excel file has several other
## LOG3xx line items (LOG310, LOG320, ...) that the source do-file's `reshape long LOG315`
## leaves untouched (wide) and unused by any downstream do-file, so they're dropped here rather
## than carried through unused.

source(file.path("R", "00_setup.R"))

gvt_final <- read_excel(file.path(INPUT_DIR, "GvtExpenditures.xls"), sheet = "Data") %>%
  mutate(
    cty_fips = as.integer(STCOU),
    county_part = substr(STCOU, 3, 5)   # Stata's substr(STCOU,3,3) is (start=3, length=3)
  ) %>%
  filter(county_part != "000") %>%
  select(cty_fips, LOG3151982, LOG3151992, LOG3152002) %>%
  pivot_longer(cols = starts_with("LOG315"), names_to = "t", names_prefix = "LOG315",
               names_transform = list(t = as.integer), values_to = "gvt_exp") %>%
  mutate(year = case_when(t == 1982 ~ 1980, t == 1992 ~ 1990, t == 2002 ~ 2000)) %>%
  select(-t) %>%
  arrange(cty_fips, year) %>%
  save_step("gvt_final")
