library(tidyverse)
library(readxl)
library(here)

# Reading in Data

sheet_idx <- c(
  "POP", # Population / 1000
  "GDP", # GDP PPS Constant 1985 Prices (Real)
  "GDP PC", # GPD Per capita 1985 Constant Prices
  "OCC AGR", # Pop in Agriculture /1000
  "OCC IND", # Pop in Industry /1000
  "OCC SER", # Pop in Services /1000
  "OCC TOT" # Total Employment /1000
)

all_data <- lapply(sheet_idx, \(x) {
  read_xls(here("data/regio-eu.xls"), sheet = x, range = "A14:T133")
})
names(all_data) <- sheet_idx

# Subset to Germany and Italy, Pivoting Year Columns into 1 variable
subset_data <- pmap(
  list(all_data, sheet_idx),
  function(x, y) {
    x |>
      filter(str_detect(CODE, "^I[0-9][0-9]") | str_detect(CODE, "^D[0-9]")) |>
      mutate(across(starts_with("19"), as.numeric)) |>
      pivot_longer(cols = starts_with("19"), names_to = "YEAR") |>
      rename(!!y := value) |>
      mutate(YEAR_num = as.numeric(YEAR))
  }
)
rm(all_data)

# Checking for NA's
NA_vals <- map_dbl(
  subset_data,
  ~ {
    sum(is.na(.x[[5]]))
  }
)

# Merging
