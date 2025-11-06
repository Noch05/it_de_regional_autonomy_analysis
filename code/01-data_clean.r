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
      mutate(YEAR_num = as.numeric(YEAR)) |>
      select(NUTS, CODE, REGIONS, YEAR, YEAR_num, value)
  }
)
rm(all_data)

# Checking for NA's and stopping in case
NA_vals <- map_dbl(
  subset_data,
  ~ {
    sum(is.na(.x$value))
  }
)
if (any(NA_vals != 0)) {
  NA_vals2 <- NA_vals[NA_vals != 0]
  stop(
    "NA Values found in ",
    paste0(names(NA_vals2), collapse = ", ")
  )
}
rm(NA_vals)
subset_data <- map2(subset_data, names(subset_data), \(x, y) {
  rename(x, !!y := value)
})

# Merging and adding new variables
df <- reduce(
  subset_data,
  full_join,
  by = c("NUTS", "CODE", "REGIONS", "YEAR", "YEAR_num")
) |>
  mutate("")

names(df) <- str_replace_all(names(df), "\\s", "_") |> tolower()

write_csv(df, here("data/it_de_regional_data_cleaned.csv"))

# nuts is EU NUTS code
# code is the code used in the dataset for each region
# region is the name of the region/lander
# year is the year as a string
# year_num is the val as a double
# All the other variables are detailed above
