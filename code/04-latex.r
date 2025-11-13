library(here)
library(tidyverse)
library(kableExtra)
library(modelsummary)

# Reading in
options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))
models <- read_rds(here("models/full_models.rds"))
se <- read_rds(here("models/model_se.rds"))
names(se) <- NULL

# Redoing each as OLS to get summary structure for table
# Replacing each coefficient with the vector from `se`

fe_row <- tribble(
  ~term                  , ~simple         , ~fixed          , ~fixed_full     , ~simple_pc      , ~fixed_pc       , ~fixed_full_pc  ,
  "Region Fixed Effects" , ""              , "$\\checkmark$" , "$\\checkmark$" , ""              , "$\\checkmark$" , "$\\checkmark$" ,
  "Time Fixed Effects"   , "$\\checkmark$" , ""              , ""              , "$\\checkmark$" , ""              , ""
)

model_table <- modelsummary(
  se,
  coef_rename = c(
    "Special Statute",
    "Federal",
    "Year",
    "Year * Special Statute",
    "Year * Federal",
    "Agriculture (%)",
    "Industry (%)",
    "Service (%)",
    "Log(Population)"
  ),
  add_rows = fe_row,
  gof_omit = "IC",
  stars = c("*" = 0.05),
  fmt = 5,
  output = "kableExtra",
  escape = FALSE,
  names = NULL
) |>
  add_header_above(
    c(" " = 1, "GDP" = 3, "GDP Per Capita" = 3)
  )
save_kable(model_table, "tables/model_table.png")


# Summary Statistic Table
df2 <- df |>
  select(
    regions,
    year,
    pop,
    gdp,
    gdp_pc,
    occ_agr,
    occ_ind,
    occ_ser,
    gov_type
  ) |>
  group_by(regions) |>
  summarize(
    across(
      c(pop, gdp, gdp_pc, occ_agr, occ_ind, occ_ser),
      ~ mean(.),
      .names = "{.col}"
    ),
    .groups = "drop"
  ) |>
  mutate(regions = str_to_title(regions)) |>
  rename(
    Region = regions,
    GDP = gdp,
    `GDP Per Capita` = gdp_pc,
    `Agricultural (%)` = occ_agr,
    `Industrial (%)` = occ_ind,
    `Service (%)` = occ_ser,
    Population = pop
  )

kable(df2, format = "html") |>
  save_kable("tables/summary.png")
