library(here)
library(tidyverse)
library(fixest)
library(kableExtra)

# Reading in
options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))
models <- read_rds(here("models/full_models.rds"))

model_table <- etable(
  models,
  digits = 5,
  se.below = TRUE,
  dict = c(
    `I(log(gdp))` = "log(GDP)",
    `I(log(gdp_pc))` = "log(GDP Per Capita)",
    `gov_typespecialstatute` = "Special Statute Region",
    `gov_typefederal` = "Federal Länder",
    `years_since` = "Years Since 1977",
    `occ_agr` = "Agricultural (%)",
    `occ_ind` = "Industrial (%)",
    `occ_ser` = "Service (%)",
    `year` = "Year",
    `regions` = "Region/Länder"
  ),
  headers = list("GDP Models" = 3, "GDP Per Capita Models" = 3),
  tex = TRUE,
  export = "tables/model_table.png",
  signif.code = c("*" = 0.05),
  notes = c(
    "* p\\textless0.05",
    "Standard errors in parentheses",
    "Clustered by time and region"
  ),
  style.tex = style.tex(main = "aer")
)


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
  mutate(regions = str_to_title(regions), gdp = gdp / 1e6, pop = pop / 1e6) |>
  rename(
    Region = regions,
    `GDP (Millions, PPS 1985)` = gdp,
    `GDP Per Capita (PPS 1985)` = gdp_pc,
    `Agricultural (%)` = occ_agr,
    `Industrial (%)` = occ_ind,
    `Service (%)` = occ_ser,
    `Population (Millions)` = pop
  )

kable(
  df2,
  format = "html",
  table.attr = 'class="table table-striped table-hover"'
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  ) %>%
  save_kable("tables/summary.png")
