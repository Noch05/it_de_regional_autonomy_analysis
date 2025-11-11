library(here)
library(tidyverse)
library(stargazer)

options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

df <- df |>
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
  )
