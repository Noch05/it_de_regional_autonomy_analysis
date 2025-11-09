library(estimatr)
library(here)
library(tidymodels)
library(tidyverse)

df <- read_csv(here("data/it_de_regional_data_cleaned.csv")) |>
  mutate(year = as.character(year))

# Year and region fixed effects, growth rates, no POP

lm1 <- lm(
  I(log(gdp)) ~ gov_type + year + regions + occ_agr + occ_ind + occ_ser - 1,
  data = df
)

lm2 <- lm(
  I(log(gdp_pc)) ~ gov_type + year + regions + occ_agr + occ_ind + occ_ser - 1,
  data = df
)

# Region Fixed effects,

lm3 <- lm()
