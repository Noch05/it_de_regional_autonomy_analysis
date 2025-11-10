library(here)
library(plm)
library(tidyverse)
library(texreg)
library(xtable)

df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

# GDP Models
gdp_models <- list(
  simple = plm(
    I(log(gdp)) ~ gov_type,
    data = df,
    model = "pooling",
    index = c("year", "regions")
  )
)

# GDP Per Capita Models
