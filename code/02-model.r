library(estimatr)
library(here)
library(tidymodels)
library(tidyverse)
library(texreg)

df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))
# GDP Models
lm1 <- lm_robust(I(log(gdp)) ~ federal, data = df)
lm2 <- lm_robust(I(log(gdp)) ~ federal + regions + years_since, data = df)
lm3 <- lm_robust(
  I(log(gdp)) ~ federal + regions + year + occ_agr + occ_ser + occ_ind,
  data = df
)

# GDP per Capita Models
lm_p1 <- lm_robust(I(log(gdp_pc)) ~ federal, data = df)
lm_p2 <- lm_robust(I(log(gdp_pc)) ~ federal + regions + years_since, data = df)
lm_p3 <- lm_robust(
  I(log(gdp_pc)) ~ federal +
    regions +
    years_since +
    occ_agr +
    occ_ser +
    occ_ind,
  data = df
)
lm_p4 <- lm_robust(
  gdp_pc ~ federal + regions + year + occ_agr + occ_ser + occ_ind,
  data = df
)
