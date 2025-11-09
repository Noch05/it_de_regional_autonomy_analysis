library(estimatr)
library(here)
library(tidymodels)
library(tidyverse)
library(texreg)

df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))
# GDP Models
gdp_mod <- list(
  lm1 <- lm_robust(I(log(gdp)) ~ federal, data = df),
  lm2 <- lm_robust(I(log(gdp)) ~ federal + regions + years_since, data = df),
  lm3 <- lm_robust(
    I(log(gdp)) ~ federal + regions + year + occ_agr + occ_ser + occ_ind,
    data = df
  )
)

# GDP per Capita Models
gdp_pc_mod <- list(
  lm_p1 <- lm_robust(I(log(gdp_pc)) ~ federal, data = df),
  lm_p2 <- lm_robust(
    I(log(gdp_pc)) ~ federal + regions + years_since,
    data = df
  ),
  lm_p3 <- lm_robust(
    I(log(gdp_pc)) ~ federal +
      regions +
      year +
      occ_agr +
      occ_ser +
      occ_ind,
    data = df
  ),
  lm_p4 <- lm_robust(
    gdp_pc ~ federal + regions + year + occ_agr + occ_ser + occ_ind,
    data = df
  )
)

gdp_tex <- texreg(
  gdp_mod,
  file = here("results/gdp.tex"),
  stars = numeric(0),
  custom.header = list("GDP Growth Rates" = 1:3),
  custom.model.names = c("Simple", "Region FE", "Region and Year FE"),
  custom.coef.map = list(
    "federalTRUE" = "Federal",
    "years_since" = "Years Since 1977",
    "occ_agr" = "Agricultural Workforce (% of Population)",
    "occ_ser" = "Service Workforce (% of Population)",
    "occ_ind" = "Industrial Workforce (% of Population)"
  ),
  digits = 5
)

texreg(
  gdp_pc_mod,
  file = here("results/gdp_pc.tex"),
  stars = numeric(0),
  custom.header = list(
    "GDP Per Capita Growth Rates" = 1:3,
    "GDP Per Capita" = 4
  ),
  custom.model.names = c(
    "Simple",
    "Region FE",
    "Region FE and Year FE",
    "Region FE and Year FE"
  ),
  custom.coef.map = list(
    "federalTRUE" = "Federal",
    "years_since" = "Years Since 1977",
    "occ_agr" = "Agricultural Workforce (% of Population)",
    "occ_ser" = "Service Workforce (% of Population)",
    "occ_ind" = "Industrial Workforce (% of Population)"
  ),
  digits = 5
)
