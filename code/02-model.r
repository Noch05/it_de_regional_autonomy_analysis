library(estimatr)
library(here)
library(tidymodels)
library(tidyverse)
library(texreg)
library(stargazer)

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
  custom.coef.map = list(
    "federalTRUE" = "Federal",
    "years_since" = "Years Since 1977",
    "occ_agr" = "Agricultural Workforce (% of Population)",
    "occ_ser" = "Service Workforce (% of Population)",
    "occ_ind" = "Industrial Workforce (% of Population)"
  ),
  digits = 5,
  booktabs = TRUE,
  caption = "GDP Growth Rate Models",
  label = "",
  caption.above = TRUE,
  sideways = TRUE,
  use.packages = FALSE,
  include.ci = FALSE,
  include.se = TRUE,
  custom.gof.names = c(
    NA,
    NA,
    "N"
  ),
  custom.gof.rows = list(
    "Regional Fixed Effects" = c("", ".", "."),
    "Year Fixed Effects" = c("", "", ".")
  ),
  include.rmse = FALSE,
)

texreg(
  gdp_pc_mod,
  file = here("results/gdp_pc.tex"),
  stars = numeric(0),
  custom.header = list(
    "GDP Per Capita Growth Rates" = 1:3,
    "GDP Per Capita" = 4
  ),
  custom.coef.map = list(
    "federalTRUE" = "Federal",
    "years_since" = "Years Since 1977",
    "occ_agr" = "Agricultural Workforce (% of Population)",
    "occ_ser" = "Service Workforce (% of Population)",
    "occ_ind" = "Industrial Workforce (% of Population)"
  ),
  digits = 5,
  booktabs = TRUE,
  use.packages = FALSE,
  caption = "GDP Per Capita Models",
  label = "",
  caption.above = TRUE,
  sideways = TRUE,
  include.ci = FALSE,
  include.se = TRUE,
  custom.gof.names = c(
    NA,
    NA,
    "N"
  ),
  custom.gof.rows = list(
    "Regional Fixed Effects" = c("", ".", ".", "."),
    "Year Fixed Effects" = c("", "", ".", ".")
  ),
  include.rmse = FALSE,
)

all_percent_changes <- c(
  map_dbl(gdp_mod, \(x) exp(coef(x)[2]) - 1),
  map_dbl(gdp_pc_mod, \(x) exp(coef(x)[2]) - 1)
)[-7] *
  100

percent_df <- data.frame(
  Model = c(
    "GDP 1",
    'GDP 2',
    "GDP 3",
    "Per Capita 1",
    "Per Capita 2",
    "Per Capita 3"
  ),
  `Percent_Change` = all_percent_changes
)

stargazer(
  percent_df,
  summary = FALSE,
  header = FALSE,
  out = "results/percent_change.tex",
  float = TRUE
)
