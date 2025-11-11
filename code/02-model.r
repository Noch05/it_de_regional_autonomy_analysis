library(here)
library(plm)
library(tidyverse)

options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

#-------------------------------------------
# Region is entity, Year is time
pdf <- pdata.frame(df, index = c("regions", "year")) |>
  select(
    year,
    years_since,
    gov_type,
    federal,
    pop,
    gdp,
    gdp_pc,
    occ_agr,
    occ_ind,
    occ_ser
  )

# effect = "twoways" is both, effect = "individual" is region,
# effect = "time" is year
# model = "random" is a Random Effects, model = "within" is a fixed effects.
# The index variables in the effects do not need to be in the formula
# GDP Models
gdp_models <- list(
  simple = plm(I(log(gdp)) ~ gov_type, model = "pooling", data = pdf),
  growth = plm(
    I(log(gdp)) ~ gov_type + years_since + years_since * gov_type,
    effect = "individual",
    model = "random",
    data = pdf
  ),
  mixed = plm(
    I(log(gdp)) ~ gov_type + year + pop + occ_ind + occ_agr + occ_ser,
    effect = "individual",
    model = "random",
    data = pdf
  )
)

# GDP Per Capita Models
gdp_pc_models <- list(
  simple = plm(I(log(gdp_pc)) ~ gov_type, model = "pooling", data = pdf),
  nominal_mixed = plm(
    gdp_pc ~ gov_type + year + occ_ind + occ_agr + occ_ser,
    effect = "individual",
    model = "random",
    data = pdf
  ),
  growth = plm(
    I(log(gdp_pc)) ~ gov_type + years_since + years_since * gov_type,
    effect = "individual",
    model = "random",
    data = pdf
  ),
  nominal_growth = plm(
    gdp_pc ~ gov_type + years_since + years_since * gov_type,
    effect = "individual",
    model = "random",
    data = pdf
  )
)

## Determine Residual Structure

all_models <- c(gdp_models, gdp_pc_models)

## Extracting Standard Errors Use vcovDC, and choose between HC1 and HC3, likely 3.
## Calculating
