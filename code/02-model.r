library(here)
library(lmtest)
library(plm)
library(tidyverse)

options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

#-------------------------------------------
# Region is entity, Year is time
pdf <- df |>
  select(
    regions,
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
  ) |>
  pdata.frame(index = c("regions", "year"))

# effect = "twoways" is both, effect = "individual" is region,
# effect = "time" is year
# model = "random" is a Random Effects, model = "within" is a fixed effects.
# The index variables in the effects do not need to be in the formula
# GDP Models
gdp_models <- list(
  simple = plm(
    I(log(gdp)) ~ gov_type,
    model = "within",
    effect = "time",
    data = pdf
  ),
  fixed = plm(
    I(log(gdp)) ~ years_since + gov_type:years_since,
    effect = "individual",
    model = "within",
    data = pdf
  ),
  fixed_full = plm(
    I(log(gdp)) ~ years_since +
      gov_type:years_since +
      occ_agr +
      occ_ind +
      occ_ser,
    effect = "individual",
    method = "within",
    data = pdf
  )
)

# GDP Per Capita Models
gdp_pc_models <- list(
  simple_pc = plm(
    I(log(gdp_pc)) ~ gov_type,
    model = "within",
    effect = "time",
    data = pdf
  ),
  fixed_pc = plm(
    I(log(gdp_pc)) ~ years_since + gov_type:years_since,
    effect = "individual",
    model = "within",
    data = pdf
  ),
  fixed_full_pc = plm(
    I(log(gdp_pc)) ~ years_since +
      gov_type:years_since +
      occ_agr +
      occ_ind +
      occ_ser,
    effect = "individual",
    method = "within",
    data = pdf
  )
)
# 1st Growth Model drop sectoral controls, the change in sectoral composition
# may be apart of the benefit to regional autonomy, via economic policy, plans etc.
#---------------------------------------------

## Determine Residual Structure

all_models <- c(gdp_models, gdp_pc_models)

write_rds(all_models, "models/full_models.rds")

## Extracting Standard Errors Use vcovDC, and choose between HC1
standard_errors <- map(all_models, \(x) {
  tryCatch(
    {
      r <- vcovDC(x, "HC1")
      coeftest(x, vcov. = r, save = TRUE)
    },
    error = \(e) {
      print(e)
      stop()
    }
  )
})
write_rds(standard_errors, "models/model_se.rds")
