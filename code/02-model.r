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
  simple = plm(I(log(gdp)) ~ gov_type, model = "pooling", data = pdf),
  fixed = plm(
    I(log(gdp)) ~ gov_type +
      year +
      I(log(pop)) +
      I(log(occ_ind)) +
      I(log(occ_agr)) +
      I(log(occ_ser)),
    effect = "individual",
    model = "random",
    data = pdf
  ),
  growth = plm(
    I(log(gdp)) ~ gov_type +
      years_since +
      years_since * gov_type,
    effect = "individual",
    model = "random",
    data = pdf
  )
)

# GDP Per Capita Models
gdp_pc_models <- list(
  simple_pc = plm(I(log(gdp_pc)) ~ gov_type, model = "pooling", data = pdf),
  nominal_fixed_pc = plm(
    gdp_pc ~ gov_type + year + occ_ind + occ_agr + occ_ser,
    effect = "individual",
    model = "random",
    data = pdf
  ),
  growth_pc = plm(
    I(log(gdp_pc)) ~ gov_type +
      years_since +
      years_since * gov_type,
    effect = "individual",
    model = "random",
    data = pdf
  ),
  nominal_growth_pc = plm(
    gdp_pc ~ gov_type + years_since + years_since * gov_type,
    effect = "individual",
    model = "random",
    data = pdf
  )
)
# Growth Models drop sectoral controls, the change in sectoral composition
# may be apart of the benefit to regional autonomy, via economic policy, plans etc.
#---------------------------------------------

## Determine Residual Structure

all_models <- c(gdp_models, gdp_pc_models)

write_rds(all_models, "models/full_models.rds")
## Heteroskedastic and clearly clustered

## Extracting Standard Errors Use vcovDC, and choose between HC1
standard_errors <- map(all_models, \(x) {
  tryCatch(
    {
      r <- vcovDC(x, "sss")
      coeftest(x, vcov. = r)
    },
    error = \(e) {
      print(e)
      stop()
    }
  )
})
write_rds(standard_errors, "models/model_se.rds")
