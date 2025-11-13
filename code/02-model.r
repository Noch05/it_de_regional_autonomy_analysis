library(here)
library(lmtest)
library(fixest)
library(tidyverse)


options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

#-------------------------------------------
twoway <- function(x) {
  return(
    vcov_cluster(
      x,
      cluster = c("regions", "years_since"),
      ssc = ssc(K.exact = TRUE)
    )
  )
}
models <- list(
  feols(I(log(gdp)) ~ gov_type | year, data = df),
  feols(I(log(gdp)) ~ years_since + gov_type:years_since | regions, data = df),
  feols(
    I(log(gdp)) ~ years_since +
      gov_type:years_since +
      occ_agr +
      occ_ind +
      occ_ser |
      regions,
    data = df
  ),
  feols(I(log(gdp_pc)) ~ gov_type | year, data = df),
  feols(
    I(log(gdp_pc)) ~ years_since + gov_type:years_since | regions,
    data = df
  ),
  feols(
    I(log(gdp_pc)) ~ years_since +
      gov_type:years_since +
      occ_agr +
      occ_ind +
      occ_ser |
      regions,
    data = df
  )
) |>
  map(\(x) {
    model <- summary(x, vcov = twoway(x))
    model$cov.iid <- NULL
    model$cov.unscaled <- NULL
    # Removing original vcov matrices
    return(model)
  })
write_rds(models, "models/full_models.rds")

# 1st Growth Model drop sectoral controls, the change in sectoral composition
# may be apart of the benefit to regional autonomy, via economic policy, plans etc.
#---------------------------------------------
