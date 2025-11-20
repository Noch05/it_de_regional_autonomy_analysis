library(here)
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
# Making Regression Table
model_table <- etable(
  models,
  digits = 5,
  se.below = TRUE,
  dict = c(
    `I(log(gdp))` = "log(GDP)",
    `I(log(gdp_pc))` = "log(GDP Per Capita)",
    `gov_typespecialstatute` = "Special Statute Region",
    `gov_typefederal` = "Federal Länder",
    `years_since` = "Years Since 1977",
    `occ_agr` = "Agricultural (%)",
    `occ_ind` = "Industrial (%)",
    `occ_ser` = "Service (%)",
    `year` = "Year",
    `regions` = "Region/Länder"
  ),
  headers = list("GDP Models" = 3, "GDP Per Capita Models" = 3),
  tex = TRUE,
  export = "tables/model_table.png",
  signif.code = c("*" = 0.05),
  notes = c(
    "* p\\textless0.05",
    "Standard errors in parentheses",
    "Clustered by time and region"
  ),
  style.tex = style.tex(main = "aer")
)
