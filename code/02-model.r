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
  mixed = plm(
    I(log(gdp)) ~ gov_type + year + pop + occ_ind + occ_agr + occ_ser,
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
  nominal_mixed_pc = plm(
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

walk2(all_models, names(all_models), \(x, y) {
  df <- tibble(
    residuals = residuals(x),
    fitted = fitted(x),
    time = as.numeric(str_extract(names(residuals(x)), "\\d\\d\\d\\d"))
  )
  ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "blue", linetype = 2) +
    theme_minimal()
  ggsave(paste0("plots/", y, "_fit_resid_plot.png"))
  ggplot(df, aes(x = time, y = residuals)) +
    geom_point() +
    theme_minimal()
  ggsave(paste0("plots/", y, "_time_resid_plot.png"))
})

## Extracting Standard Errors Use vcovDC, and choose between HC1 and HC3, likely 3.
## Calculating
