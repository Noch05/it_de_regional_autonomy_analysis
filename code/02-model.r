library(here)
library(fixest)
library(tidyverse)
library(tinytex)
library(magick)


options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

#-------------------------------------------
# Defining Standard Error Clustering
twoway <- function(x) {
  return(
    vcov_cluster(
      x,
      cluster = c("regions", "years_since"),
      ssc = ssc(K.exact = TRUE)
    )
  )
}
# 6 Models totel, 3 for GDP and 3 for GDP per capita
# Each are the same
# Model 1 and 4, Simple with Time Fixed Effects
# Model 2 and 5, Region Fixed Effects, interaction between autonomy and time
# Model 3 and 6: Same as 2 and 5, but with economic sectoral covariates

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

#---------------------------------------------

# Making Regression Table
## Table Creating Function
create_etable <- function(dict, group = NULL, digits) {
  etable(
    models,
    digits = digits,
    se.below = TRUE,
    dict = dict,
    headers = list("**GDP Models**" = 3, "**GDP Per Capita Models**" = 3),
    tex = TRUE,
    signif.code = c("*" = 0.05),
    notes = c(
      "1",
      "2",
      "3"
    ),
    group = group,
    style.tex = style.tex(main = "aer"),
    fixef_sizes.simplify = TRUE,
    fixef_sizes = TRUE
  )
}

## Table Inputs
dicts <- list(
  dict1 = c(
    `I(log(gdp))` = "log(GDP)",
    `I(log(gdp_pc))` = "log(GDP Per Capita)",
    `gov_typespecialstatute` = "Special Statute Region",
    `gov_typefederal` = "Federal Länder",
    `years_since` = "Years Since 1977",
    `year` = "Year",
    `regions` = "Region/Länder",
    "1" = "*p \\textless 0.05*",
    "2" = "*Standard errors (SE) in parentheses*",
    "3" = "*SE clustered by **time** and **region** *"
  ),
  dict2 = c(
    `I(log(gdp))` = "log(GDP)",
    `I(log(gdp_pc))` = "log(GDP Per Capita)",
    `gov_typespecialstatute` = "Special Statute Region",
    `gov_typefederal` = "Federal Länder",
    `years_since` = "Years Since 1977",
    `year` = "Year",
    `regions` = "Region/Länder",
    "1" = "*p \\textless 0.05*",
    "2" = "*Standard errors (SE) in parentheses*",
    "3" = "*SE clustered by **time** and **region** *",
    occ_agr = "Agricultural (%)",
    occ_ind = "Industrial (%)",
    occ_ser = "Services (%)"
  )
)

groups <- list(
  group1 = list(
    "Controls: Sectoral Economic Shares" = c(
      "occ_agr",
      "occ_ind",
      "occ_ser"
    )
  ),
  group2 = c()
)

## Creeating Tables and PNGs

etables <- map2(dicts, groups, \(x, y) {
  create_etable(dict = x, group = y, digits = 5)
})

files <- list(
  here("tex/model_table_cond.tex"),
  here("tex/model_table_full.tex")
)

walk2(etables, files, \(tex, file) {
  packages <- c("booktabs", "dcolumn", "siunitx", "amssymb", "pdflscape") |>
    map_chr(\(x) paste0("\\usepackage{", x, "}"))

  completed <- c(
    "\\documentclass{article}",
    packages,
    "\\begin{document}",
    "\\begin{landscape}",
    tex,
    "\\end{landscape}",
    "\\end{document}"
  )
  write_lines(completed, file, append = FALSE)
  pdf <- str_replace_all(file, "tex", "pdf")
  png <- str_replace(file, "\\.tex", "\\.png") |>
    str_replace("tex", "tables")

  latexmk(
    file,
    engine = "pdflatex",
    pdf_file = pdf
  )

  image_read_pdf(pdf, density = 300) |>
    image_write(png)
})
