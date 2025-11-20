library(here)
library(tidyverse)
library(broom)
library(stargazer)
library(tinytex)

dfs <- read_rds(here("data/it_de_regional_data_cleaned.rds")) |>
  group_split(regions)

walk(dfs, \(x) {
  name <- str_to_title(unique(x$regions))
  x <- x |>
    mutate(gdp = gdp / 1e6, pop = pop / 1e3) |>
    select(
      `Population (Thousands)` = pop,
      `GDP (Millions)` = gdp,
      `GDP Per Capita` = gdp_pc,
      "Agriculture (%)" = occ_agr,
      "Industry (%)" = occ_ind,
      "Service (%)" = occ_ser
    ) |>
    as.data.frame()

  stargazer(
    x,
    type = "latex",
    header = FALSE,
    summary = TRUE,
    style = "aer",
    out.header = FALSE,
    out = here(paste0("tex/summary_table_", name, ".tex")),
    float = FALSE
  )
})
