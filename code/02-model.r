library(here)
library(plm)
library(tidyverse)
library(texreg)
library(stargazer)

df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

# Keeping Italian NUTS1 Regions Nord-est and Nord-Ovest
italy_keep <- c(
  "valle d'aosta",
  "liguria",
  "piemonte",
  "veneto",
  "emilia-romagna",
  "lombardia",
  "trentino-alto adige",
  "friuli-venezia giula"
)

# Keeping most comparable German Regions, by geographical proximity, and economic similarity
germany_keep <- c(
  "baden-wuerttemberg",
  "bayern",
  "hessen",
  "nordrhein-westfalen"
)
df_reduced <- df |>
  filter(regions %in% c(italy_keep, germany_keep))
