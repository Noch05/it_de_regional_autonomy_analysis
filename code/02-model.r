library(here)
library(plm)
library(tidyverse)
library(texreg)
library(xtable)

options(scipen = 999)
df <- read_rds(here("data/it_de_regional_data_cleaned.rds"))

#-------------------------------------------

pdf <- pdata.frame(df, index = "regions")

# GDP Models

# GDP Per Capita Models
