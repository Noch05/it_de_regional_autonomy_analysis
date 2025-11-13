library(tidyverse)
library(here)
library(gridExtra)

df <- read_rds(here("data/it_de_regional_data_cleaned.rds")) |>
  mutate(
    country = if_else(federal == 1, "German", "Italian"),
    gdp_divide = gdp / 1e9
  )
#-------------------------------------------
# Graphing GDP over time per region
df |>
  ggplot(aes(x = year_num, y = gdp_divide, color = regions, shape = country)) +
  geom_jitter(size = 2, alpha = 0.8, width = 0.3, height = 0) +
  scale_color_discrete(labels = unique(str_to_title(df$regions))) +
  labs(
    x = "Year",
    y = "GDP (Billions, PPS, 1985 Prices)",
    title = "GDP in Purchasing Power Standard Over Time",
    color = "Region/Länder",
    shape = "Country"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("plots/gdp_plot.png"))
#-------------------------------------------
# Graphing GDP per capita over time per region
df |>
  ggplot(aes(x = year_num, y = gdp_pc, color = regions, shape = country)) +
  geom_jitter(size = 2, alpha = 0.8, width = 0.3, height = 0) +
  scale_color_discrete(labels = unique(str_to_title(df$regions))) +
  labs(
    x = "Year",
    y = "GDP Per Capita ( PPS, 1985 Prices)",
    title = "GDP Per Capita in Purchasing Power Standard Over Time",
    color = "Region/Länder",
    shape = "Country"
  ) +
  scale_y_continuous() +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.box = "vertical",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
ggsave(here("plots/gdp_pc_plot.png"))

## Determining Residual Structures

all_models <- read_rds("models/full_models.rds")

residual_plots <- map2(all_models, names(all_models), \(x, n) {
  df <- tibble(
    residuals = residuals(x),
    fitted = fitted(x),
    time = as.numeric(str_extract(names(residuals(x)), "\\d\\d\\d\\d"))
  )
  list(
    ggplot(df, aes(x = fitted, y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, color = "blue", linetype = 2) +
      theme_minimal() +
      labs(title = paste(n, "fitted")),

    ggplot(df, aes(x = time, y = residuals)) +
      geom_point() +
      theme_minimal() +
      labs(title = paste(n, "time"))
  )
}) |>
  unlist()

ggsave(
  filename = "plots/residual_plots.pdf",
  plot = marrangeGrob(residual_plots, nrow = 4, ncol = 4),
  width = 16,
  height = 9
)
