library(here)
library(tidyverse)
library(stargazer)
library(tinytex)
library(magick)

dfs <- read_rds(here("data/it_de_regional_data_cleaned.rds")) |>
  group_split(regions)

walk(dfs, \(x) {
  name <- str_to_title(unique(x$regions))
  x <- x |>
    mutate(gdp = gdp / 1e6, pop = pop / 1e6) |>
    select(
      `Population (Millions)` = pop,
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
    float = FALSE,
    digits = 2
  )
})

tex_files <- list.files(
  here("tex"),
  full.names = TRUE
)

walk(tex_files, \(x) {
  tex <- read_lines(x)
  packages <- c("booktabs", "dcolumn", "siunitx") |>
    map_chr(\(z) paste0("\\usepackage{", z, "}"))

  completed <- c(
    "\\documentclass{article}",
    packages,
    "\\begin{document}",
    tex,
    "\\end{document}"
  )
  write_lines(completed, x, append = FALSE)
  pdf <- str_replace_all(x, "tex", "pdf")
  png <- str_replace(x, "\\.tex", "\\.png") |>
    str_replace("tex", "tables")
  latexmk(
    x,
    engine = "pdflatex",
    pdf_file = pdf
  )
  image_read_pdf(pdf, density = 300) |>
    image_write(png)
})
