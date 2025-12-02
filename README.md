# Regional Autonomy and Economic Growth: Comparing German and Italian Sub-National Systems
 
## Overview
This work attempts to determine how sub-national autonomy impacts economic growth within sub-national units, utilizing the case studies of Germany and Italy. These case studies provide three distinct dimensions of analysis, highlighting the impact of autonomy across different types of states.

Italy's unique historical circumstances have created an asymmetric regionalist state, providing two dimensions of analysis. The Italian peninsula has traditionally been fragmented, resulting in strong regional identities, which were subsequently worked into the framework of the unitary Italian Republic. Italy has 15 ordinary regions, which at their inception did not have defined powers. Over time, they have gained more administrative responsibilities, while expanding their fiscal powers has lagged, resulting in a high degree of reliance on the central state. The other 5 regions are special-statute regions that have both significant administrative and fiscal powers compared to the ordinary regions. However, both types are firmly subordinated to the central state under the unitary structure.

Germany, by contrast, is a federal republic; each of its federal Länder has a significant degree of autonomy that is symmetric across the federation, indicating each Länder has the same powers, responsibilities, and level of sovereignty. This comparison of autonomy within a unitary and federal state will provide important insight into the degree to which autonomy is beneficial for growth, and how the context in which it is exercised plays a role in determining its benefits. 

## Methods, Models, and Results
Using data from the Regio-EU 1977-1996 dataset (available in the `data` directory), I trained several panel effects models to estimate the impact of autonomy on economic growth. There are 6 models in total, 3 using $\log(\text{GDP})$ as the dependent variable, and 3 using $\log(\text{GDP per Capita})$ as proxies for economic output and standard of living, respectively.

* Model 1 is a simple model containing only the level of autonomy and time fixed effects($\lambda_t$).
```math
\log(y_{it}) = \beta_1(\text{GovType}_i) + \lambda_t + \epsilon_it
```
* Model 2 features regional fixed effects ($\alpha_i$), and the interaction between time (normalized to years after 1977) and autonomy level.
```math
\log(y_{it}) = \beta_1 \, \text{Year}_t + \beta_2 \left( \text{Year}_t \times \text{GovType}_i \right) + \alpha_i + \epsilon_{it}
```
* Model 3 is an expanded version of model 2 containing additional covariates ($\mathbf{X}_{it}$) describing the sectoral composition of regional economies. These compositions change over time, so aren't fully captured by the regional fixed effects.
```math
\log(y_{it}) = \beta_1 \, \text{Year}_t + \beta_2 \left( \text{Year}_t \times \text{GovType}_i \right) + \boldsymbol{\gamma} \mathbf{X}_{it} + \alpha_i + \epsilon_{it}
```

All the standard errors for these models are clustered by time and region to match the correlation structure of the residuals, and statistical significance was gauged using $\alpha =0.05$ for all coefficients. The full table is available in the `tables` directory. 

Ultimately, while both the special-statute regions and the federal Länder are associated with higher rates of GDP and GDP per capita growth as compared to the ordinary regions, only the coefficients for the special-statute regions are consistently significant. This suggests that the return to autonomy can be highly dependent on the institutional contexts in which it is exercised. The Italian special-statute regions benefit more conclusively because they remain embedded within the unitary framework, benefiting from the active central state in the same way as ordinary regions, while federal Länder have more responsibilities and less support from their central state, offsetting the benefits of their autonomy. 

## Replication

All of the data required to replicate the analysis is available within the repo, along with intermediary files at each step. The code folder contains 4 `.r` files, each labelled with a  number, from `01` to `04`, that executes a portion of the analysis:

* `01-data_clean.r`: Takes the original data, subsets to the regions and variables of interest, then writes the cleaned data in `.rds` and `.csv` format.
* `02-model.r`: Runs the panel fixed effects models, saves the model objects in `.rds` format, and the regression table in `.tex`, `.pdf`, and `.png` format. The `.tex` is the raw LaTeX, while the `.pdf`, and `.png` are rendered. 
* `03-graph.r`: Creates graphs of GDP and GDP per capita over time, using different colors for each region, and saving them as `.png` files.
* `04-table.r`: Creates the summary statistic tables for each region, and saves them as `.tex`, `.pdf`, and `.png` files. The `.tex` is the raw LaTeX, while the `.pdf`, and `.png` are rendered. 

To replicate the analysis, I reccomend that each file be run sequentially, though strictly, once file `01` has run the others can be executed in any order. Simply ensure that all of the appropriate R-packages and other prerequisites are installed. The packages required are `{here}`, `{tidyverse}`, `{fixest}`, `{stargazer}`, `{magick}`, `{tinytex}`, and a proper LaTeX distribution, with the `{booktabs}`, `{dcolumn}`, `{siunitx}`, `{amssymb}`, `{pdflscape}` packages installed. As far as the LaTeX generation, I highly recommend installing the TinyTeX distribution using `{tinytex}` in `R`, and then it will download any necessary LaTeX packages when rendering. Also keep in mind that doing this will create intermediate directories for the `.pdf` and `.tex` intermediate outputs as well. 

However, the outputs are also already available within the repo. The cleaned data is available in `.rds` and `.csv` format, the models are available in `.rds` format, and all the plots and tables are available in `.png` format for your viewing.
