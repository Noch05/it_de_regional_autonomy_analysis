# Regional Autonomy and Economic Growth: Comparing German and Italian Sub-National Systems
 
## Overview

## Methods, Models, and Results

## Replication

All of the data required to replicate the analysis is available within the repo, along with intermediary files at each step. The code folder contains 4 `R` scripts each labelled with a number, simply run each of those files in succession and it will re-generate the output files in the repo. 

Simply ensure that all of the appropriate R-packages and other pre-requisites are installed. The packages required are `{here}`, `{tidyverse}`, `{fixest}`, `{stargazer}`, `{magick}`, `{tinytex}`, and a proper LaTeX distribution, with the `{booktabs}`, `{dcolumn}`, `{siunitx}`, `{assymb}`, `{pdflscape}` packages installed. As far as the LaTeX generation, I highly recommend installing the TinyTeX distribution using `{tinytex}` in `R`, and then it will download the necessary LaTeX packages when rendering if necessary. Also keep in mind that doing this will create intermediate directories for the `.pdf` and `.tex` intermidate outputs as well. 

However, the outputs are also readily available within the repo. The cleaned data is available in `.rds` and `.csv` format, the models are also available in `.rds` format, and all the plots and tables are available in `.png` format already. 

