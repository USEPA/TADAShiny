
<!-- README.md is generated from README.Rmd. Please edit that file. 
You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.-->

# TADAShiny

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/USEPA/TADAShiny/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/USEPA/TADAShiny?branch=develop)
[![R-CMD-check](https://github.com/USEPA/TADAShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/USEPA/TADAShiny/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

TADAShiny provides a user interface 
(https://owshiny-dev.app.cloud.gov/tada-dev/) on top of the TADAPackage
(<https://github.com/USEPA/TADA>). This application can be used to
compile and evaluate Water Quality Portal (WQP) data for samples
collected from surface water monitoring sites on streams and lakes.

In 2012, the WQP was deployed by the U.S. Geological Survey (USGS), 
the U.S. Environmental Protection Agency (USEPA), and the National 
Water Quality Monitoring Council to combine and serve water-quality 
data from numerous sources in a standardized format. The WQP holds 
over 420 million water quality sample results from over 1000 federal,
state, tribal and other partners, and is the nation's largest source
for single point of access for water-quality data. Participating 
organizations submit their data to the WQP using the EPA's Water 
Quality Exchange (WQX), a framework designed to map their data 
holdings to a common data structure.

TADAShiny (Module 1: Data Discovery and Cleaning) retrieves data from
the WQP, and runs it through a series of data wrangling, cleaning, and
quality control checks. Features include flagging invalid results and
metadata using validation reference tables, harmonization of synonyms,
result and depth unit conversions, censored data substitutions, dataset
filtering, and data visualizations. Users will be able to review and 
download summary information about their dataset, along with a data file
and that is ready for additional manual review and use in subsequent 
analyses. Users make all decisions using the app to flag data for removal
or keep data depending on its quality and relevance for their analysis.

## Installation

You can install the development version of TADAShiny from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("USEPA/TADAShiny")
```

## Disclaimer

This United States Environmental Protection Agency (EPA) GitHub project
code is provided on an “as is” basis and the user assumes responsibility
for its use. EPA has relinquished control of the information and no
longer has responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by EPA. The EPA seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by EPA or the United States Government.
