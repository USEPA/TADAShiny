# Building a Prod-Ready, Robust Shiny Application.

# Each step within each of the dev files is optional, and you don't have to
# fill them out before getting started.
# 01_start.R should only be used when first creating an app.
# 02_dev.R should be used to keep track of your development process.
# 03_maintenance.R should be used to test and maintain the app before deployment.
# 04_deploy.R should be used to document your deployment process.

##############

# Update extdata files, see make_extdata.R

##############

# spell check
library(spelling)
spelling::spell_check_package(pkg = ".", vignettes = TRUE)
spelling::get_wordlist()
# # run to update spelling word list
# spelling::update_wordlist() # do not run until after checking wordlist & fixing spelling issues!

##############

library(styler)
style_pkg(
  transformers = tidyverse_style(
    scope = I(c("tokens", "indention")),
    indent_by = 2,
    strict = FALSE # turn off aggressive alignment
  )
)

##############

library(devtools)
# Run devtools check and test
devtools::test()
# devtools::check()
# more robust test for releases (includes broken link check)
devtools::check(manual = FALSE, remote = TRUE, incoming = TRUE)

##############

# You're now set! ----
# Go to dev/04_maintenance.R
rstudioapi::navigateToFile("R/dev/04_deploy.R")
