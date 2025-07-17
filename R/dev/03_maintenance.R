# Building a Prod-Ready, Robust Shiny Application.

# Each step within each of the dev files is optional, and you don't have to
# fill them out before getting started.
# 01_start.R should only be used when first creating an app.
# 02_dev.R should be used to keep track of your development process.
# 03_maintenance.R should be used to test and maintain the app before deployment.
# 04_deploy.R should be used to document your deployment process.

#############################################
#### CURRENT FILE: MAINTENANCE SCRIPT #######
#############################################

# Check renv lock file ----
renv::status()
# See `?renv::status` for advice on resolving issues
# If you have multiple packages in an inconsistent state, we recommend
# renv::restore(), then renv::install(), then renv::snapshot(),
# but that also suggests you should be running status more frequently.

# spell check
spelling::spell_check_package(
  pkg = ".",
  vignettes = TRUE
)
# run to update spelling word list
spelling::get_wordlist()
spelling::update_wordlist()

# # Run styler to style code
# # https://style.tidyverse.org/
# # See: https://styler.r-lib.org/reference/style_pkg.html
# # Run the following with defaults
# library(styler)
# styler::style_pkg()

## Run checks ----
## Check the package before sending to prod
## Run devtools test, check and build
devtools::document() # make sure all dependencies are listed in description file
devtools::test()
# more robust test for releases (includes broken link check)
devtools::check(manual = FALSE, remote = TRUE, incoming = TRUE)
# devtools::build()

# # You're now set! ----
# # go to dev/04_maintenance.R
rstudioapi::navigateToFile("R/dev/04_deploy.R")
