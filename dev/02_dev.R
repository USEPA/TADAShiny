# Building a Prod-Ready, Robust Shiny Application.

# Each step within each of the dev files is optional, and you don't have to
# fill them out before getting started.
# 01_start.R should only be used when first creating an app.
# 02_dev.R should be used to keep track of your development process.
# 03_maintenance.R should be used to test and maintain the app before deployment.
# 04_deploy.R should be used to document your deployment process.

########################################
#### CURRENT FILE: DEV SCRIPT ##########
########################################

# usethis::use_cc0_license()
# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
# usethis::use_package( "thinkr" )
# usethis::use_package("shiny")
# usethis::use_package("shinycssloaders")
# usethis::use_package("dplyr")
# usethis::use_package("xlsx")
# usethis::use_package("readr")
# usethis::use_pipe()
# usethis::use_package("bootstrap")
# usethis::use_package("stringr")
# usethis::use_package("NADA")
# usethis::use_package("readxl")
# usethis::use_package("writexl")
# usethis::use_package("DT")
# usethis::use_package("EnvStats")
# usethis::use_package("shinyjs")
# usethis::use_package("ggplot2")
# usethis::use_package("leaflet")
# usethis::use_package("sf")
# usethis::use_package("shinybusy")
# usethis::use_package("shinyWidgets")
# usethis::use_package("forcats")
# usethis::use_package("TADA")
# usethis::use_package("scales")
# usethis::use_package("combinat")
# usethis::use_package("EPATADA")
# usethis::use_package("MESS")
# usethis::use_package("covr", type = "Suggests")

## Add modules ----
## Create a module infrastructure in R/
# golem::add_module(name = "name_of_module1") # Name of the module
# golem::add_module(name = "import") # Name of the import module
# golem::add_module(name = "query_data", with_test = TRUE) # Name of the module
# golem::add_module(name = "overview", with_test = TRUE) # Name of the module
# golem::add_module(name = "upload_data", with_test = TRUE) # Name of the module
# golem::add_module(name = "summary", with_test = TRUE) # Name of the module
# golem::add_module(name = "TADA_summary") # Name of the module
# golem::add_module(name = "censored_data")
# golem::add_module(name = "map_bbox")

# Add helper functions ----
# Creates fct_* and utils_*
# golem::add_fct( "helpers" )
# golem::add_utils( "helpers" )
# golem::add_fct("map")


## External resources
## Creates .js and .css files at inst/app/www
# golem::add_js_file( "script" )
# golem::add_js_handler( "handlers" )
# golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
# usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests ----
## Add one line by test you want to create
# usethis::use_test( "app" )

# Documentation

## Vignette ----
# usethis::use_vignette("TADAShiny")
# devtools::build_vignettes()

## We already have this as a github workflow
## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
# usethis::use_coverage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
#
# usethis::use_github()
#
# # GitHub Actions
# usethis::use_github_action()
# # Chose one of the two
# # See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_release()
# usethis::use_github_action_check_standard()
# # usethis::use_github_action_check_full()
# # Add action for PR
# usethis::use_github_action_pr_commands()
#
# # Travis CI
# usethis::use_travis()
# usethis::use_travis_badge()
#
# # AppVeyor
# usethis::use_appveyor()
# usethis::use_appveyor_badge()
#
# # Circle CI
# usethis::use_circleci()
# usethis::use_circleci_badge()
#
# # Jenkins
# usethis::use_jenkins()
#
# # GitLab CI
# usethis::use_gitlab_ci()
#
# # You're now set! ----
# # go to dev/03_maintenance.R
rstudioapi::navigateToFile("dev/03_maintenance.R")
