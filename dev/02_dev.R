# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################
usethis::use_cc0_license()
# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package("shiny")
usethis::use_package("shinycssloaders")
usethis::use_package("dplyr")
usethis::use_package("xlsx")
usethis::use_package("readr")
usethis::use_pipe()
usethis::use_package("bootstrap")
usethis::use_package("stringr")
usethis::use_package("NADA")
usethis::use_package("readxl")
usethis::use_package("DT")
usethis::use_package("EnvStats")
usethis::use_package("shinyjs")
usethis::use_package("ggplot2")
usethis::use_package("leaflet")
usethis::use_package("sf")
usethis::use_package("shinybusy")
usethis::use_package("shinyWidgets")
usethis::use_package("plotly")
usethis::use_package("TADA")

## Add modules ----
## Create a module infrastructure in R/
## golem::add_module( name = "name_of_module1" ) # Name of the module
## golem::add_module( name = "import" ) # Name of the import module
# golem::add_module(name = "query_data", with_test = TRUE) # Name of the module
# golem::add_module(name = "overview", with_test = TRUE) # Name of the module
# golem::add_module(name = "upload_data", with_test = TRUE) # Name of the module
# golem::add_module(name = "summary", with_test = TRUE) # Name of the module
# golem::add_module(name = "TADA_summary") # Name of the module

## Add helper functions ----
## Creates fct_* and utils_*
##golem::add_fct( "helpers" )
##golem::add_utils( "helpers" )


## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("TADAShiny")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
# Not available on CRAN
# remotes::install_github('yonicd/covrpage')
# covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)

usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the two
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
# usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

