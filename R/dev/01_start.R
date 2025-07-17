# Building a Prod-Ready, Robust Shiny Application.

# Each step within each of the dev files is optional, and you don't have to
# fill them out before getting started.
# 01_start.R should only be used when first creating an app.
# 02_dev.R should be used to keep track of your development process.
# 03_maintenance.R should be used to test and maintain the app before deployment.
# 04_deploy.R should be used to document your deployment process.

########################################
#### CURRENT FILE: START SCRIPT ########
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R
golem::fill_desc(
  pkg_name = "TADAShiny", # The Name of the package containing the App
  pkg_title = "TADAShiny: Data Discovery and Cleaning", # The Title of the package containing the App
  pkg_description = "Assists data partners in retrieving, wrangling, quality checking, and harmonizing data from the Water Quality Portal for subsequent analyses.", # The Description of the package containing the App
  authors = c(
    person(
      given = "Cristina",
      family = "Mullin",
      role = c("aut", "cre"),
      email = "mywaterway@epa.gov",
      comment = c(ORCID = "0000-0002-0615-6087")
    ),
    person(
      given = "Trip",
      family = "Hook",
      role = "aut"
    ),
    person(
      given = "Elise",
      family = "Hinman",
      role = "aut"
    )
  ),
  repo_url = "https://github.com/USEPA/TADAShiny" # The URL of the GitHub Repo (optional)
)

## Set {golem} options ----
# golem::set_golem_options()

## Create Common Files ----
## See ?usethis for more information
# usethis::use_cc0_license()  # You can set another license here
# usethis::use_readme_rmd( open = FALSE )
# usethis::use_code_of_conduct("mullin.cristina@epa.gov")
# usethis::use_lifecycle_badge( "Experimental" )
# usethis::use_news_md( open = FALSE )

## Use git ----
# usethis::use_git()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# # If you want to change the favicon (default is golem's one)
# golem::use_favicon() # path = "path/to/ico". Can be an online file.
# golem::remove_favicon()

## Add helper functions ----
# golem::use_utils_ui()
# golem::use_utils_server()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
