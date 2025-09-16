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

# No longer using RENV, using packrat instead, see 04_deploy.R
# # Check renv lock file ----
# renv::status()
# # See `?renv::status` for advice on resolving issues
# # If you have multiple packages in an inconsistent state, we recommend
# # renv::restore(), then renv::install(), then renv::snapshot(),
# # but that also suggests you should be running status more frequently.

##############

# Update extdata/query_choices.Rdata for WQP ML drop down mod_query_data.R
# Define the URL of the web service
url <- "https://www.waterqualitydata.us/data/Station/search?mimeType=csv&zip=no"
# Use httr::GET to make a GET request to the web service
response <- httr::GET(url)
# Use httr::content to parse the CSV content from the response
csv_content <- httr::content(response, "text")
# Use base::read.csv to read the CSV content into a data frame
data <- read.csv(text = csv_content, stringsAsFactors = FALSE)
# Extract unique monitoring location identifiers
mlids <- unique(data$MonitoringLocationIdentifier)
# Save the unique monitoring location identifiers
save(mlids, file = "inst/extdata/query_choices.Rdata")

##############

# Spell check
spelling::spell_check_package(
  pkg = ".",
  vignettes = TRUE
)
# Run to update spelling word list
spelling::get_wordlist()
spelling::update_wordlist()

##############

# Run styler to style code with default
# https://style.tidyverse.org/
# https://styler.r-lib.org/reference/style_pkg.html
styler::style_pkg()

##############

## Run checks ----
# Check the package before sending to prod
# Run devtools load_all, document, test, check and build
devtools::load_all()
devtools::document() # make sure all dependencies are listed in description file
# This may not be working correctly
# devtools::test()
# More robust test for releases (includes broken link check)
devtools::check(manual = FALSE, remote = TRUE, incoming = TRUE)
# Not always necessary to run build
# devtools::build()

##############

# You're now set! ----
# Go to dev/04_maintenance.R
rstudioapi::navigateToFile("R/dev/04_deploy.R")
