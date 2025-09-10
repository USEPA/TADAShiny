# Building a Prod-Ready, Robust Shiny Application.

# Each step within each of the dev files is optional, and you don't have to
# fill them out before getting started.
# 01_start.R should only be used when first creating an app.
# 02_dev.R should be used to keep track of your development process.
# 03_maintenance.R should be used to test and maintain the app before deployment.
# 04_deploy.R should be used to document your deployment process.

########################################
#### CURRENT FILE: DEPLOY SCRIPT #######
########################################

# Deploy ----
## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
## RStudio ----
## If you want to deploy on RStudio related platforms

# golem::add_shinyserver_file() #already exists 

# # This is how to deploy to TetraTech's shinyappsio
# # along with the file produced here TADAShiny\rsconnect\shinyapps.io\tetratech-wtr-wne
# golem::add_shinyappsio_file()
# Deploy to shinyapps.io
# rsconnect::deployApp(
#   appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/"),
#   appName = "TADAShiny",
#   forceUpdate = TRUE
# )

# Updated method using packrat?
# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# Document and reload your package
golem::document_and_reload()
# This is how to deploy to EPA's Posit Connect
# golem::add_positconnect_file() # already exists
# run these (update path)
options(rsconnect.packrat = TRUE)
# rsconnect::deployApp("path/to/the/app")
rsconnect::deployApp(appDir = getwd())
