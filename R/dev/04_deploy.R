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

# This is how to add the shiny sever file needed for any deployment
# golem::add_shinyserver_file() # already exists

# This is how to setup deployment to EPA's Posit Connect
# golem::add_positconnect_file() # already exists see rsconnect folder

# This is how to setup deployment to TetraTech's shinyappsio
# golem::add_shinyappsio_file() # already exists see rsconnect folder

# This is how to deploy, works for both TT shinyappsio and EPA posit connect
# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# Document and reload your package
golem::document_and_reload()
# Use packrat
# options(rsconnect.packrat = TRUE) # already done
# Deploy app to staging
# https://rstudio-connect.dmap-stage.aws.epa.gov/content/ca684b5d-fa77-4ac3-aacf-966b92d84e13/
rsconnect::deployApp(
  appDir = getwd(),
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/"),
  appName = "TADAShiny",
  appTitle = "TADAShiny Module 1 WQP Data Discovery and Cleaning",
  launch.browser = TRUE,
  forceUpdate = TRUE,
  appId = 1024
)

# To deploy to EPA posit connect production (public)
# We must reach out to the DMAP team
# rconnect-public.epa.gov/TADAShiny/
