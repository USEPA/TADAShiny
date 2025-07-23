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

## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::check()
devtools::build()
pkgbuild::build()

## If you want to build github pages (NOT CURRENLY USED IN TADAShiny)
# pkgdown::build_site()
# usethis::use_pkgdown() # run once to configure your package

# # This is how to deploy to TetraTech's shinyappsio
# # see file produced here TADAShiny\rsconnect\shinyapps.io\tetratech-wtr-wne
# 5. Deploy to shinyapps.io
# golem::add_shinyserver_file() #already exists
rsconnect::deployApp(
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/"),
  appName = "TADAShiny",
  forceUpdate = TRUE
)

# This is how to deploy to EPA's Posit Connect
# golem::add_positconnect_file() # already exists
rsconnect::deployApp(
  appDir = getwd(),
  account = "Cristina",
  appFiles = c("app.R", "DESCRIPTION", "NAMESPACE", "R/", "inst/"),
  quarto = FALSE,
  server = "rstudio-connect.dmap-stage.aws.epa.gov",
  appName = "TADAShiny",
  appTitle = "TADAShiny",
  appId = 403,
  launch.browser = TRUE,
  lint = TRUE,
  metadata = list(asMultiple = FALSE, asStatic = FALSE),
  logLevel = "verbose",
  forceUpdate = TRUE
)
