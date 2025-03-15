# Building a Prod-Ready, Robust Shiny Application.

# Each step within each of the dev files is optional, and you don't have to 
# fill them out before getting started. 
# 01_start.R should only be used when first creating an app. 
# 02_dev.R should be used to keep track of your development process.
# 03_deploy.R should be used to document your deployment process.

########################################
#### CURRENT FILE: DEPLOY SCRIPT #######
########################################

# Test your app

## Run checks ----
## Check the package before sending to prod
# devtools::check()

# Deploy

## Local, CRAN or Package Manager ---- 
## This will build a tar.gz that can be installed locally, 
## sent to CRAN, or to a package manager
# devtools::build()

# ## RStudio ----
# ## If you want to deploy on RStudio related platforms
golem::add_positconnect_file()
# golem::add_shinyappsio_file()
# golem::add_shinyserver_file()
# 
# ## Docker ----
# ## If you want to deploy via a generic Dockerfile
# golem::add_dockerfile()
# 
# ## If you want to deploy to ShinyProxy
# golem::add_dockerfile_shinyproxy()
# 
# ## If you want to deploy to Heroku
# golem::add_dockerfile_heroku()
# 
# ## If you want to build github pages
# usethis::use_pkgdown() # run once to configure your package
# pkgdown::build_site() 