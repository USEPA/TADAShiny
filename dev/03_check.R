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

# Check renv lock file
renv::status()
# See `?renv::status` for advice on resolving issues

## Run checks ----
## Check the package before sending to prod
## Run devtools test, check and build
devtools::document() # make sure all dependencies are listed in description file
devtools::test()
# devtools::build()
devtools::check()
