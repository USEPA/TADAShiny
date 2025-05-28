# Deploy

## Local, CRAN or Package Manager ---- 
## This will build a tar.gz that can be installed locally, 
## sent to CRAN, or to a package manager

## RStudio ----
## If you want to deploy on RStudio related platforms
golem::add_positconnect_file()
# golem::add_shinyappsio_file()
# golem::add_shinyserver_file()

## If you want to build github pages
# usethis::use_pkgdown() # run once to configure your package
# pkgdown::build_site() 