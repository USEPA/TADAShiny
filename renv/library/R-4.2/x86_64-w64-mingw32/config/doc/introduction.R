## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(eval = TRUE, results = 'hide')
Sys.setenv(R_CONFIG_ACTIVE = "default")

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("config")

## ---- eval=FALSE--------------------------------------------------------------
#  config <- config::get()
#  config$trials
#  config$dataset

## ---- eval=FALSE--------------------------------------------------------------
#  config::get("trials")
#  config::get("dataset")

## ---- eval=FALSE--------------------------------------------------------------
#  # set the active configuration globally via Renviron.site or Rprofile.site
#  Sys.setenv(R_CONFIG_ACTIVE = "production")
#  
#  # read configuration value (will return 30 from the "production" config)
#  config::get("trials")

## ---- eval=FALSE--------------------------------------------------------------
#  config::is_active("production")

## ---- include=FALSE-----------------------------------------------------------
Sys.setenv(R_CONFIG_ACTIVE = "default")

## ---- eval=FALSE--------------------------------------------------------------
#  config <- config::get(file = "conf/config.yml")

## ---- eval=FALSE--------------------------------------------------------------
#  config <- config::get(file = "conf/config.yml", use_parent = FALSE)

