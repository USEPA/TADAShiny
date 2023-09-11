library(stringr)
library(rjson)
library(dplyr)

input_packs <- "TADA,config,golem,magrittr,htmltools,readxl,writexl,shiny,leaflet,shinyWidgets,shinycssloaders,DT,ggplot2,shinybusy,dplyr,plyr,scales,forcats,testthat,remotes,covr,rmarkdown,knitr,spelling,sf,shinyjs,stringr,shinyjqui"

refs <- strsplit(input_packs, split = ",")[[1]]

