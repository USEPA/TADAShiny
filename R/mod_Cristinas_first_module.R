#' Cristinas_first_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

list.of.packages <- c("plyr", "data.table", "dataRetrieval", "dplyr", "ggplot2", "grDevices", "magrittr", "stringr", "utils", "RColorBrewer", "stats", "remotes", "gganimate", "gifski", "maps")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(remotes)
remotes::install_github("USEPA/TADA", dependencies=TRUE)

library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(grDevices)
library(magrittr)
library(stringr)
library(utils)
library(RColorBrewer)
library(stats)
library(rlang)
library(dataRetrieval)
library(gganimate)
library(gifski)
library(maps)
library(TADA)

#sample_data_1 = utils::read.csv(file = "TADAShiny/sample_data/TADAProfile_UT_Nutrients.csv")

mod_Cristinas_first_module_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}

#' Cristinas_first_module Server Functions
#'
#' @noRd 
mod_Cristinas_first_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Cristinas_first_module_ui("Cristinas_first_module_1")
    
## To be copied in the server
# mod_Cristinas_first_module_server("Cristinas_first_module_1")
