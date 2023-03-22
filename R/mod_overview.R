#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyBS::bsCollapsePanel("Data Overview",
                             "The map, tables, and plots below are built using the uploaded/queried data. Use the drop down menu below to select one parameter of interest or use choice 'All' to see a summary for all parameters together.",
                             fluidRow(column(6, uiOutput(ns("overview_select")))),
                             fluidRow(leaflet::leafletOutput(ns("overview_map")))
                             )
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(tadat$raw)
      tadat$o_char <- unique(tadat$raw$TADA.CharacteristicName)
    })
    
    output$overview_select = renderUI({
      req(tadat$o_char)
      selectInput("overview_select","Select Parameter", choices = c("All", tadat$o_char), selected = "All")
    })
    
    output$overview_map = leaflet::renderLeaflet({
      req(tadat$raw)
      
    })
 
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
