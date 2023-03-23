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
                             fluidRow(column(6, uiOutput(ns("overview_select")))), # note that this uiOutput is a widget that needs something from the server-side before rendering. It is a drop down menu.
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
    
    # an observer that creates a new reactive object vector called "o_char" that has only the characteristics contained within the tadat$raw dataset
    observe({
      req(tadat$raw)
      tadat$o_char <- unique(tadat$raw$TADA.CharacteristicName)
    })
    
    # o_char is used to parameterize this drop down select input that allows the user to map/plot one parameter at a time or all of them.
    output$overview_select = renderUI({
      req(tadat$o_char) # this is the drop down menu widget that you might be familiar seeing in the ui, but because it needs something from tadat$raw to create tadat$o_char before rendering, it goes in the server-side.
      selectInput("overview_select","Select Parameter", choices = c("All", tadat$o_char), selected = "All")
    })
    
    # the leaflet map
    output$overview_map = leaflet::renderLeaflet({
      req(tadat$raw)
      
    })
 
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
