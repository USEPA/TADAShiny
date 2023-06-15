#' review_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_review_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' review_data Server Functions
#'
#' @noRd 
mod_review_data_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_review_data_ui("review_data_1")
    
## To be copied in the server
# mod_review_data_server("review_data_1")
