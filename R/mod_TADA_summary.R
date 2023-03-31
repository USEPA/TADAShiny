#' TADA_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TADA_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::h3("TADA Working Summary:")

  )
}

#' TADA_summary Server Functions
#'
#' @noRd
mod_TADA_summary_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")

## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")
