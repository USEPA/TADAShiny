#' upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_upload_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Upload a WQP dataset"),
    "Select a pre-existing file from your computer. Currently supports .xls and .xlsx only. You can find the WQX profile templates ",
    tags$a(href="https://www.epa.gov/waterdata/water-quality-exchange-web-template-files", "here."),
    # widget to upload WQP profile or WQX formatted spreadsheet
    fileInput(
      ns("file"), "",
      multiple = TRUE,
      accept = c(".xlsx", ".xls"),
      width = "100%"
    )
  )
}

#' upload_data Server Functions
#'
#' @noRd
mod_upload_data_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    observe({
      req(input$file)
      # user uploaded data
      tadat$raw <- suppressWarnings(readxl::read_excel(input$file$datapath, sheet = 1))
    })

  })
}

## To be copied in the UI
# mod_upload_data_ui("upload_data_1")

## To be copied in the server
# mod_upload_data_server("upload_data_1")
