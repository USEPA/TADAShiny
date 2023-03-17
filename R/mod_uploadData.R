#' uploadData UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_uploadData_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # widget to upload WQP profile or WQX formatted spreadsheet
    fileInput(
      ns("file"), "",
      multiple = TRUE,
      accept = ".xlsx",
      width = "100%"
    )
 
  )
}
    
#' uploadData Server Functions
#'
#' @noRd 
mod_uploadData_server <- function(id, .data){
  moduleServer( id, function(input, output, session){
    
    # reactive file path
    input_filepath_set <- reactive({
      
      .data(NULL) # reset user input
      req(input$file$datapath) # require data uploaded to run
      
      # user uploaded data
      uploaded_data <- readxl::read_excel(input$file$datapath, sheet = 1)
      
      return(uploaded_data)
      
    })
    
    # Return the reactive value
    return(input_filepath_set)
    
  })
}
    
## To be copied in the UI
# mod_uploadData_ui("uploadData_1")
    
## To be copied in the server
# mod_uploadData_server("uploadData_1")
