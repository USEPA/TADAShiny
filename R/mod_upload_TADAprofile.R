#' Upload TADAProfile Function
#'
#' @description A shiny Module to upload a TADA profile.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList


mod_upload_TADAprofile_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # widget to upload WQP profile or WQX formatted spreadsheet
    fileInput(
      ns("file"), "",
      multiple = TRUE,
      accept = c(".xlsx", ".xls", ".csv"),
      width = "100%"
    )
 
  )
}

#' upload_TADAprofile Server Functions
#'
#' @noRd 
mod_upload_TADAprofile_server <- function(id, TADA_Profile){
  moduleServer( id, function(input, output, session){
    
    # reactive file path
    input_filepath_set <- reactive({
      
      TADA_Profile(NULL) # reset user input
      req(input$file$datapath) # require data uploaded to run
      
      # user uploaded data
      uploaded_data <- readxl::read_excel(input$file$datapath, sheet = 1) 
      
      return(uploaded_data)
      
    })
    
    # Return the reactive value
    return(input_filepath_set)
  }
  )
}

    
## To be copied in the UI
# mod_upload_TADAprofile_ui("Cristinas_first_module_1")
    
## To be copied in the server
# mod_upload_TADAprofile_server("Cristinas_first_module_1")
