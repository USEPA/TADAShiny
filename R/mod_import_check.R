#' import_check UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_check_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    htmlOutput(ns("warning_message"))
    
  )
}

#' import_check Server Functions
#'
#' @noRd 
mod_import_check_server <- function(id, input_filepath){
  
  moduleServer(
    id,
    function(input, output, session){
      
      import_error <- reactiveVal("")
      
      output$warning_message <- renderUI({
        
        if (!is.null(input_filepath())) {
          # test if the uploaded data is in the right format
          excel_import_check <- import_error_checking(input_filepath())
          
          if (excel_import_check == ""){
            import_error("")
            return(NULL)
            
          } else {
            import_error("Error")
            HTML(paste0("<div class=warningmessage><h3>Upload Error:</h3>", excel_import_check, "<i><b>Please fix above errors and re-import your data to continue.</b></i>", "</div>"))
            
            
          }
        }
        
      })
      
      return(import_error)
      
      
      
    }
  )
}
