#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# Below increases the max data upload size from the Shiny default of 5MB per file
# to 30MB for file
options(shiny.maxRequestSize=30*1024^2)

app_server <- function( input, output, session ) {
  
  # Your application server logic 
  
  .data <- reactiveVal(NULL)
  
  modImportReactiveVals <- mod_uploadData_server("uploadData_1", .data)
  
  # summarize imported data  
  mod_summarizeData_server("summarizeData_1", modImportReactiveVals)
  
}
