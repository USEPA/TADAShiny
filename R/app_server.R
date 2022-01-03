#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  epc_data <- reactiveVal(NULL)
  
  # Get the reactive value from the import server
  modImportReactiveVals <- mod_import_server("import_ui_1", epc_data)
  upload_error <- mod_import_check_server("import_check", modImportReactiveVals)
  
  # summary table when user uploads data
  mod_import_summary_server("import_summary_ui_1", modImportReactiveVals, upload_error)
  
}
