#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  TADA_Profile <- reactiveVal(NULL)
  
  modImportTADAProfile <- mod_upload_TADAprofile_server("upload_TADAprofile_1", TADA_Profile)
  
}
