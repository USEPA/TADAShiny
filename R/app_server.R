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
  
  TADA_Profile <- reactiveVal(NULL)
  
  modImportTADAProfile <- mod_upload_TADAprofile_server("upload_TADAprofile_1", TADA_Profile)
  
}
