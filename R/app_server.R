#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# Below increases the max data upload size from the Shiny default of 5MB per file
# to 30MB for file
options(shiny.maxRequestSize=30*1024^2)

app_server <- function(input, output, session) {
  # Your application server logic
  tadat = reactiveValues()
  mod_query_data_server("query_data_1", tadat)
  mod_upload_data_server("upload_data_1", tadat)
  mod_summary_server("summary_1", tadat)
}
