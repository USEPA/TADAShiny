#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @noRd
#' 

# Below increases the max data upload size from the Shiny default of 5MB per file
# to 30MB for file
options(shiny.maxRequestSize=30*1024^2)

app_server <- function(input, output, session) {
  # Your application server logic
  tadat = shiny::reactiveValues() # create a list object that holds reactive values passed between modules
  mod_query_data_server("query_data_1", tadat) # server call to the module servers with the name of the module and any dependecies (this one uses the tadat reactive values object)
  mod_data_flagging_server("data_flagging_1", tadat)
  mod_summary_server("summary_1", tadat)
  mod_overview_server("overview_1", tadat)
  
  observeEvent(tadat$overview==1, {
    appendTab(inputId="tabbar",tab=tabPanel("Overview",
                                                     br(),
                                                     mod_overview_ui("overview_1"),
                                                     hr(),
                                                     mod_summary_ui("summary_1"),
                                                     hr(),
                                                     mod_TADA_summary_ui("TADA_summary_1")))
  })
}
