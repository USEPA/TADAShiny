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

  shiny::observeEvent(tadat$raw,{
    shiny::showModal(shiny::modalDialog(
      title = "Data Loaded",
      "Your data were successfully loaded into the app. On the Overview page, explore site information using the map, hover over the pieces of the pie chart for more information on characteristics, and review the summary tables below."
    ))
    shiny::updateTabsetPanel(session=session, inputId="tabbar", selected="Overview")
  })
}
