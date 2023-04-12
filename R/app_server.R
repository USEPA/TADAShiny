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
  mod_censored_data_server("censored_data_1", tadat)
  mod_TADA_summary_server("TADA_summary_1", tadat)
  
  # switch to overview tab when tadat$new changes
  shiny::observeEvent(tadat$new,{
    removed = length(tadat$raw$ResultIdentifier[tadat$raw$Removed==TRUE])
    if(removed>0){
      message = paste0("Your data were successfully loaded and displayed on the Overview tab. ", scales::comma(removed)," results were flagged for removal because their sample media was not WATER or the result value was text or NA and no detection limit value was provided. See dataset summary information in the gray box at the bottom of the app.")
    }else{
      message = "Your data were successfully loaded into the app and are displayed on the Overview tab. See summary information about your dataset in the gray box at the bottom of the app."
    }
      shiny::showModal(shiny::modalDialog(
        title = "Data Loaded",
        message
      ))
      shiny::updateTabsetPanel(session=session, inputId="tabbar", selected="Overview")
      tadat$new = NULL
  })
  
  # switch to tab user left off on when tadat$reup changes
  shiny::observeEvent(tadat$reup,{
    shiny::showModal(shiny::modalDialog(
      title = "Data Loaded",
      "Your working dataset has been uploaded and the app switched to the tab where you left off."
    ))
    shiny::updateTabsetPanel(session=session, inputId="tabbar", selected=unique(tadat$raw$tab))
    tadat$reup = NULL
  })
  
  observe({
    shiny::req(tadat$raw)
    tadat$raw$tab = input$tabbar
    tadat$tab = input$tabbar
  })

}
