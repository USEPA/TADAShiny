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
  mod_filtering_server("filtering_1", tadat)
  mod_query_data_server("query_data_1", tadat) # server call to the module servers with the name of the module and any dependencies (this one uses the tadat reactive values object)
  mod_data_flagging_server("data_flagging_1", tadat)
  mod_summary_server("summary_1", tadat)
  mod_overview_server("overview_1", tadat)
  mod_censored_data_server("censored_data_1", tadat)
  mod_review_data_server("review_data_1", tadat)
  mod_figures_server("figures_1", tadat)
  mod_TADA_summary_server("TADA_summary_1", tadat)
  
  # this ensures all tabs except Upload are disabled upon app start
  shinyjs::disable(selector = '.nav li a[data-value="Overview"]')
  shinyjs::disable(selector = '.nav li a[data-value="Flag"]')
  shinyjs::disable(selector = '.nav li a[data-value="Filter"]')
  shinyjs::disable(selector = '.nav li a[data-value="Censored"]')
  shinyjs::disable(selector = '.nav li a[data-value="Figures"]')
  shinyjs::disable(selector = '.nav li a[data-value="Review"]')
  
  shiny::showModal(shiny::modalDialog(title = "Disclaimer",
                                      "This United States Environmental Protection Agency (EPA) GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government."))
  
  # switch to overview tab when tadat$new changes and provide user with window letting them know how many records were automatically flagged for removal upon upload
  # move this to query_data?
  shiny::observeEvent(tadat$new,{
      shiny::showModal(shiny::modalDialog(
        title = "Data Loaded",
        "Your data were successfully loaded into the app and are displayed on the Overview tab. See summary information about your dataset in the gray box at the bottom of the app."
      ))
      shiny::updateTabsetPanel(session=session, inputId="tabbar", selected="Overview")
      tadat$new = NULL
  })
  
  # update the master 'Remove' column anytime data is added to the 'remove' table
  shiny::observeEvent(tadat$removals,{
    if(dim(tadat$removals)[2]>0){
      tadat$raw$TADA.Remove = apply(tadat$removals, 1, any)
    }
  })
  
  # this observes when the user switches tabs and adds the current tab they're on as a column to their dataset. 
  shiny::observe({
    shiny::req(tadat$raw)
    tadat$raw$TADAShiny.tab = input$tabbar
    tadat$tab = input$tabbar
  })
  
  # switch to tab user left off on when tadat$reup changes, which only happens when someone uploads a workbook with the column "Removed" in it
  shiny::observeEvent(tadat$reup,{
    shiny::showModal(shiny::modalDialog(
      title = "Data Loaded",
      "Your working dataset has been uploaded and the app switched to the tab where you left off."
    ))
    # the switch tab command
    shiny::updateTabsetPanel(session=session, inputId="tabbar", selected=unique(tadat$raw$tab))
    tadat$reup = NULL
  })
  

}
