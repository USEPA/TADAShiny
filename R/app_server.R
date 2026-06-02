#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
#'

# Increase the max data upload size from the Shiny default of 5MB per file to 400MB
options(shiny.maxRequestSize = 400 * 1024^2)

# Increase the timeout duration to 3600 seconds (1 hour)
options(shiny.timeout = 3600)

# Do NOT change the global 'warn' setting here.
# If you need stricter warning handling during development or within a specific block,
# use scoped patterns such as:
# withr::with_options(list(warn = 2), { ... })
# This ensures warn is restored automatically and avoids inconsistent behavior.

app_server <- function(input, output, session) {
  # Create a reactiveValues object to hold shared data between modules
  tadat <- shiny::reactiveValues()

  # Initialize reactive values
  shiny::observeEvent(tadat$raw, {
    if (!is.null(tadat$raw) && is.null(tadat$removals)) {
      # Initialize removals with the same number of rows as raw data, all FALSE
      tadat$removals <- data.frame(matrix(
        FALSE,
        nrow = nrow(tadat$raw),
        ncol = 0
      ))
    }
  })

  # Update the master 'Remove' column anytime data is added to the 'removals' table
  shiny::observeEvent(tadat$removals, {
    if (dim(tadat$removals)[2] > 0) {
      # Ensure tadat$removals contains logical (TRUE/FALSE) values for each record
      tadat$raw$TADA.Remove <- apply(tadat$removals, 1, any)
      # # Debugging: Print the removals table and the resulting TADA.Remove column
      # print("Removals Table:")
      # print(tadat$removals)
      # print("Updated TADA.Remove:")
      # print(tadat$raw$TADA.Remove)
    }
  })

  # Module server calls
  mod_filtering_server("filtering_1", tadat)
  mod_query_data_server("query_data_1", tadat)
  mod_data_flagging_server("data_flagging_1", tadat)
  mod_summary_server("summary_1", tadat)
  mod_overview_server("overview_1", tadat)
  mod_censored_data_server("censored_data_1", tadat)
  mod_harmonize_np_server("harmonize_np_1", tadat)
  mod_depth_server("depth_1", tadat)
  mod_review_data_server("review_data_1", tadat)
  mod_figures_server("figures_1", tadat)
  mod_TADA_summary_server("TADA_summary_1", tadat)

  # Disable all tabs except Upload upon app start
  shinyjs::disable(selector = '.nav li a[data-value="Overview"]')
  shinyjs::disable(selector = '.nav li a[data-value="Flag"]')
  shinyjs::disable(selector = '.nav li a[data-value="Filter"]')
  shinyjs::disable(selector = '.nav li a[data-value="Censored"]')
  shinyjs::disable(selector = '.nav li a[data-value="Harmonize"]')
  shinyjs::disable(selector = '.nav li a[data-value="Depth"]')
  shinyjs::disable(selector = '.nav li a[data-value="Figures"]')
  shinyjs::disable(selector = '.nav li a[data-value="Review"]')

  # Initialize other reactive values and configurations
  tadat$load_progress_file <- NA
  tadat$save_progress_file <- NA
  tadat$flags_present <- FALSE
  job_id <- base::paste0("ts", format(Sys.time(), "%y%m%d%H%M%S"))
  tadat$default_outfile <- base::paste0("tada_output_", job_id)
  tadat$job_id <- job_id

  # Switch to overview tab when tadat$new changes and show a modal dialog
  shiny::observeEvent(tadat$new, {
    # browser()
    shiny::showModal(shiny::modalDialog(
      title = "Data Loaded",
      shiny::HTML(paste0(
        "A total of <strong>",
        scales::comma(length(tadat$raw$ResultIdentifier)),
        "</strong> results at <strong>",
        scales::comma(length(unique(tadat$raw$MonitoringLocationIdentifier))),
        "</strong> sites were successfully loaded into the app and are displayed on the Overview tab.
      The following data wrangling steps were performed automatically when data was loaded:
      <ol>
      <li> created TADA versions of a subset of columns for editing (originals are retained),
      <li> handled/flagged special characters and text in result values and units,
      <li> harmonized result and depth units to TADA defaults, and
      <li> replaced retired characteristic names with current names.
      </ol>
      See summary information about your dataset in the gray box at the bottom of the webpage."
      )),
      easyClose = TRUE
    ))
    shiny::updateTabsetPanel(
      session = session,
      inputId = "tabbar",
      selected = "Overview"
    )
    tadat$new <- NULL
  })

  shiny::observe({
    tadat$tab <- input$tabbar
  })

  # JCH - disabling this for now. I think progress files provide this functionality
  # this observes when the user switches tabs and adds the current tab they're on as a column to their dataset.

  # switch to tab user left off on when tadat$reup changes, which only happens when someone uploads a workbook with the column "Removed" in it
  # shiny::observeEvent(tadat$reup, {
  #  shiny::showModal(shiny::modalDialog(
  #    title = "Data Loaded",
  #    "Your working dataset has been uploaded and the app switched to the tab where you left off."
  #  ))
  #  # the switch tab command
  #  shiny::updateTabsetPanel(session = session, inputId = "tabbar", selected = unique(tadat$raw$tab))
  #  tadat$reup <- NULL
  # })
}
