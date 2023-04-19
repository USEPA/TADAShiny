#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_data_flagging_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(style = "display: none;",
             shinyWidgets::prettySwitch("dummy", label = NULL)),
    shiny::htmlOutput(ns('step_1')),
    htmltools::br(),
    shiny::fluidRow(column(
      3, shiny::actionButton(ns("runFlags"), "Run Data Flagging", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )),
    shiny::htmlOutput(ns('step_2')),
    htmltools::br(),
    DT::DTOutput(ns('flagTable')),
    htmltools::br(),
    shiny::htmlOutput(ns('step_3'))
    #DT::DTOutput(ns('summaryTable'))
  )
}

mod_data_flagging_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$step_1 = shiny::renderUI(
      HTML(
        "<h4>Click the button below to scan the dataset for
        potential missing or out-of-range data</h3>"
      )
    )
    
    flagSwitch = function(len, id, value) {
      if (length(value) == 1) {
        value <- rep(value, len)
      }
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(
          shinyWidgets::prettySwitch(
            ns(paste0(id, i)),
            label = NULL,
            value = value[i],
            status = "primary",
            fill = TRUE
          )
        )
      }
      inputs
    }
    
    shinyValue = function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value))
          TRUE
        else
          value
      }))
    }
    
    # Create a separate column in the raw data to indicate whether records
    # were excluded during the first step
    values = shiny::reactiveValues()
    values$n_fails = integer(length(n_switches))
    
    # Runs when the flag button is clicked
    shiny::observeEvent(input$runFlags, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Flagging invalid or out-of-range data...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      # The raw table, plus flagging columns (jch - just return flagging?)
      values$flagged = applyFlags(tadat$raw)
      
      # Record which records have already been flagged
      AlreadyRemoved = tadat$raw$Removed
      
      # A table (raw rows, flags) indicating whether each record passes each test
      values$testResults <- flagCensus(values$flagged)
      
      # The number of records failing each test
      values$n_fails <- colSums(values$testResults)
      
      # The site id for each record (used for counting sites)
      values$sites = values$flagged$MonitoringLocationIdentifier
      
      # Remove progress bar and display instructions
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      output$step_2 = shiny::renderUI(HTML("<h4>Select the types of flagged data to be removed</h3>"))
      # output$step_3 = shiny::renderUI(HTML("Summary of data to be removed"))

    # Runs when any of the flag switches are changed
    shiny::observe({
      # A list of all the flags that are selected
      selected = flag_types[shinyValue('switch_', n_switches)]
      # Update which rows get removed with the selected
      NewRemovals = apply(values$testResults[selected], 1, any)
      NewRemovals[is.na(NewRemovals)] <- FALSE
      tadat$raw$Removed = as.logical(AlreadyRemoved + NewRemovals)
    })
    
    switchTable = shiny::reactive({
      df = data.frame(
        Prompt = prompts,
        Count = values$n_fails,
        Remove = flagSwitch(n_switches, 'switch_', FALSE)
      )
    })
    
    output$flagTable = DT::renderDT(
      shiny::isolate(switchTable()),
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = TRUE,
        ordering = FALSE,
        preDrawCallback = DT::JS(
          'function() { Shiny.unbindAll(this.api().table().node()); }'
        ),
        drawCallback = DT::JS(
          'function() { Shiny.bindAll(this.api().table().node()); } '
        )
      )
    )
    })
    
    
  })
}
