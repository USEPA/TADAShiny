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
    htmltools::div(style = "margin-bottom:10px"),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(ns("runFlags"),
                          "Run Data Flagging",
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )),
    htmltools::br(),
    shiny::htmlOutput(ns('step_2')),
    htmltools::div(style = "margin-bottom:10px"),
    DT::DTOutput(ns('flagTable')),
    htmltools::br(),
    shiny::htmlOutput(ns('step_3')),
    shiny::fluidRow(column(6, shiny::uiOutput(ns('m2f'))))
  )
}

mod_data_flagging_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$step_1 = shiny::renderUI(
      HTML(
        "<h3>Click the 'Run Data Flagging' button to scan the dataset for
        potential quality control issues</h3>"
      )
    )
    
    flagSwitch = function(len) {
      inputs = character(len)
      for (i in seq_len(len)) {
        switch_name <- paste0("switch_", i)
        inputs[i] = as.character(
          shinyWidgets::prettySwitch(
            ns(switch_name),
            label = NULL,
            value = switch_defaults[i],
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
    values$selected_flags = character()
    
    # Runs when the flag button is clicked
    shiny::observeEvent(input$runFlags, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Flagging invalid or out-of-range data...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      # Add flagging columns to raw table
      tadat$raw = applyFlags(tadat$raw)
      # write.csv(tadat$raw, "flagged.csv")
      # tadat$raw = utils::read.csv("flagged.csv")
      
      # A table (raw rows, flags) indicating whether each record passes each test
      values$testResults <- flagCensus(tadat$raw)
      
      # The number of records failing each test
      values$n_fails <- colSums(values$testResults)
      
      # Remove progress bar and display instructions
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      output$step_2 = shiny::renderUI(HTML("<h3> Review quality control (QC) test results below. Each row describes the QC test, reports the number of results that failed the test, and contains a switch the user may toggle on/off to decide whether to flag results for removal.</h3>"))
      
      
      # Runs when any of the flag switches are changed
      shiny::observe({
        switch_id = "switch_"
        values$selected_flags = flag_types[shinyValue(switch_id, n_switches)]
        for (i in which(switch_disabled)) {
          shinyjs::disable(paste0(switch_id, i))
        }
      })
      
      shiny::observeEvent(values$selected_flags, {
        prefix = "Flag: "
        tadat$removals = dplyr::select(tadat$removals, -starts_with(prefix))
        for (flag in values$selected_flags) {
          if (!all(is.na(values$testResults[flag]))) {
            tadat$removals[paste0(prefix, flag)] = values$testResults[flag]
          }
        }
      })
      
      switchTable = shiny::reactive({
        df = data.frame(
          Reason = prompts,
          Results = values$n_fails,
          Required = levs,
          Decision = flagSwitch(n_switches)
        )
      })
      
      output$flagTable = DT::renderDT(
        shiny::isolate(switchTable()),
        escape = FALSE,
        selection = 'none',
        colnames = c(
          "Flag reason",
          "Results affected",
          "Required/Optional",
          "Switch 'on' to flag for removal"
        ),
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
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
    
    output$step_3 = shiny::renderUI({
      shiny::req(values$testResults)
      HTML("<h3>Convert depth units (Optional)</h3>")
    })
    
    output$m2f <- shiny::renderUI({
      shiny::req(values$testResults)
      shiny::radioButtons(ns('m2f'), label = "Result depth units are currently in meters. Click the radio buttons below to convert depth units to feet, inches, or back to meters.", choices = c("feet","inches","meters"), selected = character(0), inline = TRUE)
      
    })
    
    shiny::observeEvent(input$m2f,{
      shiny::req(input$m2f)
      if(input$m2f=="feet"){
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to feet...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw = TADA::TADA_ConvertDepthUnits(tadat$raw, unit = "ft")
      }
      if(input$m2f=="inches"){
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to inches...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw = TADA::TADA_ConvertDepthUnits(tadat$raw, unit = "in")
      }
      if(input$m2f=="meters"){
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to meters...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw = TADA::TADA_ConvertDepthUnits(tadat$raw, unit = "m")
      }
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
    })
  })
}
