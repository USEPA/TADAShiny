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
    htmlOutput(ns('step_1')),
    "Here",
    fluidRow(column(
      3, actionButton(ns("runFlags"), "Run Data Flagging")
    )),
    htmlOutput(ns('step_2')),
    DT::DTOutput(ns('flagTable')),
    br(),
    htmlOutput(ns('step_3')),
    DT::DTOutput(ns('summaryTable'))
  )
}

mod_data_flagging_server <- function(id, tadat) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$step_1 = renderUI(
      HTML(
        "<h3>Step 1</h3> Click the button below to scan the dataset for
        potential missing or out-of-range data"
      )
    )
    
    flagSwitch = function(len, id, value) {
      if (length(value) == 1) {
        value <- rep(value, len)
      }
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(prettySwitch(
          ns(paste0(id, i)),
          label = NULL,
          value = value[i],
          status = "primary",
          fill = TRUE
        ))
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
    values = shiny::reactiveValues()
    values$census = integer(length(n_switches))

    # 
    observeEvent(input$runFlags, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Flagging invalid or out-of-range data...",
        session = shiny::getDefaultReactiveDomain()
      )
      flagged = applyFlags(tadat$raw)
      values$censusTable <- flagCensus(flagged)
      values$census <- colSums(values$censusTable)
      values$sites = flagged$MonitoringLocationIdentifier
      shinybusy::remove_modal_spinner(session = getDefaultReactiveDomain())
      output$step_2 = renderUI(HTML(
        "<h3>Step 2</h3>Select the types of flagged data to be removed"
      ))
      output$step_3 = renderUI(HTML(
        "<h3>Summary of data to be removed"
      ))
      
      observe({
        values$summaryTable = getCounts(
          values$sites, 
          values$censusTable[flag_types[shinyValue('switch_', n_switches)]])
      })
      
      switchTable = reactive({
        df = data.frame(
          Prompt = prompts,
          Count = values$census,
          Remove = flagSwitch(n_switches, 'switch_', FALSE)
        )
      })
      
      output$flagTable = DT::renderDT(
        isolate(switchTable()),
        escape = FALSE,
        selection = 'none',
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = TRUE,
          ordering = FALSE,
          preDrawCallback = JS(
            'function() { Shiny.unbindAll(this.api().table().node()); }'
          ),
          drawCallback = JS(
            'function() { Shiny.bindAll(this.api().table().node()); } '
          )
        )
      )
      
      output$summaryTable = DT::renderDT(
        values$summaryTable,
        escape = FALSE,
        selection = 'none',
        rownames = TRUE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          preDrawCallback = JS(
            'function() { Shiny.unbindAll(this.api().table().node()); }'
          ),
          drawCallback = JS(
            'function() { Shiny.bindAll(this.api().table().node()); } '
          )
        )
      )

    })
  })
}
