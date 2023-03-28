library(shiny)
library(shinyWidgets)
library(DT)

# Read in the tables
prompt_table = read.csv("inst\\flag_prompts.csv")
test_table = read.csv("inst\\flag_tests.csv")
prompt_table <- prompt_table[order(prompt_table$Order),]
prompts <- prompt_table$Prompt
n_switches <- length(prompts)
flag_types <- prompt_table$flagType

flagCensus <- function(table) {
  print(length(flag_types))
  tabular_results = data.frame(matrix(ncol = length(flag_types), nrow =
                                        nrow(table)))
  colnames(tabular_results) <- flag_types
  for (flag in flag_types) {
    flag_count = 0
    tests = test_table[test_table$flagType == flag, ]
    results = integer(nrow(table))
    if (nrow(tests) > 0) {
      for (row in 1:nrow(tests)) {
        test_col = tests[row, 'columnName']
        test_val = tests[row, 'flagValue']
        if (test_col != 'Unknown') {
          results =
            results + as.integer(as.logical(table[test_col] == test_val))
        }
        tabular_results[flag] <- (results > 0)
      }
    } else {
      print(paste0("No tests found for flag ", flag))
    }
  }

  return(tabular_results)
}

getCounts <- function(sites, censusTable){

  summary_names = c(
    "Total in Raw File",
    "Total Removed",
    "Total in Clean File")
  
  removed_records = apply(censusTable, 1, any)
  
  # Records
  n_raw_records = nrow(censusTable)
  n_removed_records = sum(removed_records)
  n_clean_records = n_raw_records - n_removed_records
  
  # Sites
  n_raw_sites = length(unique(sites))
  n_removed_sites = length(unique(sites[removed_records]))
  n_clean_sites = n_raw_sites - n_removed_sites
  
  summaryTable = data.frame(
    row.names = summary_names,
    Records = c(n_raw_records, n_removed_records, n_clean_records),
    Sites = c(n_raw_sites, n_removed_sites, n_clean_sites)
  )  
  return(summaryTable)
}

mod_data_flagging_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(style = "display: none;",
             prettySwitch("dummy", label = NULL)),
    htmlOutput(ns('step_1')),
    fluidRow(column(
      3, actionButton(ns("runFlags"), "Run Data Flagging")
    )),
    htmlOutput(ns('step_2')),
    DT::dataTableOutput(ns('flagTable')),
    br(),
    htmlOutput(ns('step_3')),
    DT::dataTableOutput(ns('summaryTable'))
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
    values = reactiveValues()
    values$census = integer(length(n_switches))

    # 
    observeEvent(input$runFlags, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Flagging invalid or out-of-range data...",
        session = shiny::getDefaultReactiveDomain()
      )
      flagged = read.csv("flagged.csv")
      #flagged = applyFlags(tadat$raw)
      #write.csv(flagged, "flagged.csv")
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
      
      output$flagTable = DT::renderDataTable(
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
      
      output$summaryTable = DT::renderDataTable(
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
