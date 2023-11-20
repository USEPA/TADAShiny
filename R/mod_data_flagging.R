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
    htmltools::h3("Flag data for potential issues"),
    htmltools::HTML(
      "Click the button below to run a series of tests that check for quality control issues or data formats not compatible with TADA. When the tests are finished running, a table will appear below. Each row describes an evaluation test, reports the number of results affected, and contains a switch users may toggle on/off to decide whether to flag results for removal. However, evaluation tests marked as <B>Required</B> have permanently 'ON' light blue switches that cannot be changed. <B>Recommended</B> tests are automatically switched 'ON' (darker blue), and <B>Optional</B> tests are automatically switched 'OFF' (gray)."
    ),
    htmltools::div(style = "margin-bottom:10px"),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(ns("runFlags"),
                          "Run Tests",
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )),
    htmltools::div(style = "margin-bottom:10px"),
    DT::DTOutput(ns("flagTable")),
    htmltools::br(),
    htmltools::h3("Convert depth units (Optional)"),
    htmltools::HTML(
      "Depth units in the dataset are automatically converted to <B>meters</B> upon data retrieval. Click the radio buttons below to convert depth units to feet, inches, or back to meters."
    ),
    shiny::fluidRow(column(
      6,
      shiny::radioButtons(
        ns("m2f"),
        label = "",
        choices = c("feet", "inches", "meters"),
        selected = character(0),
        inline = TRUE
      )
    ))
  )
}

mod_data_flagging_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    flags <- shiny::reactiveValues()
    values <- shiny::reactiveValues()
    values$n_fails <- integer(length(n_switches))
    tadat$selected_flags <- character()
    tadat$switch_defaults <- prompt_table$Level != "Optional"
    switch_disabled <- prompt_table$Level == "Required"

    flagSwitch <- function(len) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        switch_name <- paste0("switch_", i)
        if (!(i %in% which(unlist(switch_disabled)))) {
          inputs[i] <- as.character(
            shinyWidgets::prettySwitch(
              ns(switch_name),
              label = NULL,
              value = tadat$switch_defaults[i],
              status = "primary",
              fill = TRUE
            )
          )
        } else {
          inputs[i] = "n/a"
        }
      }
      inputs
    }
    
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value <- input[[paste0(id, i)]]
        if (is.null(value)) {
          TRUE
        } else {
          value
        }
      }))
    }
    
    
    
    
    # Runs whenever selected flags are changed
    shiny::observeEvent(tadat$selected_flags, {
      prefix = "Flag: "
      if (!is.null(tadat$removals)) {
        tadat$removals = dplyr::select(tadat$removals,-(dplyr::starts_with(prefix)))
      }
      # Loop through the flags
      for (flag in tadat$selected_flags) {
        # If not all the values are NA, add the test results to removals
        if (!is.null(tadat$removals)) {
          if (!all(is.na(values$testResults[flag]))) {
            tadat$removals[paste0(prefix, flag)] = values$testResults[flag]
          }
        }
        # If the switch corresponding to this flag isn't on, switch it on
        # Checking a random switch to make sure they've been initialized
        pos = match(flag, prompts)
        tadat$switch_defaults[pos] = TRUE
        if (!is.null(input[[paste0("switch_", pos)]])) {
          switch_name = paste0("switch_", pos)
          if (is.na(pos)) {
            invalidFile("flagging")
          } else if (!isTRUE(input[[switch_name]])) {
            # Turn the switch on if it isn't already
            shinyWidgets::updatePrettySwitch(inputId = switch_name,
                                             value = TRUE)
          }
        }
      }
    })
    
    # Runs when the flag button is clicked
    shiny::observeEvent(input$runFlags, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Running flagging functions...",
        session = shiny::getDefaultReactiveDomain()
      )

      # Add flagging columns to raw table
      #tadat$raw <- applyFlags(tadat$raw, tadat$orgs)
      #write.csv(tadat$raw, "flagged.csv")
      tadat$raw = utils::read.csv("flagged.csv") # THIS IS TRIPS WORKING FILE FOR TESTING, COMMENT OUT WHEN COMMITTING TO DEVELOP
      
      # A table (raw rows, flags) indicating whether each record passes each test
      values$testResults <- flagCensus(tadat$raw)
      
      # The number of records failing each test
      values$n_fails <- colSums(values$testResults)
      
      # Remove progress bar and display instructions
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
      # Runs when any of the flag switches are changed
      shiny::observe({
        switch_id = "switch_"
        tadat$selected_flags = flag_types[shinyValue(switch_id, n_switches)]
        for (i in which(switch_disabled)) {
          shinyjs::disable(paste0(switch_id, i))
        }
      })
      
      switchTable <- shiny::reactive({
        df <- data.frame(
          Reason = prompts,
          Results = values$n_fails,
          Required = levs,
          Decision = flagSwitch(n_switches)
        )
      })
      
      output$flagTable <- DT::renderDT(
        shiny::isolate(switchTable()),
        escape = FALSE,
        selection = "none",
        colnames = c(
          "Flag reason",
          "Results affected",
          "Required/Optional",
          "Switch 'on' to flag for removal"
        ),
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          preDrawCallback = DT::JS(
            "function() { Shiny.unbindAll(this.api().table().node()); }"
          ),
          drawCallback = DT::JS(
            "function() { Shiny.bindAll(this.api().table().node()); } "
          )
        )
      )
      shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
      shinyjs::enable(selector = '.nav li a[data-value="Censored"]')
      shinyjs::enable(selector = '.nav li a[data-value="Harmonize"]')
      shinyjs::enable(selector = '.nav li a[data-value="Figures"]')
      shinyjs::enable(selector = '.nav li a[data-value="Review"]')
    })
    
    shiny::observeEvent(tadat$m2f, {
      updateRadioButtons(session, "m2f", selected = tadat$m2f)
    })
    
    shiny::observeEvent(input$m2f, {
      tadat$m2f <- input$m2f
      shiny::req(tadat$raw)
      if (input$m2f == "feet") {
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to feet...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw <-
          TADA::TADA_ConvertDepthUnits(tadat$raw, unit = "ft")
      }
      if (input$m2f == "inches") {
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to inches...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw <-
          TADA::TADA_ConvertDepthUnits(tadat$raw, unit = "in")
      }
      if (input$m2f == "meters") {
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to meters...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw <-
          TADA::TADA_ConvertDepthUnits(tadat$raw, unit = "m")
      }
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
    })
  })
}
