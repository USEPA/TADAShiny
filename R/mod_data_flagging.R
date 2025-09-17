#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_data_flagging_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Add CSS directly in the UI module to disable interaction for required switches
    tags$style(HTML("
      .disabled-switch {
        pointer-events: none; /* Disable mouse events */
        opacity: 0.5; /* Make it visually clear it's disabled */
      }
    ")),
    
    tags$div(
      style = "display: none;",
      shinyWidgets::prettySwitch("dummy", label = NULL)
    ),
    htmltools::h3("Flag data for potential issues"),
    htmltools::HTML(
      "Click the button below to run a series of tests that check for quality control issues or data formats not compatible with TADA. When the tests are finished running, a table will appear below. Each row describes an evaluation test, reports the number of results affected, and contains a switch users may toggle on/off to decide whether to flag results for removal. However, evaluation tests marked as <B>Required</B> have permanently 'ON' light blue switches that cannot be changed. <B>Recommended</B> tests are automatically switched 'ON' (darker blue), and <B>Optional</B> tests are automatically switched 'OFF' (gray)."
    ),
    htmltools::div(style = "margin-bottom:10px"),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(ns("runFlags"),
                          "Run Tests",
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
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
        selected = "meters",
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
    
    # Function to create toggle switches for each flag
    flagSwitch <- function(len) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        switch_name <- base::paste0("switch_", i)
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
          inputs[i] <- as.character(
            shinyWidgets::prettySwitch(
              ns(switch_name),
              label = NULL,
              value = TRUE, # Required flags are always TRUE
              status = "primary",
              fill = TRUE
            )
          )
          # Use JavaScript to add a CSS class that disables interaction
          shinyjs::runjs(sprintf("$('#%s').addClass('disabled-switch');", ns(switch_name)))
        }
      }
      inputs
    }
    
    # Function to get the current state of each switch
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value <- input[[base::paste0(id, i)]]
        if (is.null(value)) {
          FALSE
        } else {
          value
        }
      }))
    }
    
    # Update removals based on the state of the switches
    shiny::observe({
      switch_id <- "switch_"
      tadat$selected_flags <- flag_types[shinyValue(switch_id, n_switches)]
      
      # Ensure tadat$raw is not NULL
      shiny::req(tadat$raw)
      
      # Initialize or clear removals for each flag
      for (i in seq_len(n_switches)) {
        flag <- flag_types[i]
        switch_name <- base::paste0(switch_id, i)
        
        if (!is.null(input[[switch_name]])) {
          if (input[[switch_name]]) {
            # If the switch is on, update removals with the test results
            tadat$removals[[flag]] <- values$testResults[[flag]]
          } else {
            # If the switch is off, set removals for this flag to FALSE
            tadat$removals[[flag]] <- rep(FALSE, nrow(tadat$raw))
          }
        }
      }
    })
    
    # Runs whenever selected flags are changed
    shiny::observeEvent(tadat$selected_flags, {
      if (!is.null(tadat$removals)) {
        tadat$removals <- dplyr::select(tadat$removals, -(dplyr::starts_with(flag_prefix)))
      }
      if ((!is.null(tadat$raw)) & (!is.null(tadat$selected_flags))) {
        shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
      }
      # Loop through the flags
      for (flag in tadat$selected_flags) {
        # If not all the values are NA, add the test results to removals
        if (!is.null(tadat$removals)) {
          if (!all(is.na(values$testResults[flag]))) {
            tadat$removals[base::paste0(flag_prefix, flag)] <- values$testResults[flag]
          }
        }
        pos <- match(flag, flag_types)
        tadat$switch_defaults[pos] <- TRUE
        if (!is.null(input[[base::paste0("switch_", pos)]])) {
          switch_name <- base::paste0("switch_", pos)
          if (is.na(pos)) {
            invalidFile("flagging")
          } else if (!isTRUE(input[[switch_name]])) {
            shinyWidgets::updatePrettySwitch(
              inputId = switch_name,
              value = TRUE
            )
          }
        }
      }
    })
    
    # Any time tadat$raw is changed, check to see if the flagging fields are present
    shiny::observeEvent(tadat$raw, {
      tadat$flags_present <- checkFlagColumns(tadat$raw)
    })
    
    shiny::observeEvent(tadat$flags_present, {
      if (tadat$flags_present) {
        values$testResults <- flagCensus(tadat$raw)
        values$n_fails <- colSums(values$testResults)
        
        shiny::observe({
          switch_id <- "switch_"
          tadat$selected_flags <- flag_types[shinyValue(switch_id, n_switches)]
          for (i in which(switch_disabled)) {
            shinyjs::disable(base::paste0(switch_id, i))
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
            ordering = TRUE,
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
      }
    })
    
    shiny::observeEvent(input$runFlags, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Running flagging functions...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      tadat$raw <- applyFlags(tadat$raw, tadat$orgs)
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
    })
    
    shiny::observeEvent(tadat$m2f, {
      shiny::updateRadioButtons(session, "m2f", selected = tadat$m2f)
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
        tadat$raw <- EPATADA::TADA_ConvertDepthUnits(tadat$raw, unit = "ft")
      }
      if (input$m2f == "inches") {
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to inches...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw <- EPATADA::TADA_ConvertDepthUnits(tadat$raw, unit = "in")
      }
      if (input$m2f == "meters") {
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = "Converting depth units to meters...",
          session = shiny::getDefaultReactiveDomain()
        )
        tadat$raw <- EPATADA::TADA_ConvertDepthUnits(tadat$raw, unit = "m")
      }
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
    })
  })
}
