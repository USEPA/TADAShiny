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
    htmltools::HTML("Click the button below to run a series of tests that check for quality control issues or data formats not compatible with TADA. When the tests are finished running, a table will appear below. Each row describes an evaluation test, reports the number of results affected, and contains a switch users may toggle on/off to decide whether to flag results for removal. However, evaluation tests marked as <B>Required</B> have permanently 'ON' light blue switches that cannot be changed. <B>Recommended</B> tests are automatically switched 'ON' (darker blue), and <B>Optional</B> tests are automatically switched 'OFF' (gray)."),
    htmltools::div(style = "margin-bottom:10px"),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(ns("runFlags"),
                          "Run Tests",
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )),
    htmltools::div(style = "margin-bottom:10px"),
    DT::DTOutput(ns('flagTable')),
    htmltools::br(),
    htmltools::h3("Flag duplicative results uploaded by multiple organizations"),
    htmltools::HTML("Sometimes organizations unintentionally upload the same dataset multiple times to the WQP. For example, USGS will collect data at the request of state agencies. The USGS 'copy' of the results is uploaded to NWIS and made available in the portal, and the state agency's 'copy' of the results is uploaded to WQX. This step flags these duplicative uploads based on date, characteristic and result value/unit, and monitoring locations within 100 meters of one another to flag and select one representative result. Use the ordering widget to create a hierarchy of organizations used to select the representative result. For example, if the state agency's organization name is at the top of the list, its result will be selected over the USGS upload of the sample, and the USGS version will be flagged for removal."),
    htmltools::br(),
    htmltools::br(),
    shiny::fluidRow(column(8, shiny::uiOutput(ns("org_order")))),
    htmltools::br(),
    shiny::fluidRow(column(2, shiny::actionButton(ns("run_dups"), "Flag duplicate uploads", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
    htmltools::br(),
    shiny::uiOutput(ns("dup_nums")),
    htmltools::br(),
    htmltools::h3("Convert depth units (Optional)"),
    htmltools::HTML("Depth units in the dataset are automatically converted to <B>meters</B> upon data retrieval. Click the radio buttons below to convert depth units to feet, inches, or back to meters."),
    shiny::fluidRow(column(6, shiny::radioButtons(ns('m2f'), label = "", choices = c("feet","inches","meters"), selected = character(0), inline = TRUE)))
  )
}

mod_data_flagging_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flags <- shiny::reactiveValues()
    
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
        text = "Running flagging functions...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      # Add flagging columns to raw table
      tadat$raw = applyFlags(tadat$raw)
      #write.csv(tadat$raw, "flagged.csv")
      # tadat$raw = utils::read.csv("flagged.csv") # THIS IS TRIPS WORKING FILE FOR TESTING, COMMENT OUT WHEN COMMITTING TO DEVELOP
      
      # A table (raw rows, flags) indicating whether each record passes each test
      values$testResults <- flagCensus(tadat$raw)
      
      # The number of records failing each test
      values$n_fails <- colSums(values$testResults)
      
      # Remove progress bar and display instructions
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
      
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
        tadat$removals = dplyr::select(tadat$removals, -(dplyr::starts_with(prefix)))
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
      shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
      shinyjs::enable(selector = '.nav li a[data-value="Censored"]')
      shinyjs::enable(selector = '.nav li a[data-value="Harmonize"]')
      shinyjs::enable(selector = '.nav li a[data-value="Review"]')
    })
    
    shiny::observeEvent(input$m2f,{
      shiny::req(tadat$raw)
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
    
    # This widget creates the interactive ordering list
    output$org_order <- shiny::renderUI({
      shiny::req(tadat$raw)
        labels = data.frame(unique(tadat$raw[,c("OrganizationFormalName","OrganizationIdentifier")]))
        labels$label = substr(labels$OrganizationFormalName,1,70)
        labels$label = ifelse(nchar(labels$OrganizationFormalName)>70,paste0(labels$label, "..."),labels$OrganizationFormalName)
        flags$labels = labels
        shiny::isolate(
          shinyjqui::orderInput(ns("org_order"), label = "Drag and drop organization names in preferential order",
                                items = labels$label,
                                class = "btn-group-vertical")
        )
    })
    
    # Runs when the run button is pushed - runs the find potential duplicates between multiple orgs function
    shiny::observeEvent(input$run_dups,{
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Finding and flagging duplicate uploads...",
        session = shiny::getDefaultReactiveDomain()
      )
      # get to the org identifier from the input label ordering
      order = input$org_order
      flags$labels = flags$labels[match(order, flags$labels$label),]
      orgs = flags$labels$OrganizationIdentifier
      # run the duplicates function with the selected org hierarchy
      tadat$raw = TADA::TADA_FindPotentialDuplicatesMultipleOrgs(tadat$raw, org_hierarchy = orgs)
      # create a one-column dataframe with the results from the duplicates function to be added to tadat$removals
      multorgs = tadat$raw$TADA.ResultSelectedMultipleOrgs
      multorgs = ifelse(multorgs=="N", as.logical(TRUE), as.logical(FALSE))
      # manually add to tadat$removals - EDH couldn't come up with a simpler, straightforward way to include this in the flagging table in a way that wouldn't confuse users 
      label = "Flag: Metadata indicates duplicative uploads of the same results from multiple organizations"
      tadat$removals[label] = multorgs
      # tadat$raw = tadat$raw %>% dplyr::select(-TADA.ResultSelectedMultipleOrgs)
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      flags$mult_org_num = length(multorgs[multorgs==TRUE])
    })
    
    output$dup_nums <- shiny::renderUI({
      shiny::req(flags$mult_org_num)
      htmltools::h5(paste0(flags$mult_org_num, " duplicate uploads flagged for removal."))
    })
    
    # insertbreak <- function(x, len = 50){
    #   y = unlist(gregexpr(' ', x))
    #   y = y[y>len][1]
    #   z = y+1
    #   stringi::stri_sub(x, z, y) <- "\n "
    #   return(x)
    # }
    
    
  })
}
