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

# Used
#FilterFields(.data)
#Generate list of field name
#FilterFieldReview(field, .data)
#Generate list of unique values in a given field
#FilterParList (.data)
#Generate list of parameters

# Not yet used

#FilterParFields(.data, parameter)
#Generate list of field names subset by parameter

#FilterParFieldReview(field, .data, parameter)
#Generate list of unique values in a given field subset by parameter


mod_filtering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::textOutput(ns("promptStep0")),
    shiny::actionButton(ns("runFilter"), "Run Filtering"),
    shiny::textOutput(ns("promptStep1")),
    DT::dataTableOutput(ns("filterStep1")),
    shiny::textOutput(ns("promptStep2")),
    DT::DTOutput(ns("filterStep2")),
    shiny::textOutput(ns("promptStep3")),
    DT::DTOutput(ns("filterStep3")),
    shiny::actionButton(ns("addFilters"), "Add Selected Filters"),
    DT::DTOutput(ns("selectedFilters"))
  )
}



mod_filtering_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tables = reactiveValues()
    values = reactiveValues()
    values$buttonTarget <- "filterStep2"
    shinyjs::disable("addFilters")
    output$promptStep0 = shiny::renderText("Click the button to begin filtering")
    output$promptStep3 = renderText(NULL)
    output$filterStep2 = DT::renderDataTable(NULL)
    output$filterStep3 = DT::renderDataTable(NULL)
    tables$selected <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("Field", "Type", "Filter"))))
    output$selectedFilters = DT::renderDataTable(tables$selected)
    addButton = function(len) {
      inputs = character(len)
      for (i in seq_len(len)) {
        switch_name <- paste0("button_", i)
        inputs[i] = as.character(shiny::actionButton(ns(switch_name),
                                                     label = "+"))
      }
      inputs
    }
    
    shiny::observeEvent(input$runFilter, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Getting field names from dataset...",
        session = shiny::getDefaultReactiveDomain()
      )
      tables$filter_fields = data.frame(TADA::FilterFields(tadat$raw))
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      output$promptStep1 = renderText("1: Select field")
      output$filterStep1 = DT::renderDT(
        tables$filter_fields,
        escape = FALSE,
        selection = 'single',
        rownames = FALSE,
        options = list(dom = 't',
                       paging = FALSE)
      )
    })
    
    
    shiny::observeEvent(input$filterStep1_rows_selected, {
      values$selected_field = tables$filter_fields[input$filterStep1_rows_selected,]$Field
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = paste0(
          "Getting all values from field '",
          values$selected_field,
          "'..."
        ),
        session = shiny::getDefaultReactiveDomain()
      )
      output$filterStep2 = DT::renderDT(NULL)
      output$filterStep3 = DT::renderDT(NULL)
      tables$filter_values = data.frame(TADA::FilterFieldReview(values$selected_field, tadat$raw))
      output$promptStep2 = shiny::renderText(paste0("2: Filter by ", values$selected_field))
      if (values$selected_field == "TADA.CharacteristicName") {
        tables$filter_values$Add = NULL
        sel_type = 'single'
        shinyjs::disable("addFilters")
      } else{
        sel_type = 'multiple'
        shinyjs::enable("addFilters")
        values$buttonTarget <- "filterStep2"
      }
      output$filterStep2 = DT::renderDT(
        tables$filter_values,
        escape = FALSE,
        selection = 'multiple',
        rownames = FALSE,
        options = list(dom = 't',
                       paging = FALSE)
      )
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
    })
    
    shiny::observeEvent(input$filterStep2_rows_selected, {
      if (values$selected_field == "TADA.CharacteristicName") {
        output$filterStep3 = DT::renderDT(NULL)
        shinyjs::enable("addFilters")
        values$buttonTarget <- "filterStep3"
        selected_param = tables$filter_values[input$filterStep2_rows_selected,]$FieldValue
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = paste0(
            "Getting all fields for parameter '",
            selected_param,
            "'..."
          )
        )
        sel_values = TADA::FilterParFields(tadat$raw, selected_param)
        output$promptStep3 = shiny::renderText(paste0("3: Filter within ", selected_param))
        output$filterStep3 = DT::renderDataTable(
          data.frame(sel_values),
          escape = FALSE,
          selection = 'multiple',
          rownames = FALSE,
          options = list(dom = 't',
                         paging = FALSE)
        )
        shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
        
      }
    })
    
    shiny::observeEvent(input$addFilters, {
      if (values$buttonTarget == "filterStep2"){
        sel = input$filterStep2_rows_selected
      } else{
        sel = input$filterStep3_rows_selected
      }
    })
    
  })
}