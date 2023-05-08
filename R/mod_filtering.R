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
    DT::DTOutput(ns("filterStep3"))
  )
}



mod_filtering_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tables = reactiveValues()
    values = reactiveValues()
    output$promptStep0 = shiny::renderText("Click the button to begin filtering")
    output$filterStep2 = DT::renderDataTable(NULL)
    output$filterStep3 = DT::renderDataTable(NULL)
    
    addButton = function(len) {
      inputs = character(len)
      for (i in seq_len(len)) {
        switch_name <- paste0("button_", i)
        inputs[i] = as.character(
          shiny::actionButton(
            ns(switch_name),
            label = "Add"
          ))}
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
      print(012)
      tables$filter_values = data.frame(TADA::FilterFieldReview(values$selected_field, tadat$raw))
      print(123)
      tables$filter_values$Add = addButton(nrow(tables$filter_values))
      print(234)
      output$promptStep2 = shiny::renderText(paste0("2: Filter by ", values$selected_field))
      output$filterStep2 = DT::renderDataTable(
        tables$filter_values,
        escape = FALSE,
        selection = 'single',
        rownames = FALSE,
        options = list(dom = 't',
                       paging = FALSE)
      )
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
    })
    
    shiny::observeEvent(input$filterStep2_rows_selected, {
      if (values$selected_field == "TADA.CharacteristicName") {
        selected_param = tables$filter_values[input$filterStep2_rows_selected,]$FieldValue
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = paste0(
            "Getting all fields for parameter '",
            selected_param,
            "'..."
          ))
        output$promptStep3 = shiny::renderText(paste0("3: Filter within ", selected_param))
        output$filterStep3 = DT::renderDataTable(
          data.frame(TADA::FilterParFields(tadat$raw, selected_param)),
          escape = FALSE,
          selection = 'single',
          rownames = FALSE,
          options = list(dom = 't',
                         paging = FALSE)
        )
        shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
        
      }
    })
    
  })
}