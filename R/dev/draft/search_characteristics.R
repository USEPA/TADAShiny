# this snippet runs the Characteristics search using a selectable 'match type'.
# It uses a separate 'text_input' widget to search the values
# to simplify the reactive behavior.  It might be possible to collapse
# the widgets and search directly in the drop-down list, but I was not able to get that to work.

match_types <- c(
  "Starts With" = 'starts_with',
  "Ends With" = 'ends_with',
  "Contains" = 'contains',
  "Equals" = 'matches'
)
characteristic_list <- unique(utils::read.csv(url(
      "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"
    ))$Name)
# # this can be loaded from a local file in much less time
# characteristic_table <- utils::read.csv("../inst/Characteristic.CSV")
# characteristic_list <- characteristic_table$Name

ui <- fluidPage(
  titlePanel("Selectize with 'Match Type' Search"),
  
  fluidRow(
      column(4,
         selectizeInput(
          inputId = "match_type_selector",
          label = "Match type:",
          choices = match_types, # Choices are populated on client
          selected = 'contains',
          multiple = FALSE
        )
      ),
      column(4,
        # Input for the user to type their search string
        textInput(
          inputId = "text_sring", 
          label = "Search string:", 
          value = ""
        )
      ),
      column(4,
        selectizeInput(
          inputId = "characteristic_select", 
          label = "Select one or more Characteristic:", 
          choices = NULL, 
          multiple = TRUE,
          options = list(
            openOnFocus = TRUE
          )
        )
      )
  )
)

server <- function(input, output, session) {

  # A reactive expression that filters the choices based on the input pattern
  filtered_list <- reactive({
    text_sring <- input$text_sring
    if (text_sring == "") {
      # If the text string is empty, return all choices
      return(characteristic_list)
    } 
    else {
      match_type <- 'contains'
      if (input$match_type_selector != '') {
        match_type = input$match_type_selector
      }
      # set the grep pattern for each match type
      if (match_type == 'starts_with') {
        grep_pattern <- paste0("^", text_sring)
      }
      else if (match_type == 'ends_with') {
        grep_pattern <- paste0(text_sring, "$")
      }
      else if (match_type == 'matches') {
        grep_pattern <- paste0("^", text_sring, "$")
      }
      else { # contains
        grep_pattern <- text_sring
      }
      return(characteristic_list[grep(
                                      grep_pattern,
                                      characteristic_list,
                                      ignore.case = TRUE
                                    )
                                 ]
      )
    }
  })
  
  # Observer to update the selectizeInput choices whenever the filtered_list changes
  observe({
    # using isolate() here is key to this whole thing working.
    # the value would be subject to an event when the updateSelectizeInput() happens below,
    # so you need to 'isolate' the current value before you run the update
    previous_selected <- isolate(input$characteristic_select)
    
    updateSelectizeInput(
      session,
      "characteristic_select",
      choices = c(filtered_list(), previous_selected),
      server = TRUE,
      selected = previous_selected
    )
  })
}


shinyApp(ui, server)
