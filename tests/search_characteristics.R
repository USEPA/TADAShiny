library(shiny)

match_types <- c(
  "Starts With" = 'starts_with',
  "Ends With" = 'ends_with',
  "Contains" = 'contains',
  "Equals" = 'matches'
)
characteristic_list <- unique(utils::read.csv(url(
      "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"
    ))$Name)

ui <- fluidPage(
  titlePanel("Selectize 'Starts With' Filter"),
  
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
      column(8,
        selectizeInput(
          inputId = "characteristic_select", 
          label = "Select one or more Characteristic:", 
          choices = NULL, 
          multiple = TRUE
        )
      )
  )
)

server <- function(input, output, session) {
  # All possible choices
  all_characteristics <- characteristic_list
  
  # Set up server-side selectize
  updateSelectizeInput(
    session = session,
    inputId = "characteristic_select",
    choices = all_characteristics,
    server = TRUE,
    options = list(
      placeholder = "Start typing to search...",
      # The searchField option allows customization of the search behavior,
      # but for a simple list, the default behavior is sufficient for 'starts with'.
      searchField = "value",
      onType = I('
           text => Shiny.setInputValue("characteristic_select_search", text)
        ')
    )
  )
  
  # reset the server-size selectize when 'Match type' changes
  observeEvent(input$match_type_selector, {
      updateSelectizeInput(
        session = session,
        inputId = "characteristic_select",
        choices = characteristic_list,
        server = TRUE
      )
  }, ignoreInit = TRUE)
  
  # Reactive observer to listen for changes in the selectize input text string
  observeEvent(input$characteristic_select_search, {
    query <- input$characteristic_select_search
    
    if (nchar(query) > 0) {
          match_type <- 'contains'
          if (input$match_type_selector != '') {
            match_type = input$match_type_selector
          }
          # set the grep pattern for each match type
          if (match_type == 'starts_with') {
            grep_pattern <- paste0("^", query)
          }
          else if (match_type == 'ends_with') {
            grep_pattern <- paste0(query, "$")
          }
          else if (match_type == 'matches') {
            grep_pattern <- paste0("^", query, "$")
          }
          else { # contains
            grep_pattern <- query
          }
          filtered_characteristics <- all_characteristics[grep(
            grep_pattern,
            all_characteristics,
            ignore.case = TRUE
          )]
    } else {
      filtered_characteristics <- all_characteristics
    }
    
    updateSelectizeInput(
      session = session,
      inputId = "characteristic_select",
      choices = filtered_characteristics,
      server = TRUE
    )
    
  }, ignoreInit = TRUE)
  
} # end server function

shinyApp(ui, server)
