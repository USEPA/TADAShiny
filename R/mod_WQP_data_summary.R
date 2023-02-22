#' WQP_data_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_WQP_data_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # Table title
    
    tags$div(id = 'table_class',
             
             h3(textOutput(ns("table_title"))),
             
             tableOutput(ns("table"))
             
             
    ),
    
  )
}

#' WQP_data_summary Server Functions
#'
#' @noRd 
mod_WQP_data_summary_server <- function(id, input_filepath){
  moduleServer( id, function(input, output, session){
    
    shinyjs::useShinyjs(html = TRUE)
    
    observe({
      shinyjs::toggleCssClass(id = "table_class", class = 'uploadSummary',
                              condition = !is.null(input_filepath()))
    })
    
    output$table_title <- renderText({
      
      if (is.null(input_filepath())){
        return(NULL)
        
      } else {
        "Data Import Summary"
        
      }
      
    })
    
    output$table <- renderTable({
      
      if (is.null(input_filepath())) {
        # Just render nothing, because no file is uploaded.
        
        return(NULL)
        
      } else {
        
        # Apply the instance specific processing to the dataframe and render
        #use TADA R package for table contents
        import_summary_table <- TADA::SummarizeCharacteristics(input_filepath()) 
        
        return(import_summary_table)
      }
    })
    
  })
}

## To be copied in the UI
# mod_WQP_data_summary_ui("WQP_data_summary_1")

## To be copied in the server
# mod_WQP_data_summary_server("WQP_data_summary_1")