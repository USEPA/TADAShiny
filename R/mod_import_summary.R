#' import_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_import_summary_ui <- function(id){
  
  ns <- NS(id)
  tagList(
    
    # Table title
    
    tags$div(id = 'table_class',
             
             h3(textOutput(ns("table_title"))),
             
             tableOutput(ns("table"))
             
    ),  
    
    
  )
  
}

#' import_summary Server Function
#'
#' @noRd 
mod_import_summary_server <- function(id, input_filepath, upload_error){
  
  moduleServer(
    id,
    function(input, output, session){
      
      shinyjs::useShinyjs(html = TRUE)
      
      observe({
        shinyjs::toggleCssClass(id = "table_class", class = 'uploadSummary',
                                condition = !is.null(input_filepath()) | upload_error() == "")
      })
      
      
      output$table_title <- renderText({
        
        if (is.null(input_filepath()) | upload_error() != ""){
          return(NULL)
          
        } else {
          "Data Import Summary"
          
        }
        
      })
      
      output$table <- renderTable({
        
        if (is.null(input_filepath()) | upload_error() != "") {
          # Just render nothing, because no file is uploaded.
          
          return(NULL)
          
        } else {
          
          # Apply the instance specific processing to the dataframe and render
          import_summary_table <- summarize_import(input_filepath()) 
          
          return(import_summary_table)
        }
        
        
      })
      
    })
}

