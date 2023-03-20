#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Table title

    tags$div(id = 'table_class',

             h3(textOutput(ns("table_title"))),

             tableOutput(ns("table"))


    ),

  )
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table_title <- renderText({

      if (is.null(tadat$raw)){
        return(NULL)

      } else {
        "Data Import Summary"

      }

    })

    output$table <- renderTable({

      if (is.null(tadat$raw)) {
        # Just render nothing, because no file is uploaded.

        return(NULL)

      } else {

        # Apply the instance specific processing to the dataframe and render
        #use TADA R package for table contents
        import_summary_table <- TADA::SummarizeColumn(tadat$raw, col = "TADA.CharacteristicName")

        return(import_summary_table)
      }
    })

  })
}

## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")
