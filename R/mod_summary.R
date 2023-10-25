#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Table title
    tags$div(
      id = "table_class",
      htmltools::h3(shiny::textOutput(ns("table_title"))),
      shiny::tableOutput(ns("table"))
    ),
  )
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$table_title <- shiny::renderText({
      if (is.null(tadat$raw) || dim(tadat$raw)[1] < 1) {
        return(NULL)
      } else {
        "Data Import Summary"
      }
    })

    output$table <- shiny::renderTable({
      if (is.null(tadat$raw) || dim(tadat$raw)[1] < 1) {
        # Just render nothing, because no file is uploaded.

        return(NULL)
      } else {
        # Apply the instance specific processing to the dataframe and render
        # use TADA R package for table contents
        import_summary_table <- TADA::TADA_SummarizeColumn(tadat$raw, col = "TADA.CharacteristicName")

        return(import_summary_table)
      }
    })
  })
}

## To be copied in the UI
# mod_summary_ui("summary_1")

## To be copied in the server
# mod_summary_server("summary_1")
