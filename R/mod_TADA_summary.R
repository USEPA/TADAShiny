#' TADA_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_TADA_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::fluidRow(column(4, style='padding-left:20px', shiny::wellPanel(htmltools::h3("TADA Working Summary:"),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_tot")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_rem")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_clean")))),
                                               htmltools::hr(),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_tot")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_rem")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_clean"))))
                                               )))
  )
}

#' TADA_summary Server Functions
#'
#' @noRd
mod_TADA_summary_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    summary_things = shiny::reactiveValues()

    shiny::observe({
      shiny::req(tadat$raw)
      summary_things$rem_rec = length(tadat$raw$ResultIdentifier[tadat$raw$Removed==TRUE])
      summary_things$clean_rec = length(tadat$raw$ResultIdentifier[tadat$raw$Removed==FALSE])
      clean_sites = unique(tadat$raw$MonitoringLocationIdentifier[tadat$raw$Removed==FALSE])
      summary_things$clean_site = length(clean_sites)
      summary_things$rem_site = length(unique(tadat$raw$MonitoringLocationIdentifier[!tadat$raw$MonitoringLocationIdentifier%in%clean_sites]))
    })

    output$rec_tot = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Records in Raw File: 0"
      }else{
        paste0("Total Records in Raw File: ",scales::comma(length(tadat$raw$ResultIdentifier)))
      }
    })

    output$rec_rem = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Records Removed: 0"
      }else{
        paste0("Total Records Removed: ",scales::comma(summary_things$rem_rec))
      }
    })

    output$rec_clean = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Records in Clean File: 0"
      }else{
        paste0("Total Records in Clean File: ",scales::comma(summary_things$clean_rec))
      }
    })

    output$site_tot = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Sites in Raw File: 0"
      }else{
        paste0("Total Sites in Raw File: ",scales::comma(length(unique(tadat$raw$MonitoringLocationIdentifier))))
      }
    })

    output$site_rem = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Sites Removed: 0"
      }else{
        paste0("Total Sites Removed: ",scales::comma(summary_things$rem_site))
      }
    })

    output$site_clean = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Sites in Clean File: 0"
      }else{
        paste0("Total Sites in Clean File: ",scales::comma(summary_things$clean_site))
      }
    })

  })
}

## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")

## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")
