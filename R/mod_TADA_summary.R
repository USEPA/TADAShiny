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
    shiny::fluidRow(column(4, style='padding-left:20px', shiny::wellPanel(htmltools::h3("TADA Working Summary"),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_tot")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_rem")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_clean")))),
                                               htmltools::hr(),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_tot")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_rem")))),
                                               shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_clean")))),
                                               shiny::fluidRow(column(6, shiny::uiOutput(ns("dwn_all"))))
                                               )))
  )
}

#' TADA_summary Server Functions
#'
#' @noRd
mod_TADA_summary_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    # reactive list to hold reactive objects specific to this module
    summary_things = shiny::reactiveValues()
    # calculate the stats needed to fill the summary box
    shiny::observe({
      shiny::req(tadat$raw)
      summary_things$rem_rec = length(tadat$raw$ResultIdentifier[tadat$raw$TADA.Remove==TRUE])
      summary_things$clean_rec = length(tadat$raw$ResultIdentifier[tadat$raw$TADA.Remove==FALSE])
      clean_sites = unique(tadat$raw$MonitoringLocationIdentifier[tadat$raw$TADA.Remove==FALSE])
      summary_things$clean_site = length(clean_sites)
      summary_things$rem_site = length(unique(tadat$raw$MonitoringLocationIdentifier[!tadat$raw$MonitoringLocationIdentifier%in%clean_sites]))
    })
    # summary text = total records
    output$rec_tot = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Results in Dataset: 0"
      }else{
        paste0("Total Results in Dataset: ",scales::comma(length(tadat$raw$ResultIdentifier)))
      }
    })
    # summary text = total records removed
    output$rec_rem = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Results Flagged for Removal: 0"
      }else{
        paste0("Total Results Flagged for Removal: ",scales::comma(summary_things$rem_rec))
      }
    })
    # summary text = total records in clean
    output$rec_clean = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Results Retained: 0"
      }else{
        paste0("Total Results Retained: ",scales::comma(summary_things$clean_rec))
      }
    })
    # summary text = total sites
    output$site_tot = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Sites in Dataset: 0"
      }else{
        paste0("Total Sites in Dataset: ",scales::comma(length(unique(tadat$raw$MonitoringLocationIdentifier))))
      }
    })
    # summary text = total sites removed - sites with NO records in the clean file
    output$site_rem = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Sites Flagged for Removal: 0"
      }else{
        paste0("Total Sites Flagged for Removal: ",scales::comma(summary_things$rem_site))
      }
    })
    # summary text = total sites in clean file
    output$site_clean = shiny::renderText({
      if(is.null(tadat$raw)){
        "Total Sites Retained: 0"
      }else{
        paste0("Total Sites Retained: ",scales::comma(summary_things$clean_site))
      }
    })
    
    # download dataset button - only appears if there exists data in the app already
    output$dwn_all = shiny::renderUI({
      shiny::req(tadat$raw)
      shiny::downloadButton(ns("download_all"),"Download Working Dataset (.xlsx)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    output$download_all = shiny::downloadHandler(
      filename = function() {
            paste('TADAShiny_datadownload_', tadat$tab, '.xlsx', sep='')
          },
          content = function(file) {
            writexl::write_xlsx(TADA::OrderTADACols(tadat$raw), path = file)
          }
    )

  })
}

## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")

## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")
