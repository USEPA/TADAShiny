#' TADA_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_TADA_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::fluidRow(
    column(
      4,
      style = 'padding-left:20px',
      shiny::wellPanel(
        htmltools::h3("TADA Working Summary"),
        shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
          "rec_tot"
        )))),
        shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
          "rec_rem"
        )))),
        shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
          "rec_clean"
        )))),
        htmltools::hr(),
        shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
          "site_tot"
        )))),
        shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
          "site_rem"
        )))),
        shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
          "site_clean"
        )))),
        shiny::fluidRow(column(6, shiny::uiOutput(ns(
          "dwn_all"
        ))))
      ),
      shiny::fluidRow(column(2, shiny::actionButton(ns("disclaimer"),"DISCLAIMER"))),
      htmltools::br(),
      htmltools::br()
    ),
    #,
    # column(4,
    #        shiny::wellPanel(
    #          htmltools::h3("Removed Record Summary"),
    #          DT::DTOutput(ns("removal_summary"))
    #        ))
  ))
}

#' TADA_summary Server Functions
#'
#' @noRd
mod_TADA_summary_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # reactive list to hold reactive objects specific to this module
    summary_things = shiny::reactiveValues()
    
    # calculate the stats needed to fill the summary box
    shiny::observe({
      shiny::req(tadat$raw)
      summary_things$rem_rec = length(tadat$raw$ResultIdentifier[tadat$raw$TADA.Remove ==
                                                                   TRUE])
      summary_things$clean_rec = length(tadat$raw$ResultIdentifier[tadat$raw$TADA.Remove ==
                                                                     FALSE])
      clean_sites = unique(tadat$raw$MonitoringLocationIdentifier[tadat$raw$TADA.Remove ==
                                                                    FALSE])
      summary_things$clean_site = length(clean_sites)
      summary_things$rem_site = length(unique(tadat$raw$MonitoringLocationIdentifier[!tadat$raw$MonitoringLocationIdentifier %in%
                                                                                       clean_sites]))
      summary_things$removals = sort_removals(tadat$removals)
      
    })
    summary_things$removals <- data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c("Reason", "Count"))
    ))
    
    # output$removal_summary = DT::renderDataTable(
    #   summary_things$removals,
    #   escape = FALSE,
    #   rownames = FALSE,
    #   options = list(
    #     dom = 't',
    #     paging = FALSE,
    #     language = list(zeroRecords = "No records removed")
    #   )
    # )
    
    # summary text = total records
    output$rec_tot = shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Results in Dataset: 0"
      } else{
        paste0("Total Results in Dataset: ", scales::comma(length(tadat$raw$ResultIdentifier)))
      }
    })
    # summary text = total records removed
    output$rec_rem = shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Results Flagged for Removal: 0"
      } else{
        paste0("Total Results Flagged for Removal: ",
               scales::comma(summary_things$rem_rec))
      }
    })
    # summary text = total records in clean
    output$rec_clean = shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Results Retained: 0"
      } else{
        paste0("Total Results Retained: ",
               scales::comma(summary_things$clean_rec))
      }
    })
    # summary text = total sites
    output$site_tot = shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites in Dataset: 0"
      } else{
        paste0("Total Sites in Dataset: ", scales::comma(length(
          unique(tadat$raw$MonitoringLocationIdentifier)
        )))
      }
    })
    # summary text = total sites removed - sites with NO records in the clean file
    output$site_rem = shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites Flagged for Removal: 0"
      } else{
        paste0("Total Sites Flagged for Removal: ",
               scales::comma(summary_things$rem_site))
      }
    })
    # summary text = total sites in clean file
    output$site_clean = shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites Retained: 0"
      } else{
        paste0("Total Sites Retained: ",
               scales::comma(summary_things$clean_site))
      }
    })
    
    # download dataset button - only appears if there exists data in the app already
    output$dwn_all = shiny::renderUI({
      shiny::req(tadat$raw)
      shiny::downloadButton(ns("download_all"),
                            "Download Working Dataset (.xlsx)",
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    output$download_all = shiny::downloadHandler(
      filename = function() {
        paste('TADAShiny_datadownload_', tadat$tab, '.xlsx', sep = '')
      },
      content = function(file) {
        writexl::write_xlsx(TADA::TADA_OrderCols(tadat$raw), path = file)
      }
    )
    
    shiny::observeEvent(input$disclaimer, {
      shiny::showModal(shiny::modalDialog(title = "Disclaimer",
                                          "This United States Environmental Protection Agency (EPA) GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government."))
      
    })
    
  })
}

sort_removals <- function(removal_table) {
  if (length(removal_table) > 0) {
    prefixes <- c("Flag", "Filter")
    fields <- colnames(removal_table)
    results <-
      data.frame(matrix(
        nrow = nrow(removal_table),
        ncol = length(prefixes)
      ))
    colnames(results) <- prefixes
    results[is.na(results)] <- FALSE
    
    for (prefix in prefixes) {
      active_cols = fields[dplyr::starts_with(prefix, vars = fields)]
      if (length(active_cols) > 0) {
        results[prefix] = apply(dplyr::select(removal_table, active_cols), 1, any)
      }
    }
    totals = rowSums(results)
    results["Flag only"] = ((totals == 1) & results$Flag)
    results["Flag and Filter"] = (results$Flag & results$Filter)
    results["Filter only"] = ((totals == 1) & results$Filter)
    results = dplyr::select(results,-intersect(prefixes,  colnames(results)))
    results$Many <- rowSums(results) > 2
    results$Retained <- !apply(results, 1, any)
    counts <- colSums(results)
    counts = data.frame(Reason = names(counts), Count = as.vector(counts))
    counts = counts[(counts$Count > 0),]
    return(counts)
  }
}
## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")

## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")
