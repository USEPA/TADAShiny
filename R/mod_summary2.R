#' TADA_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_TADA_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(shiny::fluidRow(
    style = "padding-left:20px",
    shiny::wellPanel(
      htmltools::h3("Results Summary"),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_tot")))),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_rem")))),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("rec_clean")))),
      htmltools::hr(),
      htmltools::h3("Monitoring Location Summary"),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_tot")))),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_rem")))),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns("site_clean")))),
      htmltools::hr(),
      htmltools::h3("Download Working or Final Dataset"),
      htmltools::HTML(
        "Download the working dataset as a .zip file. Original data are preserved with
   the original column names. This tool creates copies of any columns that are
   modified and adds the TADA. prefix to those. In addition, new TADA-only flag columns
   are added for transparency and tracking purposes.
   An overall summary <i>Remove</i> column (far right in output) is also added
   to track decisions you make within this app to include or exclude results
   throughout the process. The <i>Removal Reason</i> column may also be included
   in this output but is only available here after running the 'Load Review Data'
   button on the 'Review' tab.<br><br>"
      ),
      shiny::fluidRow(shiny::column(
        6,
        shiny::downloadButton(
          ns("dwn_working"),
          "Working Dataset (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 10px;"
        )
      )),
      htmltools::HTML(
        "Download the final dataset as a .zip file. This has been filtered to remove
   all results that the user flagged for removal throughout the application.<br><br>"
      ),
      shiny::fluidRow(shiny::column(
        6,
        shiny::downloadButton(
          ns("dwn_final"),
          "Final Dataset (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 10px;"
        )
      )),
      htmltools::h5(
        "Note: A progress file in the .RData format will always be
   included in the download. See the 'Upload Progress File' option on the
   import tab for details on how to use this file to regenerate a dataset
   with the same decisions you made before, or to apply the same user
   selections to a new dataset."
      ),
    ),
    shiny::fluidRow(shiny::column(
      2,
      shiny::actionButton(ns("disclaimer"), "DISCLAIMER")
    )),
    htmltools::br(),
    htmltools::br()
    # )
  ))
}


#' TADA_summary Server Functions
#'
#' @noRd
mod_TADA_summary_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    summary_things <- shiny::reactiveValues()

    # Helper to create the download zip
    make_download_zip <- function(zipfile, mode = c("working", "final")) {
      mode <- match.arg(mode)

      tmpdir <- tempdir()
      base_name <- tadat$default_outfile

      xlsx_name <- file.path(
        tmpdir,
        paste0(
          base_name,
          if (mode == "working") "_working.xlsx" else "_final.xlsx"
        )
      )
      prog_name <- file.path(tmpdir, paste0(base_name, "_prog.RData"))

      # Build the dataset
      if (mode == "working") {
        out_data <- EPATADA::TADA_OrderCols(tadat$raw)
      } else {
        # Safely handle missing TADA.Remove
        if ("TADA.Remove" %in% names(tadat$raw)) {
          out_data <- tadat$raw[tadat$raw$TADA.Remove == FALSE, ]
        } else {
          out_data <- tadat$raw
        }

        out_data <- EPATADA::TADA_OrderCols(out_data)

        # Safely drop optional columns if present
        out_data <- dplyr::select(
          out_data,
          -dplyr::any_of(c("TADA.Remove", "TADA.RemovalReason"))
        )

        out_data <- EPATADA::TADA_RetainRequired(out_data)
      }

      desc <- writeNarrativeDataFrame(tadat)
      dfs <- list(Data = out_data, Parameterization = desc)

      # Write supporting files
      writeFile(tadat, prog_name)
      writexl::write_xlsx(dfs, path = xlsx_name, use_zip64 = TRUE)

      # Zip them into the final download file
      utils::zip(zipfile = zipfile, files = c(xlsx_name, prog_name))
    }

    # Enable download buttons when raw data exists
    shiny::observeEvent(
      tadat$raw,
      {
        if (!is.null(tadat$raw)) {
          shinyjs::enable("dwn_working")
          shinyjs::enable("dwn_final")
        }
      },
      ignoreInit = TRUE
    )

    # Working dataset download
    output$dwn_working <- shiny::downloadHandler(
      filename = function() {
        paste0(tadat$default_outfile, "_working.zip")
      },
      content = function(fname) {
        tryCatch(
          {
            make_download_zip(fname, mode = "working")
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error creating working download:", conditionMessage(e)),
              type = "error",
              duration = 30
            )
            stop(e)
          }
        )
      },
      contentType = "application/zip"
    )

    # Final dataset download
    output$dwn_final <- shiny::downloadHandler(
      filename = function() {
        paste0(tadat$default_outfile, "_final.zip")
      },
      content = function(fname) {
        tryCatch(
          {
            make_download_zip(fname, mode = "final")
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error creating final download:", conditionMessage(e)),
              type = "error",
              duration = 30
            )
            stop(e)
          }
        )
      },
      contentType = "application/zip"
    )

    # Summary statistics
    shiny::observe({
      shiny::req(tadat$raw)

      summary_things$rem_rec <- length(tadat$raw$ResultIdentifier[
        tadat$raw$TADA.Remove == TRUE
      ])

      summary_things$clean_rec <- length(tadat$raw$ResultIdentifier[
        tadat$raw$TADA.Remove == FALSE
      ])

      clean_sites <- unique(tadat$raw$MonitoringLocationIdentifier[
        tadat$raw$TADA.Remove == FALSE
      ])

      summary_things$clean_site <- length(clean_sites)

      summary_things$rem_site <- length(unique(tadat$raw$MonitoringLocationIdentifier[
        !tadat$raw$MonitoringLocationIdentifier %in% clean_sites
      ]))

      summary_things$removals <- sort_removals(tadat$removals)

      shinyjs::enable("dwn_working")
      shinyjs::enable("dwn_final")
    })

    summary_things$removals <- data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c("Reason", "Count"))
    ))

    # Text outputs
    output$rec_tot <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Results in Dataset: 0"
      } else {
        paste0(
          "Total Results in Dataset: ",
          scales::comma(length(tadat$raw$ResultIdentifier))
        )
      }
    })

    output$rec_rem <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Results Flagged for Removal: 0"
      } else {
        paste0(
          "Results Flagged for Removal: ",
          scales::comma(summary_things$rem_rec)
        )
      }
    })

    output$rec_clean <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Results Retained: 0"
      } else {
        paste0("Results Retained: ", scales::comma(summary_things$clean_rec))
      }
    })

    output$site_tot <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites in Dataset: 0"
      } else {
        paste0(
          "Total Sites in Dataset: ",
          scales::comma(length(unique(tadat$raw$MonitoringLocationIdentifier)))
        )
      }
    })

    output$site_rem <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites Flagged for Removal: 0"
      } else {
        paste0(
          "Total Sites Flagged for Removal: ",
          scales::comma(summary_things$rem_site)
        )
      }
    })

    output$site_clean <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites Retained: 0"
      } else {
        paste0(
          "Total Sites Retained: ",
          scales::comma(summary_things$clean_site)
        )
      }
    })

    shiny::observeEvent(input$disclaimer, {
      shiny::showModal(shiny::modalDialog(
        title = "Disclaimer",
        "This United States Environmental Protection Agency (EPA) GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government."
      ))
    })

    # Initially disabled until data exists
    shinyjs::disable("dwn_working")
    shinyjs::disable("dwn_final")
  })
}
## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")

## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")

sort_removals <- function(removal_table) {
  if (length(removal_table) > 0) {
    prefixes <- c("Flag", "Filter")
    fields <- colnames(removal_table)
    results <- data.frame(matrix(
      nrow = nrow(removal_table),
      ncol = length(prefixes)
    ))
    colnames(results) <- prefixes
    results[is.na(results)] <- FALSE

    for (prefix in prefixes) {
      active_cols <- fields[dplyr::starts_with(prefix, vars = fields)]
      if (length(active_cols) > 0) {
        results[prefix] <- apply(
          dplyr::select(removal_table, dplyr::any_of(active_cols)),
          1,
          any
        )
      }
    }
    totals <- rowSums(results)
    results["Flag only"] <- ((totals == 1) & results$Flag)
    results["Flag and Filter"] <- (results$Flag & results$Filter)
    results["Filter only"] <- ((totals == 1) & results$Filter)
    results <- dplyr::select(results, -intersect(prefixes, colnames(results)))
    results$Many <- rowSums(results) > 2
    results$Retained <- !apply(results, 1, any)
    counts <- colSums(results)
    counts <- data.frame(Reason = names(counts), Count = as.vector(counts))
    counts <- counts[(counts$Count > 0), ]
    return(counts)
  }
}
