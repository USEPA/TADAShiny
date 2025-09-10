#' TADA_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_TADA_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(shiny::fluidRow(
    # column(
    # 6,
    style = "padding-left:20px",
    shiny::wellPanel(
      htmltools::h3("Results Summary"),
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
      htmltools::h3("Monitoring Location Summary"),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
        "site_tot"
      )))),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
        "site_rem"
      )))),
      shiny::fluidRow(htmltools::h5(shiny::textOutput(ns(
        "site_clean"
      )))),
      htmltools::hr(),
      htmltools::h3("Download Working or Final Dataset"),
      htmltools::HTML(
        "Download the working dataset as a .xlsx file. This includes all results that were in the
          original data query. In addition, it includes all the TADA flag columns,
          including an overall 'Remove' and 'Removal Reason' column.<br><br>"
      ),
      shiny::fluidRow(column(
        6,
        shiny::actionButton(
          ns("download_working_button"),
          "Working Dataset (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 10px;"
        )
      )),
      htmltools::HTML(
        "Download the final dataset as a .xlsx file. This has been filtered to remove
          all results that the user flagged for removal throughout the application.<br><br>"
      ),
      shiny::fluidRow(column(
        6,
        shiny::actionButton(
          ns("download_final_button"),
          "Final Dataset (.zip)",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-bottom: 10px;"
        )
      )),
      htmltools::h5("Note: A progress file in the .RData format will always be
        included in the download. See the 'Upload Progress File' option on the
        import tab for details on how to use this file to regenerate a dataset
        with the same decisions you made before, or to apply the same user
        selections to a new dataset."),
    ),
    shiny::conditionalPanel("false", shiny::downloadButton(ns("dwn_working"), "Download Working")),
    shiny::conditionalPanel("false", shiny::downloadButton(ns("dwn_final"), "Download Final")),
    shiny::fluidRow(column(
      2, shiny::actionButton(ns("disclaimer"), "DISCLAIMER")
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
    ns <- session$ns
    # reactive list to hold reactive objects specific to this module
    summary_things <- shiny::reactiveValues()
    
    # When data is loaded, enable the download buttons
    shiny::observeEvent(tadat$raw, {
      if (is.null(tadat$raw)) {
        
      } else {
        shinyjs::enable("download_working_button")
        shinyjs::enable("download_final_button")
      }
    })
    
    # Function to determine the download path based on OS
    get_download_path <- function() {
      if (.Platform$OS.type == "windows") {
        one_drive_path <- file.path(Sys.getenv("ONEDRIVE"), "Downloads")
        downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
        return(if (dir.exists(one_drive_path)) one_drive_path else downloads_path)
      } else {
        # For macOS/Linux, default to home Downloads
        downloads_path <- file.path(Sys.getenv("HOME"), "Downloads")
        return(downloads_path)
      }
    }
    
    shiny::observeEvent(input$download_working_button, {
      tryCatch(
        {
          # Determine the path to use
          target_path <- get_download_path()
          
          # Prepare filenames with full paths
          datafile_name <- file.path(target_path, paste0(tadat$default_outfile, "_working", ".xlsx"))
          progress_file_name <- file.path(target_path, paste0(tadat$default_outfile, "_prog.RData"))
          
          # Show progress spinner
          shinybusy::show_modal_spinner(
            spin = "double-bounce",
            color = "#0071bc",
            text = "Preparing files for download...",
            session = shiny::getDefaultReactiveDomain()
          )
          
          # Process data
          out_data <- EPATADA::TADA_OrderCols(tadat$raw)
          summary_things$temp_files <- c(datafile_name, progress_file_name)
          desc <- writeNarrativeDataFrame(tadat)
          dfs <- list(Data = out_data, Parameterization = desc)
          
          # Write files
          writeFile(tadat, progress_file_name)
          writexl::write_xlsx(dfs, path = datafile_name, use_zip64 = TRUE)
          
          # Remove progress spinner
          shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
          
          # Trigger download action
          shinyjs::click("dwn_working")
        },
        error = function(e) {
          shiny::showNotification("Error writing working files. Please submit an issue with a reproducible example: https://github.com/USEPA/TADAShiny/issues")
          print(e)
        },
        warning = function(w) {
          shiny::showNotification("Warning writing working files. Please submit an issue with a reproducible example: https://github.com/USEPA/TADAShiny/issues")
          print(w)
        }
      )
    })
    
    shiny::observeEvent(input$download_final_button, {
      tryCatch(
        {
          # Determine the path to use
          target_path <- get_download_path()
          
          # Prepare filenames with full paths
          datafile_name <- file.path(target_path, paste0(tadat$default_outfile, "_final", ".xlsx"))
          progress_file_name <- file.path(target_path, paste0(tadat$default_outfile, "_prog.RData"))

          # Show progress spinner
          shinybusy::show_modal_spinner(
            spin = "double-bounce",
            color = "#0071bc",
            text = "Preparing files for download...",
            session = shiny::getDefaultReactiveDomain()
          )
          
          # Process data
          out_data <- EPATADA::TADA_OrderCols(tadat$raw[!tadat$raw$TADA.Remove, ])
          summary_things$temp_files <- c(datafile_name, progress_file_name)
          desc <- writeNarrativeDataFrame(tadat)
          dfs <- list(Data = out_data, Parameterization = desc)
          
          # Write files
          writeFile(tadat, progress_file_name)
          writexl::write_xlsx(dfs, path = datafile_name, use_zip64 = TRUE)
          
          # Remove progress spinner
          shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
          
          # Trigger download action
          shinyjs::click("dwn_final")
        },
        error = function(e) {
          shiny::showNotification("Error writing final files. Please submit an issue with a reproducible example: https://github.com/USEPA/TADAShiny/issues")
          print(e)
        },
        warning = function(w) {
          shiny::showNotification("Warning writing final files. Please submit an issue with a reproducible example: https://github.com/USEPA/TADAShiny/issues")
          print(w)
        }
      )
    })
    
    output$dwn_working <- shiny::downloadHandler(
      filename = function() {
        base::paste0(tadat$default_outfile, "_working.zip")
      },
      content = function(fname) {
        utils::zip(zipfile = fname, files = summary_things$temp_files)
      },
      contentType = "application/zip"
    )
    
    output$dwn_final <- shiny::downloadHandler(
      filename = function() {
        base::paste0(tadat$default_outfile, "_final.zip")
      },
      content = function(fname) {
        utils::zip(zipfile = fname, files = summary_things$temp_files)
      },
      contentType = "application/zip"
    )
    
    # calculate the stats needed to fill the summary box
    shiny::observe({
      shiny::req(tadat$raw)
      summary_things$rem_rec <-
        length(tadat$raw$ResultIdentifier[tadat$raw$TADA.Remove ==
                                            TRUE])
      summary_things$clean_rec <-
        length(tadat$raw$ResultIdentifier[tadat$raw$TADA.Remove ==
                                            FALSE])
      clean_sites <-
        unique(tadat$raw$MonitoringLocationIdentifier[tadat$raw$TADA.Remove ==
                                                        FALSE])
      summary_things$clean_site <- length(clean_sites)
      summary_things$rem_site <-
        length(unique(tadat$raw$MonitoringLocationIdentifier[!tadat$raw$MonitoringLocationIdentifier %in%
                                                               clean_sites]))
      summary_things$removals <- sort_removals(tadat$removals)
      
      # enable the Download buttons
      shinyjs::enable("download_working")
      shinyjs::enable("download_final")
    })
    summary_things$removals <- data.frame(matrix(
      ncol = 2,
      nrow = 0,
      dimnames = list(NULL, c("Reason", "Count"))
    ))
    
    output$rec_tot <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Results in Dataset: 0"
      } else {
        base::paste0("Total Results in Dataset: ", scales::comma(length(tadat$raw$ResultIdentifier)))
      }
    })
    
    output$rec_rem <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Results Flagged for Removal: 0"
      } else {
        base::paste0(
          "Results Flagged for Removal: ",
          scales::comma(summary_things$rem_rec)
        )
      }
    })
    
    output$rec_clean <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Results Retained: 0"
      } else {
        base::paste0(
          "Results Retained: ",
          scales::comma(summary_things$clean_rec)
        )
      }
    })
    
    output$site_tot <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites in Dataset: 0"
      } else {
        base::paste0("Total Sites in Dataset: ", scales::comma(length(
          unique(tadat$raw$MonitoringLocationIdentifier)
        )))
      }
    })
    
    output$site_rem <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites Flagged for Removal: 0"
      } else {
        base::paste0(
          "Total Sites Flagged for Removal: ",
          scales::comma(summary_things$rem_site)
        )
      }
    })
    
    output$site_clean <- shiny::renderText({
      if (is.null(tadat$raw)) {
        "Total Sites Retained: 0"
      } else {
        base::paste0(
          "Total Sites Retained: ",
          scales::comma(summary_things$clean_site)
        )
      }
    })
    
    
    shiny::observeEvent(input$disclaimer, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Disclaimer",
          "This United States Environmental Protection Agency (EPA) GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government."
        )
      )
    })
    shinyjs::disable("download_working_button")
    shinyjs::disable("download_final_button")
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
      active_cols <- fields[dplyr::starts_with(prefix, vars = fields)]
      if (length(active_cols) > 0) {
        results[prefix] <-
          apply(dplyr::select(removal_table, active_cols), 1, any)
      }
    }
    totals <- rowSums(results)
    results["Flag only"] <- ((totals == 1) & results$Flag)
    results["Flag and Filter"] <- (results$Flag & results$Filter)
    results["Filter only"] <- ((totals == 1) & results$Filter)
    results <-
      dplyr::select(results, -intersect(prefixes, colnames(results)))
    results$Many <- rowSums(results) > 2
    results$Retained <- !apply(results, 1, any)
    counts <- colSums(results)
    counts <-
      data.frame(Reason = names(counts), Count = as.vector(counts))
    counts <- counts[(counts$Count > 0), ]
    return(counts)
  }
}
## To be copied in the UI
# mod_TADA_summary_ui("TADA_summary_1")

## To be copied in the server
# mod_TADA_summary_server("TADA_summary_1")
