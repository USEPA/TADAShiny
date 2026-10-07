#' Summarize Total Nitrogen and Phosphorus UI Function
#'
#' @description A shiny Module to manage creating sum values of Total Nitrogen and Total Phosphorus.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_TN_and_TP_summation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::h3("1. Total Nitrogen and Phosphorus Summation"),
    htmltools::p(
      "Data generators commonly monitor for several nutrient subspecies that, when added together,
                 can be used to estimate a total nitrogen or phosphorus value. TADA uses the logic provided in
                 ECHO's Nurient Aggregation page (see: https://echo.epa.gov/trends/loading-tool/resources/nutrient-aggregation)
                 to rank and sum subspecies for a given day, location, depth, activity media subdivision, and unit.
                 Total Nitrogen and Total Phosphorus values are added as new results in the dataset.
                 Users may view the nutrient aggregation reference sheet by clicking 'See Summation Reference'.
                 Once data are harmonized, the user may then summarize total N and P.",
      htmltools::strong("NOTE: "),
      "When two or more measurements of the same substance occur on the same day at the same location,
                 the function uses the maximum of the group of values to calculate a total nutrient value."
    ),
    shiny::fluidRow(shiny::column(
      3,
      htmltools::div(style = "margin-top:20px"),
      shiny::downloadButton(
        ns("sum_dwn"),
        "See Summation Reference (.csv)",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )),
    htmltools::br(),
    shiny::fluidRow(shiny::column(
      3,
      htmltools::div(style = "margin-top:20px"),
      shiny::uiOutput(ns("sum_apply"))
    )),
    htmltools::br()
  )
}

#' harmonize_np Server Functions
#'
#' @noRd
mod_TN_and_TP_summation_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sum_dwn <- shiny::downloadHandler(
      filename = function() {
        "TADA_NPSummationKey.csv"
      },
      content = function(file) {
        utils::write.csv(
          EPATADA::TADA_GetNutrientSummationRef(),
          file,
          row.names = FALSE
        )
      }
    )
    
    output$sum_apply <- shiny::renderUI({
      if ("TADA.Harmonized.Flag" %in% names(tadat$raw)) {
        shiny::actionButton(
          ns("sum_apply"),
          "Perform Total N and P Summations",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      }
    })
    
    shiny::observeEvent(input$sum_apply, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Calculating Total N and P...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      out <- tryCatch({
        # Split current data into rows to keep and rows removed
        dat <- subset(tadat$raw, tadat$raw$TADA.Remove == FALSE)
        rem <- subset(tadat$raw, tadat$raw$TADA.Remove == TRUE)
        
        # Run the nutrient summation on keepable rows
        dat <- EPATADA::TADA_CalculateTotalNP(dat, daily_agg = "max")
        dat$TADA.Remove[is.na(dat$TADA.Remove)] <- FALSE
        
        # Count newly created rows for the success message
        newrowlen <- sum(
          dat$TADA.NutrientSummation.Flag %in%
            "New row added: Nutrient summation from one or more subspecies.",
          na.rm = TRUE
        )
        
        list(dat = dat, rem = rem, newrowlen = newrowlen)
      }, error = function(e) {
        message("TN/TP summation error: ", conditionMessage(e))
        NULL
      })
      
      shinybusy::remove_modal_spinner(
        session = shiny::getDefaultReactiveDomain()
      )
      
      if (is.null(out)) {
        shiny::showModal(shiny::modalDialog(
          title = "TN/TP Summation Error",
          "An error occurred while calculating Total N and P. Check server logs."
        ))
        return()
      }
      
      # Rebuild raw from the updated kept data + removed rows
      tadat$raw <- plyr::rbind.fill(out$dat, out$rem)
      tadat$raw <- EPATADA::TADA_OrderCols(tadat$raw)
      
      # Rebuild removals so row count matches the new raw
      tadat$removals <- sync_removals(tadat$raw, tadat$removals)
      
      # Optional: ensure removal reason exists after raw changes
      if (!"TADA.RemovalReason" %in% names(tadat$raw)) {
        tadat$raw$TADA.RemovalReason <- NA_character_
      }
      
      shiny::showModal(shiny::modalDialog(
        title = "Success! Calculations Complete.",
        paste0(
          scales::comma(out$newrowlen),
          " Total Nitrogen and/or Total Phosphorus results calculated."
        )
      ))
      
      shinyjs::disable("sum_apply")
    })
  })
}