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
      # a modal that pops up showing it's working on calculating Total N and P
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Calculating Total N and P...",
        session = shiny::getDefaultReactiveDomain()
      )

      dat <- subset(tadat$raw, tadat$raw$TADA.Remove == FALSE)
      rem <- subset(tadat$raw, tadat$raw$TADA.Remove == TRUE)
      dat <- EPATADA::TADA_CalculateTotalNP(dat, daily_agg = "max")
      dat$TADA.Remove[is.na(dat$TADA.Remove)] <- FALSE

      # add new measurements to tadat$removals, all equal FALSE
      ## NOTE THAT THIS ASSUMES NEWLY CREATED RESULTS FROM TOTAL NP WILL NECESSARILY BE ADDED TO END OF TADAT$RAW DATA FRAME
      ncols <- ncol(tadat$removals)
      nrows <- length(dat$ResultIdentifier[grepl(
        "TADA-",
        dat$ResultIdentifier
      )])
      new_df <- as.data.frame(matrix(FALSE, ncol = ncols, nrow = nrows))
      names(new_df) <- names(tadat$removals)
      tadat$removals <- plyr::rbind.fill(tadat$removals, new_df)
      tadat$raw <- plyr::rbind.fill(dat, rem)
      tadat$raw <- EPATADA::TADA_OrderCols(tadat$raw)
      # Need to update TADA.NutrientSummation.Flag outputs in EPATADA R package function to differentiate TN and TP
      # nitrolen <- length(dat$TADA.NutrientSummation.Flag[dat$TADA.NutrientSummation.Flag %in% c("New row added: Nutrient summation from one or more subspecies.")])
      # phoslen <- length(dat$TADA.NutrientSummation.Flag[dat$TADA.NutrientSummation.Flag %in% c("New row added: Nutrient summation from one or more subspecies.")])
      newrowlen <- length(dat$TADA.NutrientSummation.Flag[
        dat$TADA.NutrientSummation.Flag %in%
          c("New row added: Nutrient summation from one or more subspecies.")
      ])
      # remove the modal once the dataset has been harmonized
      shinybusy::remove_modal_spinner(
        session = shiny::getDefaultReactiveDomain()
      )

      shiny::showModal(shiny::modalDialog(
        title = "Success! Calculations Complete.",
        # base::paste0(scales::comma(nitrolen), " Total Nitrogen results calculated and ", scales::comma(phoslen), " Total Phosphorus results calculated.")
        base::paste0(
          scales::comma(newrowlen),
          " Total Nitrogen and/or Total Phosphorus results calculated."
        )
      ))
      shinyjs::disable("sum_apply")
    })
  })
}

## To be copied in the UI
# mod_TN_and_TP_summation_ui("TN_and_TP_summation_1")

## To be copied in the server
# mod_TN_and_TP_summation_server("TN_and_TP_summation_1")
