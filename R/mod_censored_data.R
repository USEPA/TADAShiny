#' censored_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

nd_method_options <-
  c("Multiply detection limit by x",
    "Random number between 0 and detection limit",
    "No change")
od_method_options <- c("Multiply detection limit by x", "No change")

mod_censored_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(htmltools::h3("Censored Data Categories")),
    shiny::fluidRow(
      "TADAdataRetrieval assigns each result in your dataset to non-detect, over-detect, other, or uncensored. The pie chart below displays the relative proportions of results in each category. Please note that detection limit data with conflicts or data quality issues are not displayed in this pie chart or handled in the methods below."
    ),
    htmltools::br(),
    shiny::fluidRow(column(12, shiny::plotOutput(
      ns("id_censplot")
    ))),
    htmltools::br(),
    shiny::fluidRow(htmltools::h3(
      "Handle Censored Data Using Simple Methods"
    )),
    shiny::fluidRow(
      "Use the drop down menus below to pick a simple method for handling non-detects and over-detects in the dataset. When you press 'Apply Methods to Dataset', a table will appear below with the first 10 detection limit results, showing their initial values and estimated values."
    ),
    htmltools::br(),
    shiny::fluidRow(
      column(
        3,
        shiny::selectizeInput(
          ns("nd_method"),
          "Non-Detect Handling Method",
          choices = nd_method_options,
          selected = nd_method_options[1],
          multiple = TRUE,
          options = list(maxItems = 1)
        )
      ),
      column(3, shiny::uiOutput(ns("nd_mult"))),
      column(
        3,
        shiny::selectizeInput(
          ns("od_method"),
          "Over-Detect Handling Method",
          choices = od_method_options,
          selected = od_method_options[2],
          multiple = TRUE,
          options = list(maxItems = 1)
        )
      ),
      column(3, shiny::uiOutput(ns("od_mult")))
    ),
    shiny::fluidRow(
      column(
        3,
        shiny::actionButton(ns("apply_methods"), "Apply Methods to Dataset", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      column(3, shiny::uiOutput(ns("undo_methods")))
    ),
    htmltools::br(),
    shiny::fluidRow(column(12, DT::DTOutput(ns(
      "see_det"
    )))),
    htmltools::br(),
    shiny::fluidRow(
      htmltools::h3("Consider More Complex Censored Data Handling Methods")
    ),
    shiny::fluidRow(
      "Use the picker list below to select grouping columns to create summary table. The summary table shows the number of non- and over-detects in each group, the total number of results in each group, the number of detection limit types (censoring levels) and the percentage of the dataset that is censored. These numbers are then used to suggest a potential statistical censored data method to use. Currently, the user must perform more complex analyses outside of TADAShiny."
    ),
    htmltools::br(),
    shiny::fluidRow(shiny::wellPanel(
      shiny::fluidRow(column(12, shiny::uiOutput(
        ns("cens_groups")
      ))),
      shiny::fluidRow(column(
        12,
        shiny::actionButton(ns("cens_sumbutton"), "ID and Summarize Censored Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ))
    )),
    shiny::fluidRow(DT::DTOutput(ns("cens_sumtable")), width = 600)
  )
}

#' censored_data Server Functions
#'
#' @noRd
mod_censored_data_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # initialize dropdown values
    
    # reactive values specific to this module
    censdat <- shiny::reactiveValues()
    
    # update dataset when on censored data page
    shiny::observeEvent(tadat$tab, {
      shiny::req(tadat$raw)
      if (tadat$tab == "Censored") {
        # dat = subset(tadat$raw, tadat$raw$TADA.Remove==FALSE) # first, get rid of anything that has removed=FALSE flag
        # dat$TADA.Remove = ifelse(!dat$TADA.CensoredData.Flag%in%c("Non-Detect","Over-Detect","Uncensored", "Other Condition/Limit Populated"),TRUE,dat$TADA.Remove)
        # if(any(dat$TADA.Remove==TRUE)){ # let users know when there are "problem" censored data results that will be flagged for removal.
        #   shiny::showModal(shiny::modalDialog(
        #     title = "Detection Limit Data Warning",
        #     paste0(length(dat$ResultIdentifier[dat$TADA.Remove==TRUE])," results were flagged for removal because they have conflicting, ambiguous and/or unfamiliar detection limits and conditions. These will show up in the pie chart, but only 'Non-Detect', 'Over-Detect', and 'Uncensored' results will be used in the sections below. You may download your dataset for review at any time using the 'Download Working Dataset' button at the bottom of the page.")
        #   ))
        # }
        censdat$dat <-
          subset(tadat$raw, tadat$raw$TADA.Remove == FALSE) # however, this reactive object has all of the data that were not previously removed and do not have ambiguous detection limit data. This is the "clean" dataset
      }
    })
    
    # pie chart showing breakdown of censored/uncensored data passed through idCensoredData function
    output$id_censplot <- shiny::renderPlot({
      shiny::req(censdat$dat)
      piedat <- censdat$dat %>%
        dplyr::group_by(TADA.CensoredData.Flag) %>%
        dplyr::summarise(num = length(ResultIdentifier))
      piedat$Label <-
        paste0(piedat$TADA.CensoredData.Flag,
               " - ",
               scales::comma(piedat$num),
               " results")
      # Basic piechart
      ggplot2::ggplot(piedat, ggplot2::aes(x = "", y = num, fill = Label)) +
        ggplot2::geom_bar(stat = "identity",
                          width = 1,
                          color = "white") +
        ggplot2::labs(title = "Number of Results per Censored Data Category") +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::scale_fill_brewer(palette = "Dark2") +
        ggplot2::theme_void() + # remove background, grid, numeric labels
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 18),
          legend.title = ggplot2::element_text(size = 16),
          legend.text = ggplot2::element_text(size = 14)
        ) #+
      # ggplot2::geom_text(ggplot2::aes(label = scales::comma(num)), color = "white", size=6,position = ggplot2::position_stack(vjust = 0.5))
    })
    
    
    # this adds the multiplier numeric input next to the method selection if the nd method selected is to mult det limit by x
    
    output$nd_mult <- shiny::renderUI({
      init_val = tadat$nd_mult
      if (is.null(init_val)){
        init_val = 0.5
      }
      if (input$nd_method == nd_method_options[1]) {
        shiny::numericInput(ns("nd_mult"),
                            "Multiplier (x)",
                            value = init_val,
                            min = 0)
      }
    })
    
    # this adds the multiplier numeric input next to the method selection if the od method selected is to mult det limit by x
    output$od_mult <- shiny::renderUI({
      init_val = tadat$od_mult
      if (is.null(init_val)){
        init_val = 0.5
      }
      if (input$od_method == od_method_options[1]) {
        shiny::numericInput(ns("od_mult"),
                            "Multiplier (x)",
                            value = init_val,
                            min = 0)
      }
    })
    
    
    # initialize global variables for saving/loading
    
    tadat$censor_applied = FALSE
    
    shiny::observeEvent(tadat$load_progress_file, {
      if (!is.na(tadat$load_progress_file)) {
        shiny::updateSelectizeInput(session,
                                    "nd_method",
                                    choices = nd_method_options,
                                    selected = tadat$nd_method)
        shiny::updateSelectizeInput(session,
                                    "od_method",
                                    choices = od_method_options,
                                    selected = tadat$od_method)
        shiny::updateNumericInput(session, "nd_mult", value = tadat$nd_mult)
        shiny::updateNumericInput(session, "od_mult", value = tadat$od_mult)
      }
    })
    
    # Make this part more concise?
    shiny::observeEvent(input$nd_method, {
      tadat$nd_method = input$nd_method
    })
    
    shiny::observeEvent(input$nd_mult, {
      tadat$nd_mult = input$nd_mult
    })
    
    shiny::observeEvent(input$od_method, {
      tadat$od_method = input$od_method
    })
    
    shiny::observeEvent(input$od_mult, {
      tadat$od_mult = input$od_mult
      })
    
    
    # Button to apply the simple methods to the nd and od results in the dataset.
    shiny::observeEvent(input$apply_methods, {
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Applying selected methods...",
        session = shiny::getDefaultReactiveDomain()
      )
      removed <-
        subset(tadat$raw, tadat$raw$TADA.Remove == TRUE) # first, remove results we dont want to handle at all
      good <-
        subset(tadat$raw, tadat$raw$TADA.Remove == FALSE) # keep the "goods" that will be run through the simpleCensoredMethods function
      trans <-
        data.frame(
          input = nd_method_options,
          actual = c("multiplier", "randombelowlimit", "as-is")
        )
      if (is.null(input$nd_mult)) {
        # these if's get the reactive inputs into a format that the TADA function will understand
        nd_multiplier <- "null"
      } else {
        nd_multiplier <- input$nd_mult
      }
      if (is.null(input$od_mult)) {
        od_multiplier <- "null"
      } else {
        od_multiplier <- input$od_mult
      }
      good <-
        TADA::TADA_SimpleCensoredMethods(
          good,
          nd_method = trans$actual[trans$input == input$nd_method],
          nd_multiplier = nd_multiplier,
          od_method = trans$actual[trans$input == input$od_method],
          od_multiplier = od_multiplier
        )
      tadat$raw <-
        plyr::rbind.fill(removed, good) # stitch good and removed datasets back together in tadat$raw
      tadat$raw <- TADA::TADA_OrderCols(tadat$raw)
      
      # create dataset displayed in table below
      dat <-
        subset(good,
               good$TADA.CensoredData.Flag %in% c("Non-Detect", "Over-Detect"))
      dat <-
        dat[, c(
          "ResultIdentifier",
          "TADA.CharacteristicName",
          "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
          "DetectionQuantitationLimitMeasure.MeasureUnitCode",
          "TADA.ResultMeasureValue",
          "TADA.ResultMeasure.MeasureUnitCode"
        )]
      dat <-
        dat %>% dplyr::rename(
          "Estimated Detection Limit Value" = TADA.ResultMeasureValue,
          "Original Detection Limit Value" = TADA.DetectionQuantitationLimitMeasure.MeasureValue,
          "Original Unit" = DetectionQuantitationLimitMeasure.MeasureUnitCode,
          "Estimated Unit" = TADA.ResultMeasure.MeasureUnitCode
        )
      censdat$exdat <-
        dat[1:10, ] # just show the first 10 records so user can see what happened to data
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      tadat$censor_applied = TRUE
    })
    
    # this button appears after someone has applied the OD/ND methods, in case they want to undo and try another method instead
    output$undo_methods <- shiny::renderUI({
      shiny::req(censdat$exdat)
      shiny::actionButton(ns("undo_methods"), "Undo Method Application", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    # executes the undo if undo methods button is pressed.
    shiny::observeEvent(input$undo_methods, {
      censdat$exdat <- NULL # reset exdat
      tadat$raw$TADA.ResultMeasureValue <-
        ifelse(
          tadat$raw$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Estimated from Detection Limit",
          tadat$raw$TADA.DetectionQuantitationLimitMeasure.MeasureValue,
          tadat$raw$TADA.ResultMeasureValue
        ) # reset to detection quantitation limit value
      tadat$raw$TADA.ResultMeasureValueDataTypes.Flag[tadat$raw$TADA.ResultMeasureValueDataTypes.Flag == "Result Value/Unit Estimated from Detection Limit"] <-
        "Result Value/Unit Copied from Detection Limit" # reset data types flag to what it was before simpleCensoredMethods function run
      print(1234)
      print(nrow(tadat$raw))
      tadat$raw <- tadat$raw %>% dplyr::select(-TADA.CensoredMethod)
      print(nrow(tadat$raw))
      tadat$censor_applied = FALSE
    })
    
    # creates a nice table showing an example of how censored data were changed.
    output$see_det <- DT::renderDT({
      shiny::req(censdat$exdat)
      DT::datatable(
        censdat$exdat[1:10, ],
        options = list(
          dom = "t",
          scrollX = TRUE,
          pageLength = 10,
          searching = FALSE
        ),
        selection = "none",
        rownames = FALSE
      )
    })
    
    # from the clean dataset, get all of the column names someone might want to group by when summarizing their data for use in more advanced censored data methods.
    output$cens_groups <- shiny::renderUI({
      shiny::req(censdat$dat)
      ccols <- names(tadat$raw)[!names(tadat$raw) %in% c(
        "TADA.Remove",
        "TADAShiny.tab",
        "TADA.ResultMeasureValue",
        "ResultMeasureValue",
        "ResultIdentifier",
        "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
        "DetectionQuantitationLimitMeasure.MeasureValue"
      )] # remove the columns that are generally unique to each result from consideration. Why would someone want to group by result value or identifier? Then every summary would be unique to one value...not a "summary"
      tcols <-
        ccols[grepl("TADA.", ccols)] # put all of the TADA columns at the top of the selection drop down
      ucols <-
        ccols[!grepl("TADA.", ccols)] # then have the WQP columns
      ccols <-
        c(tcols, ucols) # string them back together in one vector used in the selection widget below
      shiny::selectizeInput(
        ns("cens_groups"),
        label = "Select Grouping Columns for Summarization",
        choices = ccols,
        selected = c("TADA.ComparableDataIdentifier"),
        multiple = TRUE
      )
    })
    
    # runs the summary function when cens button is pushed following group selection
    shiny::observeEvent(input$cens_sumbutton, {
      summary <-
        TADA::TADA_Stats(censdat$dat, group_cols = input$cens_groups)
      censdat$summary <-
        summary[, !names(summary) %in% c(
          "UpperFence",
          "LowerFence",
          "Min",
          "Max",
          "Mean",
          "Percentile_5th",
          "Percentile_10th",
          "Percentile_15th",
          "Percentile_25th",
          "Percentile_50th_Median",
          "Percentile_75th",
          "Percentile_85th",
          "Percentile_95th",
          "Percentile_98th"
        )]
    })
    
    # creates summary table complete with csv button in case someone wants to
    # download the summary table
    output$cens_sumtable <- DT::renderDT({
      DT::datatable(
        censdat$summary,
        extensions = "Buttons",
        options = list(
          dom = "Blftipr",
          scrollX = TRUE,
          pageLength = 10,
          searching = FALSE,
          order = list(list(length(
            input$cens_groups
          ), "desc")),
          buttons = c("csv")
        ),
        selection = "none",
        rownames = FALSE
      )
    })
  })
}

## To be copied in the UI
# mod_censored_data_ui("censored_data_1")

## To be copied in the server
# mod_censored_data_server("censored_data_1")
