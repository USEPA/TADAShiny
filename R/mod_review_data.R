#' review_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_review_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::h3("Removal Review"),
    htmltools::HTML(
      "Use this tab to review flagging and filter decisions and explore the filtered dataset. Click the button below to begin."
    ),
    shiny::fluidRow(column(
      4,
      shiny::actionButton(ns("review_go"), "Load Review Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )),
    htmltools::br(),
    shiny::fluidRow(column(
      8, shiny::plotOutput(ns("review_barchar"), height = "500px")
    )),
    shiny::fluidRow(column(12, shiny::plotOutput(ns(
      "reason_barchar"
    )))),
    htmltools::HTML(
      "<B>Note:</B> This pie chart shows the number of results flagged/filtered for each reason. Some results may be removed for multiple reasons. Because of this, the total number of flagged results in this pie chart is equal to or greater than the number of unique results removed."
    )
  )
}

#' review_data Server Functions
#'
#' @noRd
mod_review_data_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    review_things <- shiny::reactiveValues()

    shiny::observeEvent(input$review_go, {
      removals <- tadat$removals
      sel <- which(removals == TRUE, arr.ind = TRUE)
      # Bombing here
      if (length(sel) > 0) {
        removals[sel] <- names(removals)[sel[, "col"]]
        removals[removals == FALSE] <- ""
        tadat$raw$TADA.RemovalReason <- apply(
          removals, 1,
          function(row) {
            paste(row[nzchar(row)], collapse = ", ")
          }
        )
      } else {
        tadat$raw$TADA.RemovalReason <- NA
      }

      # data for bar chart - this is real rough
      step_rems <- sort_removals(tadat$removals)
      total <- dim(tadat$raw)[1]
      flag <-
        ifelse(length(step_rems$Count[step_rems$Reason %in% "Flag only"]) > 0, step_rems$Count[step_rems$Reason %in% "Flag only"], 0)
      filtflag <-
        ifelse(length(step_rems$Count[step_rems$Reason %in% "Flag and Filter"]) > 0, step_rems$Count[step_rems$Reason %in% "Flag and Filter"], 0)
      filter <-
        ifelse(length(step_rems$Count[step_rems$Reason %in% "Filter only"]) > 0, step_rems$Count[step_rems$Reason %in% "Filter only"], 0)
      mrfl <- total - flag - filtflag
      mrfi <- mrfl - filter

      step_rems_plot <-
        data.frame(
          Step = c(
            "1: Starting Total",
            "2: Measurements Retained After Flagging",
            "3: Measurements Retained After Filtering"
          ),
          Count = c(total, mrfl, mrfi)
        )
      step_rems_plot$Step <-
        factor(
          step_rems_plot$Step,
          levels = c(
            "1: Starting Total",
            "2: Measurements Retained After Flagging",
            "3: Measurements Retained After Filtering"
          )
        )
      review_things$step_rems_plot <- step_rems_plot

      # data for removal reason bar column chart
      rem_reas <-
        data.frame(
          Reason = names(tadat$removals),
          Count = apply(tadat$removals, 2, sum)
        )
      rem_reas <- subset(rem_reas, rem_reas$Count > 0)
      if (nrow(rem_reas) > 0) {
        review_things$rem_reas <- rem_reas
      } else {
        review_things$rem_reas <- data.frame(Reason = "No Removals", Count = 1)
      }
    })

    # characteristics bar chart showing top characteristics by result number in dataset
    output$review_barchar <- shiny::renderPlot({
      shiny::req(review_things$step_rems_plot)
      dat <- review_things$step_rems_plot |>
        dplyr::mutate(num_chr = paste0("n = ", Count)) |>
        dplyr::rowwise() |>
        dplyr::mutate(Step_wrap = stringr::str_wrap(string = Step, width = 30))
      ggplot2::ggplot(dat, ggplot2::aes(x = Step_wrap, y = Count)) +
        ggplot2::geom_col(width = 0.75,
                          fill = "#005ea2",
                          color = "black") +
        ggplot2::geom_text(ggplot2::aes(label = num_chr),
                           vjust = -0.5,
                           size = 5) +
        ggplot2::labs(title = "Results Retained Following Flagging/Filtering Steps", x = "Step Description", y = "Count") +
        ggplot2::theme_classic() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 18),
                       axis.title = ggplot2::element_text(size = 16),
                       axis.text = ggplot2::element_text(size = 14),
                       legend.position = "none")
    })

    # column bar chart showing the reasons why data was removed
    output$reason_barchar <- shiny::renderPlot({
      shiny::req(review_things$rem_reas)

      dat <- review_things$rem_reas |>
        dplyr::mutate(Legend_raw = dplyr::if_else(is.na(Reason), "Not Applicable", paste0(Reason)),
                      num_chr = paste0("n = ", Count)) |>
        dplyr::rowwise() |>
        dplyr::mutate(Legend = stringr::str_wrap(string = Legend_raw, width = 30))

      # define number of colors required for bar chart
      colorCount <- length(unique(dat$Legend))

      # define color palette
      getPalette <-
        grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))

      # create column bar chart
      ggplot2::ggplot(dat, ggplot2::aes(x = Legend, y = Count, fill = Legend)) +
        ggplot2::scale_fill_manual(values = getPalette(colorCount), name = "Removal Reasons") +
        ggplot2::geom_col(width = 0.75,
                          color = "black") +
        ggplot2::geom_text(ggplot2::aes(label = num_chr),
                           vjust = -0.5,
                           size = 5) +
        ggplot2::labs(title = "Reasons for Removal of Results", x = "Reason", y = "Count") +
        ggplot2::theme_classic() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 18),
          axis.title = ggplot2::element_text(size = 16),
          axis.text = ggplot2::element_text(size = 14),
          legend.title = ggplot2::element_text(size = 16),
          legend.text = ggplot2::element_text(size = 16))
    })
  })
}

## To be copied in the UI
# mod_review_data_ui("review_data_1")

## To be copied in the server
# mod_review_data_server("review_data_1")
