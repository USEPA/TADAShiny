#' flag_sites UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_flag_sites_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      style = "display: none;",
      shinyWidgets::prettySwitch("dummy", label = NULL)
    ),
    htmltools::HTML("<h3>Flagged Sites:</h3>"),
    htmltools::HTML(
      "Review list of stations with relevant metadata and group nearby stations if needed."
    ),
    htmltools::div(style = "margin-bottom:10px"),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(ns("applyChanges"),
                          "Apply Changes",
                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )),
    htmltools::div(style = "margin-bottom:10px"),
    DT::DTOutput(ns("flagTable")),
  )
}
    
#' flag_sites Server Functions
#'
#' @noRd 
mod_flag_sites_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    flags <- shiny::reactiveValues()
    values <- shiny::reactiveValues()
    values$n_fails <- integer(length(n_switches))
    tadat$selected_flags <- character()
    tadat$switch_defaults <- prompt_table$Level != "Optional"
    switch_disabled <- prompt_table$Level == "Required"
    
    flagSwitch <- function(len) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        switch_name <- paste0("switch_", i)
        if (!(i %in% which(unlist(switch_disabled)))) {
          inputs[i] <- as.character(
            shinyWidgets::prettySwitch(
              ns(switch_name),
              label = NULL,
              value = tadat$switch_defaults[i],
              status = "primary",
              fill = TRUE
            )
          )
        } else {
          inputs[i] <- "n/a"
        }
      }
      inputs
    }
    
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value <- input[[paste0(id, i)]]
        if (is.null(value)) {
          FALSE
        } else {
          value
        }
      }))
    }
    
    
    
    
    output$flagTable <- DT::renderDT(
      shiny::isolate(switchTable()),
      escape = FALSE,
      selection = "none",
      colnames = c(
        "Flag reason",
        "Results affected",
        "Required/Optional",
        "Switch 'on' to flag for removal"
      ),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        preDrawCallback = DT::JS(
          "function() { Shiny.unbindAll(this.api().table().node()); }"
        ),
        drawCallback = DT::JS(
          "function() { Shiny.bindAll(this.api().table().node()); } "
        )
      )
    )
    
    
  })
}
    
## To be copied in the UI
# mod_flag_sites_ui("flag_sites_1")
    
## To be copied in the server
# mod_flag_sites_server("flag_sites_1")
