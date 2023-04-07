#' censored_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_censored_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::sidebarPanel(shiny::fluidRow(column(12,
                                               shiny::uiOutput(ns("cens_groups")))),
                        shiny::fluidRow(column(12,shiny::actionButton(ns("cens_sumbutton"),"ID and Summarize Censored Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
    shiny::mainPanel(shiny::fluidRow(column(12, DT::DTOutput(ns("cens_sumtable"), height="400px"))))
    
 
  )
}
    
#' censored_data Server Functions
#'
#' @noRd 
mod_censored_data_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    censdat = shiny::reactiveValues()
    
    output$cens_groups = shiny::renderUI({
      shiny::req(tadat$raw)
      ccols = names(tadat$raw)[!names(tadat$raw)%in%c("Removed","tab","TADA.ResultMeasureValue","ResultMeasureValue","ResultIdentifier","TADA.DetectionQuantitationLimitMeasure.MeasureValue","DetectionQuantitationLimitMeasure.MeasureValue")]
      tcols = ccols[grepl("TADA.",ccols)]
      ucols = ccols[!grepl("TADA.",ccols)]
      ccols = c(tcols, ucols)
      shinyWidgets::pickerInput("cens_groups", label = "Select Grouping Columns for Summarization", choices = ccols, selected = c("TADA.CharacteristicName","TADA.ResultMeasure.MeasureUnitCode","TADA.ResultSampleFractionText","TADA.MethodSpecificationName"), multiple = TRUE)
    })
    
    shiny::observeEvent(input$cens_sumbutton,{
      dat = subset(tadat$raw, tadat$raw$Removed=="FALSE")
      censdat$dat = TADA::idCensoredData(dat)
      censdat$summary = TADA::summarizeCensoredData(censdat$dat, spec_cols = input$cens_groups)
    })
    
    output$cens_sumtable = DT::renderDT({
      DT::datatable(censdat$summary,
                    options = list(dom="t", scrollY=TRUE, pageLength=10,
                                   rownames= FALSE,
                                   selection = 'none'))
    })
 
  })
}
    
## To be copied in the UI
# mod_censored_data_ui("censored_data_1")
    
## To be copied in the server
# mod_censored_data_server("censored_data_1")
