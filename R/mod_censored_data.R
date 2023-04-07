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
  tagList(shiny::fluidRow(htmltools::h3("Step 1: Categorize Censored Data Records")),
          shiny::fluidRow("Press the button below to assign each detection limit record in your dataset to non-detect, over-detect, or other. Once finished a pie chart will display below and you will have options for simple censored data handling."),
          htmltools::br(),
          shiny::fluidRow(column(4, shiny::fluidRow(shiny::actionButton(ns("id_cens"),"ID Censored Data",shiny::icon("fingerprint"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 br(),
                                 shiny::fluidRow(shiny::uiOutput(ns("rem_probcens")))),
                          column(6, shiny::plotOutput(ns("id_censplot")))),
          shiny::fluidRow(htmltools::h3("Step 2: Handle Censored Data Using Simple Methods")),
          shiny::fluidRow("Use the drop down menus below to pick a single simple method for handling non-detects and over-detects in the dataset."),
          htmltools::br(),
          shiny::fluidRow(column(3, selectInput(ns("nd_method"),"Non-Detect Handling Method",choices = c("Multiply detection limit by X","Random number between 0 and detection limit","No change"), multiple = FALSE)),
                          column(3, shiny::uiOutput(ns("nd_mult"))),
                          column(3, selectInput(ns("od_method"),"Over-Detect Handling Method",choices = c("Multiply detection limit by X","No change"), selected = "No change", multiple = FALSE)),
                          column(3, shiny::uiOutput(ns("od_mult")))),
          shiny::fluidRow(column(4, shiny::actionButton(ns("apply_methods"),"Apply Methods to Dataset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
          shiny::fluidRow(htmltools::h3("Optional Step 3: Consider More Complex Censored Data Handling Methods")),
          shiny::fluidRow("Use the picker list below to select grouping columns to create summary table. The summary table shows the number of non- and over-detects in each group, the total number of results in each group, and the percentage of the dataset that is censored. These numbers are then used to suggest a potential statistical censored data method to use. Currently, the user must perform more complex analyses outside of TADAShiny."),
          htmltools::br(),
    shiny::fluidRow(shiny::wellPanel(shiny::fluidRow(column(12,shiny::uiOutput(ns("cens_groups")))),
                                     shiny::fluidRow(column(12,shiny::actionButton(ns("cens_sumbutton"),"ID and Summarize Censored Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
                    DT::DTOutput(ns("cens_sumtable"), height="400px"))
    
 
  )
}
    
#' censored_data Server Functions
#'
#' @noRd 
mod_censored_data_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # reactive values specific to this module
    censdat = shiny::reactiveValues()
    
    # hit the action button, run idCensoredData on Removed = FALSE dataset, mark flagged data records in tadat$raw as "not screened"
    observeEvent(input$id_cens,{
      dat = subset(tadat$raw, tadat$raw$Removed==FALSE)
      removed = subset(tadat$raw, tadat$raw$Removed==TRUE)
      removed$TADA.CensoredData.Flag = "Not screened"
      dat = TADA::idCensoredData(dat)
      tadat$raw = plyr::rbind.fill(dat, removed)
      censdat$dat = dat
      if(any(censdat$dat$TADA.CensoredData.Flag%in%c("Censored but not Categorized","Conflict between Condition and Limit"))){
        censdat$issues = subset(censdat$dat, censdat$dat$TADA.CensoredData.Flag%in%c("Censored but not Categorized","Conflict between Condition and Limit"))
        shiny::showModal(shiny::modalDialog(
          title = "Warning",
          "Your dataset contains censored data with detection limits that are not documented in TADA and/or results with conflicting detection limits and conditions. Please review the pie chart and/or download your dataset to view these results. If you'd like to flag these results for removal, click the 'Remove Suspect Censored Data' button."
        ))
      }
    })
    
    output$id_censplot = shiny::renderPlot({
      req(censdat$dat)
      piedat = censdat$dat%>%dplyr::group_by(TADA.CensoredData.Flag)%>%dplyr::summarise(num = length(ResultIdentifier))
      # Basic piechart
      ggplot2::ggplot(piedat, ggplot2::aes(x="", y=num, fill=TADA.CensoredData.Flag)) +
        ggplot2::geom_bar(stat="identity", width=1, color="white") +
        ggplot2::labs(title="Number of Results per Censored Data Category")+
        ggplot2::coord_polar("y", start=0) +
        ggplot2::scale_fill_brewer(palette = "Dark2") +
        ggplot2::theme_void() + # remove background, grid, numeric labels
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 18),legend.title=ggplot2::element_text(size=16), legend.text = ggplot2::element_text(size = 14)) +
        ggplot2::geom_text(ggplot2::aes(label = scales::comma(num)), color = "white", size=6,position = ggplot2::position_stack(vjust = 0.5))
    })
    
    output$rem_probcens = shiny::renderUI({
      shiny::req(censdat$issues)
      shiny::actionButton(ns("rem_probcens"),"Remove Suspect Censored Data",shiny::icon("circle-xmark"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    shiny::observeEvent(input$rem_probcens,{
      tadat$raw$Removed = ifelse(tadat$raw$ResultIdentifier%in%censdat$issues$ResultIdentifier,TRUE,tadat$raw$Removed)
    })
    
    output$nd_mult = shiny::renderUI({
      if(input$nd_method=="Multiply detection limit by X"){
        shiny::numericInput(ns("nd_mult"),"Multiplier",value = 0.5, min=0)
      }
    })
    
    output$od_mult = shiny::renderUI({
      if(input$od_method=="Multiply detection limit by X"){
        shiny::numericInput(ns("od_mult"),"Multiplier",value = 1, min=0)
      }
    })
    
    shiny::observeEvent(input$apply_methods,{
      
      
    })
    
    output$cens_groups = shiny::renderUI({
      shiny::req(censdat$dat)
      ccols = names(censdat$dat)[!names(tadat$raw)%in%c("Removed","tab","TADA.ResultMeasureValue","ResultMeasureValue","ResultIdentifier","TADA.DetectionQuantitationLimitMeasure.MeasureValue","DetectionQuantitationLimitMeasure.MeasureValue")]
      tcols = ccols[grepl("TADA.",ccols)]
      ucols = ccols[!grepl("TADA.",ccols)]
      ccols = c(tcols, ucols)
      shinyWidgets::pickerInput(ns("cens_groups"), label = "Select Grouping Columns for Summarization", choices = ccols, selected = c("TADA.CharacteristicName","TADA.ResultMeasure.MeasureUnitCode","TADA.ResultSampleFractionText","TADA.MethodSpecificationName"), multiple = TRUE)
    })
    
    shiny::observeEvent(input$cens_sumbutton,{
      censdat$summary = TADA::summarizeCensoredData(censdat$dat, spec_cols = input$cens_groups)
    })
    
    output$cens_sumtable = DT::renderDT({
      DT::datatable(censdat$summary,
                    options = list(dom="t", scrollX=TRUE, scrollY="400px", pageLength=10,
                                   rownames= FALSE,
                                   selection = 'none'))
    })
 
  })
}
    
## To be copied in the UI
# mod_censored_data_ui("censored_data_1")
    
## To be copied in the server
# mod_censored_data_server("censored_data_1")
