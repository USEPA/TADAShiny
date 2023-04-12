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
  tagList(shiny::fluidRow(htmltools::h3("Categorize Censored Data Records")),
          shiny::fluidRow("Assign each detection limit record in your dataset to non-detect, over-detect, or other using the button below. Once finished a pie chart will display the results and you will have options for simple censored data handling."),
          htmltools::br(),
          shiny::fluidRow(column(4, shiny::fluidRow(shiny::actionButton(ns("id_cens"),"ID Censored Data",shiny::icon("fingerprint"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 br()
                                 ),
                          column(6, shiny::plotOutput(ns("id_censplot")))),
          shiny::fluidRow(htmltools::h3("Handle Censored Data Using Simple Methods")),
          shiny::fluidRow("Use the drop down menus below to pick a simple method for handling non-detects and over-detects in the dataset."),
          htmltools::br(),
          shiny::fluidRow(column(3, selectInput(ns("nd_method"),"Non-Detect Handling Method",choices = c("Multiply detection limit by x","Random number between 0 and detection limit","No change"), multiple = FALSE)),
                          column(3, shiny::uiOutput(ns("nd_mult"))),
                          column(3, selectInput(ns("od_method"),"Over-Detect Handling Method",choices = c("Multiply detection limit by x","No change"), selected = "No change", multiple = FALSE)),
                          column(3, shiny::uiOutput(ns("od_mult")))),
          shiny::fluidRow(column(2, shiny::actionButton(ns("apply_methods"),"Apply Methods to Dataset",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                          column(2, shiny::uiOutput(ns("undo_methods")))),
          htmltools::br(),
          shiny::fluidRow(column(12, shiny::plotOutput(ns("see_det")))),
          htmltools::br(),
          shiny::fluidRow(htmltools::h3("Consider More Complex Censored Data Handling Methods")),
          shiny::fluidRow("Use the picker list below to select grouping columns to create summary table. The summary table shows the number of non- and over-detects in each group, the total number of results in each group, and the percentage of the dataset that is censored. These numbers are then used to suggest a potential statistical censored data method to use. Currently, the user must perform more complex analyses outside of TADAShiny."),
          htmltools::br(),
    shiny::fluidRow(shiny::wellPanel(shiny::fluidRow(column(12,shiny::uiOutput(ns("cens_groups")))),
                                     shiny::fluidRow(column(12,shiny::actionButton(ns("cens_sumbutton"),"ID and Summarize Censored Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))),
                    DT::DTOutput(ns("cens_sumtable")))
    
 
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
      if(length(removed$ResultIdentifier)>0){
        removed$TADA.CensoredData.Flag = "Not screened"
      }
      dat = TADA::idCensoredData(dat)
      dat$Removed = ifelse(dat$TADA.CensoredData.Flag%in%c("Censored but not Categorized","Conflict between Condition and Limit"),TRUE,dat$Removed)
      if(any(dat$Removed==TRUE)){
          shiny::showModal(shiny::modalDialog(
          title = "Detection Limit Data Warning",
          paste0(length(dat$ResultIdentifier[dat$Removed==TRUE])," results were flagged for removal because they have ambiguous and/or unfamiliar detection limits and conditions. These will show up in the pie chart as 'Censored but not Categorized' and 'Conflict between Condition and Limit', but will not be used in the sections below. You may download your dataset for review at any time using the 'Download Working Dataset' button at the bottom of the page.")
        ))
      }
      tadat$raw = plyr::rbind.fill(dat, removed)
      censdat$dat = dat
    })
    
    output$id_censplot = shiny::renderPlot({
      req(censdat$dat)
      piedat = censdat$dat%>%dplyr::group_by(TADA.CensoredData.Flag)%>%dplyr::summarise(num = length(ResultIdentifier))
      piedat$Label = paste0(piedat$TADA.CensoredData.Flag," - ", scales::comma(piedat$num)," results")
      # Basic piechart
      ggplot2::ggplot(piedat, ggplot2::aes(x="", y=num, fill=Label)) +
        ggplot2::geom_bar(stat="identity", width=1, color="white") +
        ggplot2::labs(title="Number of Results per Censored Data Category")+
        ggplot2::coord_polar("y", start=0) +
        ggplot2::scale_fill_brewer(palette = "Blues") +
        ggplot2::theme_void() + # remove background, grid, numeric labels
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 18),legend.title=ggplot2::element_text(size=16), legend.text = ggplot2::element_text(size = 14))  #+
        # ggplot2::geom_text(ggplot2::aes(label = scales::comma(num)), color = "white", size=6,position = ggplot2::position_stack(vjust = 0.5))
    })
    
    output$nd_mult = shiny::renderUI({
      if(input$nd_method=="Multiply detection limit by x"){
        shiny::numericInput(ns("nd_mult"),"Multiplier (x)",value = 0.5, min=0)
      }
    })
    
    output$od_mult = shiny::renderUI({
      if(input$od_method=="Multiply detection limit by x"){
        shiny::numericInput(ns("od_mult"),"Multiplier (x)",value = 1, min=0)
      }
    })
    
    shiny::observeEvent(input$apply_methods,{
      removed = subset(tadat$raw, tadat$raw$Removed==TRUE)
      good = subset(tadat$raw, tadat$raw$Removed==FALSE)
      trans = data.frame(input = c("Multiply detection limit by x","Random number between 0 and detection limit","No change"),actual = c("multiplier","randombelowlimit","as-is"))
      if(is.null(input$nd_mult)){
        nd_multiplier = "null"
      }else{nd_multiplier=input$nd_mult}
      if(is.null(input$od_mult)){
        od_multiplier = "null"
      }else{od_multiplier=input$od_mult}
      good = TADA::simpleCensoredMethods(good,nd_method = trans$actual[trans$input==input$nd_method], nd_multiplier = nd_multiplier, od_method = trans$actual[trans$input==input$od_method], od_multiplier = od_multiplier)
      tadat$raw = plyr::rbind.fill(removed, good)
      
      # create scatter plot dataset
      dat = subset(good, good$TADA.CensoredData.Flag%in%c("Non-Detect","Over-Detect"))
      dat = dat[order(dat$TADA.DetectionQuantitationLimitMeasure.MeasureValue),c("TADA.DetectionQuantitationLimitMeasure.MeasureValue","TADA.ResultMeasureValue")]
      dat$order = 1:dim(dat)[1]
      dat1 = dat%>%dplyr::select(order, TADA.DetectionQuantitationLimitMeasure.MeasureValue)%>%dplyr::rename(value = TADA.DetectionQuantitationLimitMeasure.MeasureValue)
      dat1$Type = "Original Detection Limit Value"
      dat2 = dat%>%dplyr::select(order, TADA.ResultMeasureValue)%>%dplyr::rename(value = TADA.ResultMeasureValue)
      dat2$Type = "Estimated Detection Limit Value"
      dat = plyr::rbind.fill(dat2, dat1)
      censdat$plotdat = dat
    })
    
    output$undo_methods = shiny::renderUI({
      shiny::req(censdat$plotdat)
      shiny::actionButton(ns("undo_methods"),"Undo Method Application",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    shiny::observeEvent(input$undo_methods,{
      censdat$plotdat = NULL
      tadat$raw$TADA.ResultMeasureValue = ifelse(tadat$raw$TADA.ResultMeasureValueDataTypes.Flag=="Result Value/Unit Estimated from Detection Limit",tadat$raw$TADA.DetectionQuantitationLimitMeasure.MeasureValue,tadat$raw$TADA.ResultMeasureValue)
      tadat$raw$TADA.ResultMeasureValueDataTypes.Flag[tadat$raw$TADA.ResultMeasureValueDataTypes.Flag=="Result Value/Unit Estimated from Detection Limit"] = "Result Value/Unit Copied from Detection Limit"
      tadat$raw = tadat$raw%>%dplyr::select(-TADA.CensoredMethod)
    })
    
    output$see_det = shiny::renderPlot({
      shiny::req(censdat$plotdat)
      ggplot2::ggplot(data=censdat$plotdat, ggplot2::aes(x=order, y=value))+
        ggplot2::geom_point(ggplot2::aes(color=Type),pch=1, size=5)+
        ggplot2::scale_colour_manual(values = c("#005ea2","#ff5a5f"))+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::labs(title="Detection limits sorted by value")+
        ggplot2::xlab("Ordered detection limits from lowest to highest value")+
        ggplot2::ylab("Numeric value (log scale)")+
        ggplot2::scale_y_log10(labels = scales::label_log())+
        ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
    })
    
    output$cens_groups = shiny::renderUI({
      shiny::req(censdat$dat)
      ccols = names(tadat$raw)[!names(tadat$raw)%in%c("Removed","tab","TADA.ResultMeasureValue","ResultMeasureValue","ResultIdentifier","TADA.DetectionQuantitationLimitMeasure.MeasureValue","DetectionQuantitationLimitMeasure.MeasureValue")]
      tcols = ccols[grepl("TADA.",ccols)]
      ucols = ccols[!grepl("TADA.",ccols)]
      ccols = c(tcols, ucols)
      shiny::selectizeInput(ns("cens_groups"), label = "Select Grouping Columns for Summarization", choices = ccols, selected = c("TADA.CharacteristicName","TADA.ResultMeasure.MeasureUnitCode","TADA.ResultSampleFractionText","TADA.MethodSpecificationName"), multiple = TRUE)
    })
    
    shiny::observeEvent(input$cens_sumbutton,{
      censdat$summary = TADA::summarizeCensoredData(censdat$dat, spec_cols = input$cens_groups)
    })
    
    output$cens_sumtable = DT::renderDT({
      DT::datatable(censdat$summary,
                    options = list(scrollX=TRUE, pageLength=10,searching = FALSE, order = list(list(length(input$cens_groups), 'desc'))),
                    selection = 'none', rownames=FALSE)
    })
 
  })
}
    
## To be copied in the UI
# mod_censored_data_ui("censored_data_1")
    
## To be copied in the server
# mod_censored_data_server("censored_data_1")
