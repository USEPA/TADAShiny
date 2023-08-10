#' harmonize_np UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_harmonize_np_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::h3("1. Synonym Harmonization"),
    htmltools::HTML("Use this section to harmonize characteristic-fraction-speciation synonyms. Click 'Compose Synonym Table' and the table will appear below. The table shows the characteristic-fraction-speciation combinations in your dataset (original columns highlighted blue), as well as any changes that will be made to TADA metadata to allow synonyms to be grouped appropriately (denoted by 'Target' and 'Conversion' columns). Many of these harmonization decisions have been made and documented by the TADA Team in the 'Assumptions' columns. Click the 'CSV' button at the top left corner of the table to download the synonym reference table for your dataset. You may edit manually and re-upload in the file upload widget next to the blue button. When you are ready to harmonize your dataset to the synonym table target elements, click 'Harmonize Data with Synonym Table'. This button only appears when a synonym table has been generated/loaded into this tab."),
    shiny::fluidRow(column(2, htmltools::div(style="margin-top:20px"), shiny::actionButton(ns("harm_go"),"Compose Synonym Table",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    column(4, shiny::fileInput(ns("harm_file"),"Upload Custom Table (.csv only)")),
                    column(3, offset = 3, htmltools::div(style="margin-top:20px"), shiny::uiOutput(ns("harm_apply")))),
    htmltools::br(),
    shiny::fluidRow(column(11,DT::DTOutput(ns("syn_table")))),
    htmltools::br(),
    htmltools::h3("2. Total Nitrogen and Phosphorus Summation"),
    htmltools::p("Data generators commonly analyze for several nutrient subspecies that, when added together, can be used to estimate a total nitrogen or phosphorus value. TADA uses the logic provided in ECHO's ", htmltools::a("Nurient Aggregation", href="https://echo.epa.gov/trends/loading-tool/resources/nutrient-aggregation")," page to rank and sum subspecies for a given day, location, depth, activity media subdivision, and unit. Total Nitrogen and Total Phosphorus values are added as new results in the dataset. Users may view the nutrient aggregation reference sheet by clicking 'See Summation Reference'. Once data are harmonized, the user may then summarize total N and P.", htmltools::strong("NOTE: "), "When two or more measurements of the same substance occur on the same day at the same location, the function uses the maximum of the group of values to calculate a total nutrient value."),
    shiny::fluidRow(column(3, htmltools::div(style="margin-top:20px"), shiny::downloadButton(ns("sum_dwn"),"See Summation Reference",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                    column(3, htmltools::div(style="margin-top:20px"), shiny::uiOutput(ns("sum_apply")))),
    htmltools::br()
 
  )
}
    
#' harmonize_np Server Functions
#'
#' @noRd 
mod_harmonize_np_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    # columns needed for harmonization table
    cols = c("TADA.ActivityMediaName",                   
             "TADA.CharacteristicName",               
             "Target.TADA.CharacteristicName",          
             "TADA.CharacteristicNameAssumptions",      
             "TADA.ResultSampleFractionText",        
             "Target.TADA.ResultSampleFractionText",   
             "TADA.FractionAssumptions",            
             "TADA.MethodSpecificationName",           
             "Target.TADA.MethodSpecificationName",      
             "TADA.SpeciationAssumptions",             
             "Target.TADA.SpeciationConversionFactor",  
             "TADA.ResultMeasure.MeasureUnitCode",      
             "Target.TADA.ResultMeasure.MeasureUnitCode",
             "Target.TADA.UnitConversionFactor",         
             "HarmonizationGroup")
    
    # reactive values for tab
    harm = shiny::reactiveValues()
    
    # when user hits harm go button, runs TADA_GetSynonymRef and makes friendly column names for table.
    shiny::observeEvent(input$harm_go,{
      ref = TADA::TADA_GetSynonymRef(tadat$raw[tadat$raw$TADA.Remove==FALSE,])
      ref = ref %>% dplyr::arrange(Target.TADA.CharacteristicName, Target.TADA.ResultSampleFractionText, Target.TADA.MethodSpecificationName)
      colns = names(ref)
      harm$colns = colns %>% dplyr::recode(TADA.ActivityMediaName = "Media",
                                           TADA.CharacteristicName = "Characteristic",
                                           Target.TADA.CharacteristicName = "Target Characteristic",
                                           TADA.CharacteristicNameAssumptions = "Characteristic Assumptions",
                                           TADA.ResultSampleFractionText = "Fraction",
                                           Target.TADA.ResultSampleFractionText = "Target Fraction",
                                           TADA.FractionAssumptions = "Fraction Assumptions",
                                           TADA.MethodSpecificationName = "Speciation",
                                           Target.TADA.MethodSpecificationName = "Target Speciation",
                                           TADA.SpeciationAssumptions = "Speciation Assumptions",
                                           Target.TADA.SpeciationConversionFactor = "Speciation Conversion Factor (to AS N or AS P)",
                                           TADA.ResultMeasure.MeasureUnitCode = "Unit",
                                           Target.TADA.ResultMeasure.MeasureUnitCode = "Target Unit",
                                           Target.TADA.UnitConversionFactor = "Unit Conversion Factor",
                                           HarmonizationGroup = "Harmonization Group" 
      )
      harm$ref = ref
    })
    
    # This essentially does the same thing with a file upload as the button above.
    shiny::observe({
      shiny::req(input$harm_file)
      # user uploaded data
      ref <- suppressWarnings(read.csv(input$harm_file$datapath))
      if(all(cols%in%names(ref))&dim(ref)[1]>0){
        ref = ref %>% dplyr::arrange(Target.TADA.CharacteristicName, Target.TADA.ResultSampleFractionText, Target.TADA.MethodSpecificationName)
        colns = names(ref)
        harm$colns = colns %>% dplyr::recode(TADA.ActivityMediaName = "Media",
                                             TADA.CharacteristicName = "Characteristic",
                                             Target.TADA.CharacteristicName = "Target Characteristic",
                                             TADA.CharacteristicNameAssumptions = "Characteristic Assumptions",
                                             TADA.ResultSampleFractionText = "Fraction",
                                             Target.TADA.ResultSampleFractionText = "Target Fraction",
                                             TADA.FractionAssumptions = "Fraction Assumptions",
                                             TADA.MethodSpecificationName = "Speciation",
                                             Target.TADA.MethodSpecificationName = "Target Speciation",
                                             TADA.SpeciationAssumptions = "Speciation Assumptions",
                                             Target.TADA.SpeciationConversionFactor = "Speciation Conversion Factor (to AS N or AS P)",
                                             TADA.ResultMeasure.MeasureUnitCode = "Unit",
                                             Target.TADA.ResultMeasure.MeasureUnitCode = "Target Unit",
                                             Target.TADA.UnitConversionFactor = "Unit Conversion Factor",
                                             HarmonizationGroup = "Harmonization Group" 
        )
        harm$ref = ref
      }else{
        shiny::showModal(shiny::modalDialog(
          title = "Unsuitable synonym table",
          "Your synonym reference table is missing essential columns needed for harmonization. Please refer to the default table and work from that."
        ))
      }
    })
    
    # Button to apply synonym table to data
    output$harm_apply = shiny::renderUI({
      shiny::req(harm$ref)
      shiny::actionButton(ns("harm_apply"),"Harmonize Data with Synonym Table",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    })
    
    # Render data table of synonym ref
    output$syn_table = DT::renderDT({
      shiny::req(harm$ref)
      DT::datatable(harm$ref,
                    class = 'cell-border stripe',
                    colnames = harm$colns,
                    filter = "top",
                    extensions = 'Buttons',
                    options = list(dom="Blftipr",scrollX=TRUE, 
                                   pageLength=5,#searching = FALSE,
                                   buttons = c('csv')),
                    selection = 'none', rownames=FALSE) %>% 
        DT::formatStyle(columns = names(harm$ref), `font-size` = "12px") %>%
        DT::formatStyle(columns = c("TADA.ActivityMediaName","TADA.CharacteristicName","TADA.ResultSampleFractionText","TADA.MethodSpecificationName","TADA.ResultMeasure.MeasureUnitCode"), backgroundColor = "#2e6da4", color = "white")
    })
    
    # apply synonym ref to data when button is pushed
    shiny::observeEvent(input$harm_apply,{
      # a modal that pops up showing it's working on harmonizing
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Harmonizing data...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      dat = subset(tadat$raw, tadat$raw$TADA.Remove==FALSE)
      rem = subset(tadat$raw, tadat$raw$TADA.Remove==TRUE)
      dat = TADA::TADA_HarmonizeSynonyms(dat, ref = harm$ref)
      tadat$raw = plyr::rbind.fill(dat, rem)
      tadat$raw = TADA::TADA_OrderCols(tadat$raw)

      # remove the modal once the dataset has been harmonized
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
      num = length(dat$TADA.Harmonized.Flag[dat$TADA.Harmonized.Flag==TRUE])
      shiny::showModal(shiny::modalDialog(
        title = "Success! Harmonization Complete.",
        paste0("Synonym reference table was successfully applied to TADA dataset. ",num," results were harmonized to fit into more informative characteristic-fraction-speciation-unit groups.")
      ))
    })
    
    output$sum_dwn <- downloadHandler(
      filename = function() {
        "TADA_NPSummationKey.csv"
      },
      content = function(file) {
        write.csv(TADA::TADA_GetNutrientSummationRef(), file, row.names = FALSE)
      })
    
    output$sum_apply <- shiny::renderUI({
      if("TADA.Harmonized.Flag"%in%names(tadat$raw)){
        shiny::actionButton(ns("sum_apply"),"Perform Total N and P Summations",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      }
    })
    
    shiny::observeEvent(input$sum_apply,{
      # a modal that pops up showing it's working on harmonizing
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Calculating Total N and P...",
        session = shiny::getDefaultReactiveDomain()
      )
      
      dat = subset(tadat$raw, tadat$raw$TADA.Remove==FALSE)
      rem = subset(tadat$raw, tadat$raw$TADA.Remove==TRUE)
      dat = TADA::TADA_CalculateTotalNP(dat, daily_agg = "max")
      dat$TADA.Remove[is.na(dat$TADA.Remove)] = FALSE
      
      # add new measurements to tadat$removals, all equal FALSE
      ## NOTE THAT THIS ASSUMES NEWLY CREATED RESULTS FROM TOTAL NP WILL NECESSARILY BE ADDED TO END OF TADAT$RAW DATA FRAME
      ncols = ncol(tadat$removals)
      nrows = length(dat$ResultIdentifier[grepl("TADA-",dat$ResultIdentifier)])
      new_df = as.data.frame(matrix(FALSE, ncol = ncols, nrow = nrows))
      names(new_df) = names(tadat$removals)
      tadat$removals = plyr::rbind.fill(tadat$removals, new_df)
      
      tadat$raw = plyr::rbind.fill(dat, rem)
      tadat$raw = TADA::TADA_OrderCols(tadat$raw)
      
      nitrolen = length(dat$TADA.NutrientSummation.Flag[dat$TADA.NutrientSummation.Flag%in%c("Nutrient summation from one or more subspecies.")])
      phoslen = length(dat$TADA.NutrientSummation.Flag[dat$TADA.NutrientSummation.Flag%in%c("Nutrient summation from one subspecies.")])
      # remove the modal once the dataset has been harmonized
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
      shiny::showModal(shiny::modalDialog(
        title = "Success! Calculations Complete.",
        paste0(nitrolen, " Total Nitrogen results calculated and ", phoslen, " Total Phosphorus results calculated.")
      ))
    })
    
 
  })
}
    
## To be copied in the UI
# mod_harmonize_np_ui("harmonize_np_1")
    
## To be copied in the server
# mod_harmonize_np_server("harmonize_np_1")
