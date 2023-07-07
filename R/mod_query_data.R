#' query_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinybusy
#'

load("inst/extdata/statecodes_df.Rdata")
load("inst/extdata/query_choices.Rdata")
# county = readr::read_tsv(url("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"), col_names = FALSE)
# county = county%>%tidyr::separate(X1,into = c("STUSAB","STATE","COUNTY","COUNTY_NAME","COUNTY_ID"), sep=",")
# orgs = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Organization.CSV"))$ID)
# chars = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))$Name)
# chargroup = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicGroup.CSV"))$Name)
# media = c(unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ActivityMedia.CSV"))$Name),"water","Biological Tissue","No media")
# # sitetype = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV"))$Name)
# sitetype = c("Aggregate groundwater use","Aggregate surface-water-use","Aggregate water-use establishment","Atmosphere","Estuary","Facility","Glacier","Lake, Reservoir, Impoundment","Land","Not Assigned","Ocean","Spring","Stream","Subsurface","Well","Wetland")
# projects = unique(data.table::fread("https://www.waterqualitydata.us/data/Project/search?mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET")$ProjectIdentifier)
# mlids = unique(data.table::fread("https://www.waterqualitydata.us/data/Station/search?mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET")$MonitoringLocationIdentifier)
# save(orgs, chars, chargroup, media, county, sitetype, projects, mlids, file = "query_choices.Rdata")

mod_query_data_ui <- function(id){
  ns <- NS(id)
  tagList(shiny::fluidRow(htmltools::h3("Option A: Use example data"),
                          column(3,shiny::selectInput(ns("example_data"),"Use example data",choices = c("","Nutrients Utah (15k results)","Shepherdstown (34k results)","Tribal (132k results)")))),
          shiny::fluidRow(column(3, shiny::actionButton(ns("example_data_go"), "Load",shiny::icon("truck-ramp-box"),
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
          htmltools::hr(),
          shiny::fluidRow(htmltools::h3("Option B: Query the Water Quality Portal (WQP)"),
                   "Use the fields below to download a dataset directly from WQP. Fields with '(s)' in the label allow multiple selections. Hydrologic Units may be at any scale, from subwatershed to region. However, be mindful that large queries may time out."),
          htmltools::br(), # styling several fluid rows with columns to hold the input drop down widgets
          htmltools::h4("Date Range"),
          shiny::fluidRow(column(4, shiny::dateInput(ns("startdate"),"Start Date", format = "yyyy-mm-dd", startview = "year")),
                          column(4, shiny::dateInput(ns("enddate"),"End Date", format = "yyyy-mm-dd", startview = "year"))),
          htmltools::h4("Location Information"),
          shiny::fluidRow(column(4,shiny::selectizeInput(ns("state"),"State", choices = NULL)),
                          column(4,shiny::selectizeInput(ns("county"), "County (pick state first)", choices = NULL)),
                          column(4,shiny::textInput(ns("huc"),"Hydrologic Unit", placeholder = "e.g. 020700100103"))),
          shiny::fluidRow(column(4, shiny::selectizeInput(ns("siteid"), "Monitoring Location ID(s)", choices = NULL,multiple = TRUE))),
          htmltools::h4("Metadata Filters"),
          shiny::fluidRow(column(4, shiny::selectizeInput(ns("org"),"Organization(s)", choices = NULL, multiple = TRUE)),
                          column(4, shiny::selectizeInput(ns("proj"),"Project(s)", choices = NULL, multiple = TRUE)),
                          column(4, shiny::selectizeInput(ns("type"), "Site Type(s)", choices = c("",sitetype), multiple = TRUE))),
          shiny::fluidRow(column(4, shiny::selectizeInput(ns("media"), tags$span("Sample Media", 
            tags$i(
              class = "glyphicon glyphicon-info-sign", 
              style = "color:#0072B2;",
              title = "At present, TADA is only designed to work with water sample media"
            )
          ), choices = c("",media), selected = c("Water", "water"), multiple = TRUE)),
                          column(4, shiny::selectizeInput(ns("chargroup"),"Characteristic Group", choices = NULL)),
                          column(4, shiny::selectizeInput(ns("characteristic"),"Characteristic(s)", choices = NULL, multiple = TRUE))),
          shiny::fluidRow(column(4, shiny::actionButton(ns("querynow"),"Run Query",shiny::icon("cloud"),
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
          htmltools::hr(),
          shiny::fluidRow(htmltools::h3("Option C: Upload dataset"),
                   htmltools::HTML(("Select a file from your computer. This upload feature currently only accepts data in .xls and .xlsx formats.
                                    The file can be a <B>fresh</B> TADA dataset or a <B>working</B> TADA dataset that you are returning to the
                                    app to iterate on. Data must also be formatted in the EPA Water Quality eXchange (WQX) schema to leverage
                                    this tool. You may reach out to the WQX helpdesk at WQX@epa.gov for assistance preparing and submitting your data 
                                    to the WQP through EPA's WQX.")),
                   # widget to upload WQP profile or WQX formatted spreadsheet
                   column(9,shiny::fileInput(ns("file"), "",
                             multiple = TRUE,
                             accept = c(".xlsx", ".xls"),
                             width = "100%")))
      )
}

#' query_data Server Functions
#'
#' @noRd
mod_query_data_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # read in the excel spreadsheet dataset if this input reactive object is populated via fileInput and define as tadat$raw
    shiny::observe({
      shiny::req(input$file)
      # user uploaded data
      raw <- suppressWarnings(readxl::read_excel(input$file$datapath, sheet = 1))
      initializeTable(tadat, raw)
    })
    
  # if user presses example data button, make tadat$raw the nutrients dataset contained within the TADA package.
    shiny::observeEvent(input$example_data_go,{
      if(input$example_data == "Shepherdstown (34k results)"){
        raw = TADA::Data_NCTCShepherdstown_HUC12
      }
      if(input$example_data == "Tribal (132k results)"){
        raw = TADA::Data_6Tribes_5y
      }
      if(input$example_data == "Nutrients Utah (15k results)"){
        raw = TADA::Data_Nutrients_UT
      }
      initializeTable(tadat, raw)
    })

    # this section has widget update commands for the selectizeinputs that have a lot of possible selections - shiny suggested hosting the choices server-side rather than ui-side
    shiny::updateSelectizeInput(session,"state",choices = c("",unique(statecodes_df$STUSAB)),  server = TRUE)
    shiny::updateSelectizeInput(session,"org",choices = c("",orgs), server = TRUE)
    shiny::updateSelectizeInput(session,"chargroup",choices = c("",chargroup), server = TRUE)
    shiny::updateSelectizeInput(session,"characteristic",choices = c("",chars), server = TRUE)
    shiny::updateSelectizeInput(session,"proj", choices = c("",projects), server = TRUE)
    shiny::updateSelectizeInput(session,"siteid", choices = c("",mlids), server = TRUE)

    # this observes when the user inputs a state into the drop down and subsets the choices for counties to only those counties within that state.
    shiny::observeEvent(input$state,{
      state_counties = subset(county, county$STUSAB==input$state)
      shiny::updateSelectizeInput(session,"county",choices = c("",unique(state_counties$COUNTY_NAME)), server = TRUE)
    })

    # this event observer is triggered when the user hits the "Query Now" button, and then runs the TADAdataRetrieval function
    shiny::observeEvent(input$querynow,{
      # convert to null when needed
      if(input$state==""){ # changing inputs of "" or NULL to "null"
        statecode = "null"
      }else{statecode = input$state}
      if(input$county==""){
        countycode = "null"
      }else{countycode = input$county}
      if(input$huc==""){
        huc = "null"
      }else{huc = input$huc}
      if(is.null(input$type)){
        siteType = "null"
      }else{siteType = input$type}
      if(input$chargroup==""){
        characteristicType = "null"
      }else{characteristicType = input$chargroup}
      if(is.null(input$characteristic)){
        characteristicName = "null"
      }else{characteristicName = input$characteristic}
      if(is.null(input$media)){
        sampleMedia = "null"
      }else{sampleMedia = input$media}
      if(is.null(input$proj)){
        project = "null"
      }else{project = input$proj}
      if(is.null(input$org)){
        organization = "null"
      }else{organization = input$org}
      if(is.null(input$siteid)){
        siteid = "null"
      }else{
        siteid = input$siteid
        # siteid = stringr::str_trim(unlist(strsplit(input$siteids,",")))
      }
      if(length(input$enddate)==0){ # ensure if date is empty, the query receives a proper input ("null")
        enddate = "null"
      }else{
        enddate = as.character(input$enddate)
      }
      if(length(input$startdate)==0){ # ensure if date is empty, the query receives a proper start date. Might want a warning message instead.
        startdate = "1800-01-01"
      }else{
        startdate = as.character(input$startdate)
      }
      # a modal that pops up showing it's working on querying the portal
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Querying WQP database...",
        session = shiny::getDefaultReactiveDomain()
      )
      # storing the output of TADAdataRetrieval with the user's input choices as a reactive object named "raw" in the tadat list.
      raw = TADA::TADA_DataRetrieval(statecode = statecode,
                                        countycode = countycode,
                                        huc = huc,
                                        siteid = siteid,
                                        siteType = siteType,
                                        characteristicName = characteristicName,
                                        characteristicType = characteristicType,
                                        sampleMedia = sampleMedia,
                                        project = project,
                                        organization = organization,
                                        startDate = startdate,
                                        endDate = enddate,
                                        applyautoclean = TRUE
      )
      # remove the modal once the dataset has been pulled
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())

      # show a modal dialog box when tadat$raw is empty and the query didn't return any records.
      # but if tadat$raw isn't empty, perform some initial QC of data that aren't media type water or have NA Resultvalue and no detection limit data
      if(dim(raw)[1]<1){
        shiny::showModal(shiny::modalDialog(
          title = "Empty Query",
          "Your query returned zero results. Please adjust your search inputs and try again."
        ))
      }else{
          initializeTable(tadat, raw)
          }
    })

  })
}

initializeTable <- function(tadat, raw){
  # Test to see if this is a raw table or one previously worked on in TADA
  if("TADA.Remove"%in%names(raw)){
    tadat$reup = TRUE
    tadat$ovgo = FALSE
    shinyjs::enable(selector = '.nav li a[data-value="Overview"]')
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
    shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
    shinyjs::enable(selector = '.nav li a[data-value="Censored"]')
    shinyjs::enable(selector = '.nav li a[data-value="Review"]')
  } else {
    tadat$new = TRUE # this is used to determine if the app should go to the overview page first - only for datasets that are new to TADAShiny
    tadat$ovgo = TRUE # load data into overview page
    shinyjs::enable(selector = '.nav li a[data-value="Overview"]')
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
    # Set flagging column to FALSE
    raw$TADA.Remove = FALSE
  }
  
  removals <- data.frame(matrix(nrow = nrow(raw), ncol = 0))
  # removals["Media Type"] = ifelse(!raw$TADA.ActivityMediaName%in%c("WATER"),TRUE,raw$Removed)
  # removals["Special Characters"] = ifelse(raw$TADA.ResultMeasureValueDataTypes.Flag%in%c("ND or NA","Text","Coerced to NA"),TRUE,raw$Removed)
  
  tadat$raw = raw
  tadat$removals = removals
  }


## To be copied in the UI
# mod_query_data_ui("query_data_1")

## To be copied in the server
# mod_query_data_server("query_data_1")
