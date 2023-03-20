#' query_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

load("inst/extdata/statecodes_df.Rdata")
orgs = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Organization.CSV"))$ID)
chars = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))$Name)
media = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ActivityMedia.CSV"))$Name)
county = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/County.CSV"))$County.Name)
# sitetype = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV"))$Name)
sitetype = c("Aggregate groundwater use","Aggregate surface-water-use","Aggregate water-use establishment","Atmosphere","Estuary","Facility","Glacier","Lake, Reservoir, Impoundment","Land","Not Assigned","Ocean","Spring","Stream","Subsurface","Well","Wetland")
projects = unique(data.table::fread("https://www.waterqualitydata.us/data/Project/search?mimeType=csv&zip=no&providers=NWIS&providers=STEWARDS&providers=STORET")$ProjectIdentifier)


mod_query_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(4,selectizeInput(ns("state"),"State", choices = NULL)),
             column(4,selectizeInput(ns("county"), "County", choices = NULL)),
             column(4, selectizeInput(ns("org"),"Organization(s)", choices = NULL, multiple = TRUE))),
    fluidRow(column(4, selectizeInput(ns("proj"),"Project(s)", choices = NULL, multiple = TRUE)),
             column(4, selectizeInput(ns("characteristic"),"Characteristic(s)", choices = NULL, multiple = TRUE)),
             column(4, selectizeInput(ns("media"), "Sample Media", choices = c("",media), selected = "Water", multiple = TRUE))),
    fluidRow(column(4, selectizeInput(ns("type"), "Site Type(s)", choices = c("",sitetype), multiple = TRUE)),
             column(8, textInput(ns("siteids"), "Monitoring Location ID(s), separated by commas", value = ""))),
    fluidRow(column(3, dateInput(ns("startdate"),"Start Date", format = "yyyy-mm-dd", startview = "year")),
             column(3, dateInput(ns("enddate"),"End Date", format = "yyyy-mm-dd", startview = "year"))),
    # textInput(ns("hucs"), "Type in HUC(s), separated by commas", value = ""),
    fluidRow(column(3, actionButton(ns("querynow"),"Run Query"))),
    fluidRow(column(3, textOutput(ns("success"))))
  )
}

#' query_data Server Functions
#'
#' @noRd
mod_query_data_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    updateSelectizeInput(session,"state",choices = c("",unique(statecodes_df$STUSAB)),  server = TRUE)
    updateSelectizeInput(session,"county",choices = c("",county), server = TRUE)
    updateSelectizeInput(session,"org",choices = c("",orgs), server = TRUE)
    updateSelectizeInput(session,"characteristic",choices = c("",chars), server = TRUE)
    updateSelectizeInput(session,"proj", choices = c("",projects), server = TRUE)

    observeEvent(input$querynow,{
      # convert to null when needed
      if(input$state==""){
        statecode = "null"
      }else{statecode = input$state}
      if(input$county==""){
        countycode = "null"
      }else{countycode = input$county}
      if(is.null(input$type)){
        siteType = "null"
      }else{siteType = input$type}
      if(is.null(input$characteristic)){
        characteristicName = "null"
      }else{characteristicName = input$characteristic}
      if(is.null(input$media)){
        sampleMedia = "null"
      }else{sampleMedia = input$media}
      if(is.null(input$project)){
        project = "null"
      }else{project = input$project}
      if(is.null(input$org)){
        organization = "null"
      }else{organization = input$org}
      if(input$siteids == ""){
        siteid = "null"
      }else{
        siteid = stringr::str_trim(unlist(strsplit(input$siteids,",")))}

      tadat$raw = TADA::TADAdataRetrieval(statecode = statecode,
                                        startDate = as.character(input$startdate),
                                        countycode = countycode,
                                        siteid = siteid,
                                        siteType = siteType,
                                        characteristicName = characteristicName,
                                        sampleMedia = sampleMedia,
                                        project = project,
                                        organization = organization,
                                        endDate = as.character(input$enddate),
                                        applyautoclean = TRUE
      )

      output$success <- renderText({
        if(is.null(tadat$raw)){
          "Working on it..."
        }else{
          "I did it!"
        }
      })
    })

  })
}

## To be copied in the UI
# mod_query_data_ui("query_data_1")

## To be copied in the server
# mod_query_data_server("query_data_1")
