#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom scales rescale
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    "The map, tables, and plots below are built using the uploaded/queried data. Use the drop down menu below to select one parameter of interest or use choice 'All' to see a summary for all parameters together.",
     fluidRow(column(6, uiOutput(ns("overview_select"))),
              column(6, uiOutput(ns("overview_dates")))), # note that this uiOutput is a widget that needs something from the server-side before rendering. It is a drop down menu.
     fluidRow(column(6, leaflet::leafletOutput(ns("overview_map"))),
              column(6, plotOutput(ns("overview_hist"))))
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mapdat = reactiveValues()
    
    o_char = reactive({
      req(tadat$raw)
      if(dim(tadat$raw)[1]>0){
        unique(tadat$raw$TADA.CharacteristicName)
      }
    })

    # o_char is used to parameterize this drop down select input that allows the user to map/plot one parameter at a time or all of them.
    output$overview_select = renderUI({
      req(o_char()) # this is the drop down menu widget that you might be familiar seeing in the ui, but because it needs something from tadat$raw to create tadat$o_char before rendering, it goes in the server-side.
      selectInput(ns("overview_select"),"Select Characteristic", choices = c("All", o_char()))
    })
    
    observeEvent(input$overview_select,{
      if(!input$overview_select=="All"){
        alldat = subset(tadat$raw, tadat$raw$TADA.CharacteristicName==input$overview_select)
      }else{
        alldat = tadat$raw
      }
      alldat$ActivityStartDate = as.Date(alldat$ActivityStartDate, "%Y-%m-%d")
      mapdat$alldat = alldat
    })
    
    output$overview_dates = renderUI({
      req(mapdat$alldat) 
      start = min(mapdat$alldat$ActivityStartDate)
      end = max(mapdat$alldat$ActivityStartDate)
      sliderInput(ns("overview_dates"),"Select Date Range", min = start, max = end, value = c(start,end), timeFormat = "%Y-%m-%d")
    })
    
    # create dataset for map and histogram using characteristic and dates selected
    observe({
      req(input$overview_dates)
      mapdat$alldat2 = subset(mapdat$alldat, mapdat$alldat$ActivityStartDate>=input$overview_dates[1]&mapdat$alldat$ActivityStartDate<=input$overview_dates[2])
      mapdat$sumdat = mapdat$alldat2%>%dplyr::group_by(MonitoringLocationIdentifier,MonitoringLocationName,TADA.LatitudeMeasure, TADA.LongitudeMeasure)%>%dplyr::summarise(Sample_Count = length(unique(ResultIdentifier)), Visit_Count = length(unique(ActivityStartDate)), Parameter_Count = length(unique(TADA.CharacteristicName)), Organization_Count = length(unique(OrganizationIdentifier)))
    })
    
    # the leaflet map
    output$overview_map = leaflet::renderLeaflet({
      req(input$overview_dates)
      pal <- colorNumeric(
        palette = "Blues",
        domain = mapdat$sumdat$Parameter_Count)
      leaflet()%>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
        addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
        addLayersControl(position ="topright",
                         baseGroups = c("World topo", "Satellite"))%>%
        clearShapes()%>%
        fitBounds(lng1 = min(mapdat$sumdat$TADA.LongitudeMeasure), lat1 = min(mapdat$sumdat$TADA.LatitudeMeasure), lng2 = max(mapdat$sumdat$TADA.LongitudeMeasure), lat2 = max(mapdat$sumdat$TADA.LatitudeMeasure))%>%
        addCircleMarkers(data = mapdat$sumdat, lng=~TADA.LongitudeMeasure, lat=~TADA.LatitudeMeasure, color="black",fillColor=~pal(Parameter_Count), fillOpacity = 0.7, stroke = TRUE, weight = 1.5, radius=~scales::rescale(mapdat$sumdat$Sample_Count, c(2,15)),
                         popup = paste0("Site ID: ", mapdat$sumdat$MonitoringLocationIdentifier,
                                        "<br> Site Name: ", mapdat$sumdat$MonitoringLocationName,
                                        "<br> Sample Count: ", mapdat$sumdat$Sample_Count,
                                        "<br> Visit Count: ", mapdat$sumdat$Visit_Count,
                                        "<br> Parameter Count: ", mapdat$sumdat$Parameter_Count))
    })
    
    # histogram
    output$overview_hist = renderPlot({
      req(input$overview_dates)
      ggplot2::ggplot(data = mapdat$alldat2, aes(x = ActivityStartDate))+geom_histogram(color = "black", fill = "#005ea2")+labs(title=input$overview_select,x="Dates", y = "Sample Count")+theme_classic(base_size = 14)
    })
    
    
    # om%>%
    #   clearShapes()%>%
    #   fitBounds(lng1 = min(mapdat$sumdat$TADA.LongitudeMeasure), lat1 = min(mapdat$sumdat$TADA.LatitudeMeasure), lng2 = max(mapdat$sumdat$TADA.LongitudeMeasure), lat2 = max(mapdat$sumdat$TADA.LatitudeMeasure))%>%
    #   addCircleMarkers(data = mapdat$sumdat, lng=~TADA.LongitudeMeasure, lat=~TADA.LatitudeMeasure, color="black",fillColor=~pal(Parameter_Count), fillOpacity = 0.7, stroke = TRUE, weight = 1.5, radius=~scales::rescale(mapdat$sumdat$Sample_Count, c(2,15)),
    #                    popup = paste0("Site ID: ", mapdat$sumdat$MonitoringLocationIdentifier,
    #                                   "<br> Site Name: ", mapdat$sumdat$MonitoringLocationName,
    #                                   "<br> Sample Count: ", mapdat$sumdat$Sample_Count,
    #                                   "<br> Visit Count: ", mapdat$sumdat$Visit_Count,
    #                                   "<br> Parameter Count: ", mapdat$sumdat$Parameter_Count)) #%>%
    # addLegend("bottomright", pal = pal, values = ~mapdat$mdat$Parameter_Count,
    #           title = "Number of characteristics sampled",
    #           opacity = 1
    # )
 
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
