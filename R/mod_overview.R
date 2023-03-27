#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(6, leaflet::leafletOutput(ns("overview_map"))),
             column(6, fluidRow(column(11, plotOutput(ns("overview_hist"), height = "200px"))),
                    fluidRow(column(11, dataTableOutput(ns("overview_orgtable"))))))
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, tadat){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mapdat = reactiveValues()
    
    # create dataset for map and histogram using raw data
    observeEvent(tadat$raw, {
      mapdat$sumdat = tadat$raw%>%dplyr::group_by(MonitoringLocationIdentifier,MonitoringLocationName,TADA.LatitudeMeasure, TADA.LongitudeMeasure)%>%dplyr::summarise(Sample_Count = length(unique(ResultIdentifier)), Visit_Count = length(unique(ActivityStartDate)), Parameter_Count = length(unique(TADA.CharacteristicName)), Organization_Count = length(unique(OrganizationIdentifier)))
      mapdat$sumdat$radius = 3
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>10,5,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>50,8,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>100,10,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>200,15,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>500,20,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>1500,30,mapdat$sumdat$radius)
      mapdat$orgs = tadat$raw%>%dplyr::group_by(OrganizationFormalName)%>%dplyr::summarise('Sample Count' = length(unique(ResultIdentifier)))%>%dplyr::arrange(desc("Sample Count"))
      mapdat$chars = tadat$raw%>%dplyr::group_by(TADA.CharacteristicName)%>%dplyr::summarise(Sample_Count = length(unique(ResultIdentifier)))
      })
    
    # the leaflet map
    output$overview_map = leaflet::renderLeaflet({
      req(mapdat$sumdat)
      pal <- leaflet::colorNumeric(
        palette = "Blues",
        domain = mapdat$sumdat$Parameter_Count)
      leaflet::leaflet()%>%
        # leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite", options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
        leaflet::addLayersControl(position ="topright",
                         baseGroups = c("World topo", "Satellite"))%>%
        leaflet::clearShapes()%>%
        leaflet::fitBounds(lng1 = min(mapdat$sumdat$TADA.LongitudeMeasure), lat1 = min(mapdat$sumdat$TADA.LatitudeMeasure), lng2 = max(mapdat$sumdat$TADA.LongitudeMeasure), lat2 = max(mapdat$sumdat$TADA.LatitudeMeasure))%>%
        leaflet::addCircleMarkers(data = mapdat$sumdat, lng=~TADA.LongitudeMeasure, lat=~TADA.LatitudeMeasure, color="black",fillColor=~pal(Parameter_Count), fillOpacity = 0.7, stroke = TRUE, weight = 1.5, radius=mapdat$sumdat$radius,
                         popup = paste0("Site ID: ", mapdat$sumdat$MonitoringLocationIdentifier,
                                        "<br> Site Name: ", mapdat$sumdat$MonitoringLocationName,
                                        "<br> Sample Count: ", mapdat$sumdat$Sample_Count,
                                        "<br> Visit Count: ", mapdat$sumdat$Visit_Count,
                                        "<br> Parameter Count: ", mapdat$sumdat$Parameter_Count))
    })
    
    # histogram
    output$overview_hist = renderPlot({
      req(tadat$raw)
      ggplot2::ggplot(data = tadat$raw, ggplot2::aes(x = ActivityStartDate))+ggplot2::geom_histogram(color = "black", fill = "#005ea2", binwidth = 7)+ggplot2::labs(title=input$overview_select,x="Dates", y = "Sample Count")+ggplot2::theme_classic(base_size = 14)
    })
    
    output$overview_orgtable = renderDataTable(
      mapdat$orgs,
      options = list(scrollY = TRUE,dom = 't')
    )
    
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
