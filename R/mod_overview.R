#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' 

mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::h3("Data Overview"),
    shiny::fluidRow(column(7,leaflet::leafletOutput(ns("overview_map"))),# "Larger point sizes represent more samples collected at a site; darker points represent more characteristics collected at a site. Click on a point to see the site ID, name, and sample/visit/parameter counts.",
             column(5,plotly::plotlyOutput(ns("overview_piechar")))),#"Hover over a piece of the pie chart to see the characteristic name, count, and its percentage of the dataset. The pie shows the top ten characteristics as their own slices; all other characteristics fit into the 'ALL OTHERS' group.",
    htmltools::br(),
    shiny::fluidRow(column(7,shiny::plotOutput(ns("overview_hist"), height="400px")),#"This histogram shows sample collection frequency for all sites over the time period queried.",
             column(5, DT::DTOutput(ns("overview_orgtable"), height="400px")))
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mapdat = shiny::reactiveValues()
    
    # create dataset for map and histogram using raw data
    shiny::observeEvent(tadat$raw, {
      mapdat$sumdat = tadat$raw%>%dplyr::group_by(MonitoringLocationIdentifier,MonitoringLocationName,TADA.LatitudeMeasure, TADA.LongitudeMeasure)%>%dplyr::summarise("Sample_Count" = length(unique(ResultIdentifier)), "Visit_Count" = length(unique(ActivityStartDate)), "Parameter_Count" = length(unique(TADA.CharacteristicName)), "Organization_Count" = length(unique(OrganizationIdentifier)))
      mapdat$sumdat$radius = 3
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>10,5,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>50,8,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>100,10,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>200,15,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>500,20,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Sample_Count>1500,30,mapdat$sumdat$radius)
      mapdat$orgs = tadat$raw%>%dplyr::group_by(OrganizationFormalName)%>%dplyr::summarise("Sample Count" = length(unique(ResultIdentifier)))
      mapdat$orgs = mapdat$orgs[order(mapdat$orgs$OrganizationFormalName, decreasing=TRUE),]
      chars = tadat$raw%>%dplyr::group_by(TADA.CharacteristicName)%>%dplyr::summarise("Sample_Count" = length(unique(ResultIdentifier)))
      topslice = chars%>%dplyr::slice_max(order_by = Sample_Count, n = 10)
      bottomslice = chars%>%dplyr::ungroup()%>%dplyr::filter(!TADA.CharacteristicName%in%topslice$TADA.CharacteristicName)%>%dplyr::select("Sample_Count")%>%dplyr::summarise("Sample_Count" = sum(Sample_Count))%>%dplyr::mutate("TADA.CharacteristicName" = "ALL OTHERS")
      mapdat$chars = plyr::rbind.fill(topslice, bottomslice)
      })
    
    # the leaflet map
    output$overview_map = leaflet::renderLeaflet({
      shiny::req(mapdat$sumdat)
      pal <- leaflet::colorNumeric(
        palette = "Blues",
        domain = mapdat$sumdat$Parameter_Count)
      leaflet::leaflet()%>%
        # leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite", options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE)) %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
        # leaflet::addLayersControl(position ="topright",
        #                  baseGroups = c("World topo", "Satellite"))%>%
        leaflet::clearShapes()%>%
        leaflet::fitBounds(lng1 = min(mapdat$sumdat$TADA.LongitudeMeasure), lat1 = min(mapdat$sumdat$TADA.LatitudeMeasure), lng2 = max(mapdat$sumdat$TADA.LongitudeMeasure), lat2 = max(mapdat$sumdat$TADA.LatitudeMeasure))%>%
        leaflet::addCircleMarkers(data = mapdat$sumdat, lng=~TADA.LongitudeMeasure, lat=~TADA.LatitudeMeasure, color="black",fillColor=~pal(Parameter_Count), fillOpacity = 0.7, stroke = TRUE, weight = 1.5, radius=mapdat$sumdat$radius,
                         popup = paste0("Site ID: ", mapdat$sumdat$MonitoringLocationIdentifier,
                                        "<br> Site Name: ", mapdat$sumdat$MonitoringLocationName,
                                        "<br> Sample Count: ", mapdat$sumdat$Sample_Count,
                                        "<br> Visit Count: ", mapdat$sumdat$Visit_Count,
                                        "<br> Parameter Count: ", mapdat$sumdat$Parameter_Count))%>%
        leaflet::addLegend("bottomright", pal = pal, values =mapdat$sumdat$Parameter_Count,
                  title = "Characteristics",
                  opacity = 0.5
        )
    })
    
    # histogram
    output$overview_hist = shiny::renderPlot({
      shiny::req(tadat$raw)
      ggplot2::ggplot(data = tadat$raw, ggplot2::aes(x = ActivityStartDate))+ggplot2::geom_histogram(color = "black", fill = "#005ea2", binwidth = 7)+ggplot2::labs(title="Samples collected over date range queried",x="Time", y = "Sample Count")+ggplot2::theme_classic(base_size = 14)
    })
    
    output$overview_orgtable = DT::renderDT(
      mapdat$orgs,
      options = list(dom="t", scrollY=TRUE, pageLength=5),
      rownames= FALSE,
      selection = 'none'
    )
    
    output$overview_piechar = plotly::renderPlotly({
      shiny::req(mapdat$chars)
      fig = plotly::plot_ly(data = mapdat$chars, labels =~TADA.CharacteristicName, values =~Sample_Count, textinfo = "text", text =~Sample_Count, marker = list(colorscale="Viridis"))%>%plotly::add_pie(hole = 0.3)%>%
        plotly::layout(title = "Characteristics in Dataset", showlegend = FALSE, font = list(family = "Arial", size = 12))
    })
 
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_1")
    
## To be copied in the server
# mod_overview_server("overview_1")
