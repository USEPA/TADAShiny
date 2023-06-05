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
    htmltools::HTML("<B>Note:</B> This page shows maps and figures using the <B>original</B> dataset uploaded to this TADAShiny session. If you'd like to see updated figures after working in other tabs, please press the 'Refresh' button."),
    htmltools::div(style="margin-bottom:10px"),
    shiny::fluidRow(column(3, shiny::actionButton(ns("refresh_overview"),"Refresh",shiny::icon("arrows-rotate"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
    htmltools::hr(),
    shiny::fluidRow(column(12, shiny::wellPanel(shiny::htmlOutput(ns("overview_totals"))))),
    htmltools::br(),
    shiny::fluidRow(column(6,shinycssloaders::withSpinner(leaflet::leafletOutput(ns("overview_map"), height = "400px"))),# "Larger point sizes represent more samples collected at a site; darker points represent more characteristics collected at a site. Click on a point to see the site ID, name, and sample/visit/parameter counts.",
             column(6,DT::DTOutput(ns("overview_orgtable")))),#"Hover over a piece of the pie chart to see the characteristic name, count, and its percentage of the dataset. The pie shows the top ten characteristics as their own slices; all other characteristics fit into the 'ALL OTHERS' group.",
    htmltools::br(),
    shiny::fluidRow(column(6,shiny::plotOutput(ns("overview_hist"), height="500px")),#"This histogram shows sample collection frequency for all sites over the time period queried.",
             column(6, shiny::plotOutput(ns("overview_barchar"), height="615px")))
  )
}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    # this widget produces the text at the top of the page describing record, site, and org numbers in dataset
    output$overview_totals = shiny::renderText({
      shiny::req(tadat$raw)
      paste0("Your dataset contains <B>",scales::comma(length(unique(tadat$raw$ResultIdentifier))),"</B> unique results from <B>",scales::comma(length(unique(tadat$raw$MonitoringLocationIdentifier))),"</B> monitoring location(s) and <B>", scales::comma(length(unique(tadat$raw$OrganizationFormalName))),"</B> unique organization(s).")
    })
    # this a reactive list created to hold all the reactive objects specific to this module.
    mapdat = shiny::reactiveValues()

    # create dataset for map and histogram using raw data
    shiny::observeEvent(tadat$ovgo, {
      usedata = tadat$raw%>%dplyr::filter(TADA.Remove==FALSE) # do not consider data automatically removed upon upload for plots and maps
      # create summary info and binning for map
      mapdat$sumdat = usedata%>%dplyr::group_by(MonitoringLocationIdentifier,MonitoringLocationName,TADA.LatitudeMeasure, TADA.LongitudeMeasure)%>%dplyr::summarise("Result_Count" = length(unique(ResultIdentifier)), "Visit_Count" = length(unique(ActivityStartDate)), "Parameter_Count" = length(unique(TADA.CharacteristicName)), "Organization_Count" = length(unique(OrganizationIdentifier)))
      mapdat$sumdat$radius = 3
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Result_Count>10,5,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Result_Count>50,8,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Result_Count>100,10,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Result_Count>200,15,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Result_Count>500,20,mapdat$sumdat$radius)
      mapdat$sumdat$radius = ifelse(mapdat$sumdat$Result_Count>1500,30,mapdat$sumdat$radius)
      # create org summary for table.
      mapdat$orgs = usedata%>%dplyr::group_by(OrganizationFormalName)%>%dplyr::summarise("Result_Count" = length(unique(ResultIdentifier)))
      # get top 10 characteristics by result number in the dataset and place the rest in a group called "all others"
      chars = usedata%>%dplyr::group_by(TADA.CharacteristicName)%>%dplyr::summarise("Result_Count" = length(unique(ResultIdentifier)))
      topslice = chars%>%dplyr::slice_max(order_by = Result_Count, n = 10)
      bottomslice = chars%>%dplyr::ungroup()%>%dplyr::filter(!TADA.CharacteristicName%in%topslice$TADA.CharacteristicName)%>%dplyr::select("Result_Count")%>%dplyr::summarise("Result_Count" = sum(Result_Count))%>%dplyr::mutate("TADA.CharacteristicName" = "ALL OTHERS")
      chars = plyr::rbind.fill(topslice, bottomslice)%>%dplyr::filter(Result_Count>0)
      chars = chars%>%dplyr::mutate(TADA.Chars = substr(TADA.CharacteristicName, 1,22))
      chars$TADA.Chars = ifelse(nchar(chars$TADA.CharacteristicName)>22,paste0(chars$TADA.Chars, "..."),chars$TADA.Chars)
      chars = chars%>%dplyr::mutate(TADA.Chars = forcats::fct_reorder(TADA.Chars, Result_Count, .desc=TRUE))
      mapdat$chars = chars
      tadat$ovgo = NULL
      })

    # the leaflet map
    output$overview_map = leaflet::renderLeaflet({
      shiny::req(mapdat$sumdat)
      # blue color palette
      pal <- leaflet::colorBin(
        palette = "Blues",
        domain = mapdat$sumdat$Parameter_Count)
      leaflet::leaflet()%>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = leaflet::providerTileOptions(updateWhenZooming = FALSE,updateWhenIdle = TRUE))%>%
        leaflet::clearShapes()%>% # get rid of whatever was there before if loading a second dataset
        leaflet::fitBounds(lng1 = min(mapdat$sumdat$TADA.LongitudeMeasure), lat1 = min(mapdat$sumdat$TADA.LatitudeMeasure), lng2 = max(mapdat$sumdat$TADA.LongitudeMeasure), lat2 = max(mapdat$sumdat$TADA.LatitudeMeasure))%>% # fit to bounds of data in tadat$raw
        leaflet::addCircleMarkers(data = mapdat$sumdat, lng=~TADA.LongitudeMeasure, lat=~TADA.LatitudeMeasure, color="black",fillColor=~pal(Parameter_Count), fillOpacity = 0.7, stroke = TRUE, weight = 1.5, radius=mapdat$sumdat$radius,
                         popup = paste0("Site ID: ", mapdat$sumdat$MonitoringLocationIdentifier,
                                        "<br> Site Name: ", mapdat$sumdat$MonitoringLocationName,
                                        "<br> Result Count: ", mapdat$sumdat$Result_Count,
                                        "<br> Visit Count: ", mapdat$sumdat$Visit_Count,
                                        "<br> Characteristic Count: ", mapdat$sumdat$Parameter_Count))%>%
        leaflet::addLegend("bottomright", pal = pal, values =mapdat$sumdat$Parameter_Count,
                  title = "Characteristics",
                  opacity = 0.5
        )
    })

    # histogram showing results collected over time.
    output$overview_hist = shiny::renderPlot({
      shiny::req(tadat$raw)
      ggplot2::ggplot(data = tadat$raw[tadat$raw$TADA.Remove==FALSE,], ggplot2::aes(x = as.Date(ActivityStartDate, format = "%Y-%m-%d")))+ggplot2::geom_histogram(color = "black", fill = "#005ea2", binwidth = 7)+ggplot2::labs(title="Results collected per week over date range queried",x="Time", y = "Result Count")+ggplot2::theme_classic(base_size = 16)
    })
    
    # organization numbers table
    output$overview_orgtable = DT::renderDT({
      DT::datatable(data.frame(mapdat$orgs),
      options = list(pageLength=10, colnames = c("Organization Name","Results Count"), searching = FALSE),
      rownames= FALSE,
      selection = 'none')
    })
    
    # characteristics bar chart showing top characteristics by result number in dataset
    output$overview_barchar = shiny::renderPlot({
      shiny::req(mapdat$chars)
      ggplot2::ggplot(mapdat$chars, ggplot2::aes(x=TADA.Chars, y=Result_Count)) +
        ggplot2::geom_bar(stat = "identity", fill = "#005ea2", color = "black") +
        ggplot2::labs(title="Number of Results per Characteristic",x="", y = "Results Count")+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::geom_text(ggplot2::aes(x = TADA.Chars, y = Result_Count+(0.07*max(Result_Count)), label = Result_Count), size = 5, color="black") #+
   
          })
    
    shiny::observeEvent(input$refresh_overview,{
      shiny::req(tadat$raw)
      tadat$ovgo = 2
    })

  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
