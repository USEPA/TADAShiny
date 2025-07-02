#' map_bbox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_bboxUI <- function(id, label = "Clear Drawing") {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map_bbox")),
    actionButton(inputId = ns("clear_map"), label = label)
  )
}

mod_map_bboxServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactive values FIRST
    bbox_reVal <- reactiveValues(bbox = NULL)
    
    # Render base map
    output$map_bbox <- leaflet::renderLeaflet({
      # Create the map
      m <- leaflet::leaflet() |>
        leaflet.extras::addDrawToolbar(
          targetGroup = "drawn_items",
          polylineOptions = FALSE, 
          circleOptions = FALSE, 
          markerOptions = FALSE,
          circleMarkerOptions = FALSE, 
          polygonOptions = FALSE,
          rectangleOptions = leaflet.extras::drawRectangleOptions(),
          singleFeature = TRUE) |>
        add_USGS_base() |>
        leaflet::setView(lng = -114, lat = 42, zoom = 3)
      
      return(m)
    })
    
    # Clear the map
    shiny::observeEvent(input$clear_map, {
      proxy <- leaflet::leafletProxy("map_bbox", session = session)
      # remove both the toolbar and any drawn features
      proxy %>% leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
      # immediately re-add the toolbar so the user can draw again
      proxy %>% leaflet.extras::addDrawToolbar(
        targetGroup    = "drawn_items",
        polylineOptions       = FALSE,
        polygonOptions        = FALSE,
        circleOptions         = FALSE,
        markerOptions         = FALSE,
        circleMarkerOptions   = FALSE,
        rectangleOptions      = leaflet.extras::drawRectangleOptions(),
        singleFeature         = TRUE
      )
      bbox_reVal$bBox <- NULL
    })
    
    # Handle new drawings
    observeEvent(input$map_bbox_draw_new_feature, {
      
      feat <- input$map_bbox_draw_new_feature
      coords <- unlist(feat$geometry$coordinates)
      coords_m <- matrix(coords, ncol = 2, byrow = TRUE)
      
      # Create proper bbox
      poly <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords_m))), crs = sf::st_crs(4326))
      bbox_temp <- unname(sf::st_bbox(poly))
      
      # Store as bbox object
      bbox_reVal$bBox <- bbox_temp
      
    })
    
    # Handle deleted drawings
    shiny::observeEvent(input$map_bbox_draw_deleted_features, {
      bbox_reVal$bBox <- NULL
    })
    
    return(bbox_reVal)
  })
}
