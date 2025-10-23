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
    fluidRow(
      column(width = 6,
             leaflet::leafletOutput(ns("map_bbox"))
      ),
      # Bounding box inputs on the right (takes 4 columns)
      column(
        width = 6,
        h4("Bounding Box Latitude and Longitude"),
        
        fluidRow(
          column(
            width = 3,
            br(),
            br(),
            # West coordinate
            numericInput(
              inputId = ns("bb_W"),
              label = "West:",
              value = NULL,
              min = -180,
              max = 180,
              step = 0.00001
            )
          ),
          column(
            width = 3,
            # North coordinate
            numericInput(
              inputId = ns("bb_N"),
              label = "North:",
              value = NULL,
              min = -90,
              max = 90,
              step = 0.00001
            ),
            br(),
            br(),
            # South coordinate
            numericInput(
              inputId = ns("bb_S"),
              label = "South:",
              value = NULL,
              min = -90,
              max = 90,
              step = 0.00001
            )
          ),
          column(
            width = 3,
            br(),
            br(),
            # East coordinate
            numericInput(
              inputId = ns("bb_E"),
              label = "East:",
              value = NULL,
              min = -180,
              max = 180,
              step = 0.00001
            )
          )
        ),
        # Clear button
        br(),
        fluidRow(
          column(width = 3,
                 shiny::actionButton(inputId = ns("clear_map"), label = label, width = "100%")
                 )
        )
    )
    )
  )
}

mod_map_bboxServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values FIRST
    bbox_reVal <- shiny::reactiveValues(bBox = NULL)

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
          rectangleOptions = leaflet.extras::drawRectangleOptions(
            shapeOptions = leaflet.extras::drawShapeOptions(),
            showArea = FALSE
          ),
          singleFeature = TRUE
        ) |>
        leafem::addMouseCoordinates() |>
        add_USGS_base() |>
        leaflet::setView(lng = -114, lat = 42, zoom = 3)

      return(m)
    })

    # Clear the map
    shiny::observeEvent(input$clear_map, {
      proxy <- leaflet::leafletProxy("map_bbox", session = session)
      # remove both the toolbar and any drawn features
      proxy |> leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
      # immediately re-add the toolbar so the user can draw again
      proxy |> leaflet.extras::addDrawToolbar(
        targetGroup = "drawn_items",
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = leaflet.extras::drawRectangleOptions(),
        singleFeature = TRUE
      )
      
      bbox_reVal$bBox <- NULL
      
      shiny::updateNumericInput(session = session, inputId = "bb_W", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_S", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_E", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_N", value = NA)
    })

    # Handle new drawings
    shiny::observeEvent(input$map_bbox_draw_new_feature, {
      feat <- input$map_bbox_draw_new_feature
      coords <- unlist(feat$geometry$coordinates)
      coords_m <- matrix(coords, ncol = 2, byrow = TRUE)

      # Create proper bbox
      poly <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords_m))), crs = sf::st_crs(4326))
      bbox_temp <- unname(sf::st_bbox(poly))

      # Store as bbox object
      bbox_reVal$bBox <- bbox_temp
    })
    
    # Update numeric inputs when bbox changes: Map to inputs
    shiny::observe({
      if (!is.null(bbox_reVal$bBox)) {
        shiny::updateNumericInput(session = session, 
                                  inputId = "bb_W", 
                                  value = bbox_reVal$bBox[1])  # xmin = West
        
        shiny::updateNumericInput(session = session, 
                                  inputId = "bb_S", 
                                  value = bbox_reVal$bBox[2])  # ymin = South
        
        shiny::updateNumericInput(session = session, 
                                  inputId = "bb_E", 
                                  value = bbox_reVal$bBox[3])  # xmax = East
        
        shiny::updateNumericInput(session = session, 
                                  inputId = "bb_N", 
                                  value = bbox_reVal$bBox[4])  # ymax = North
      }
    })

    return(bbox_reVal)
  })
}