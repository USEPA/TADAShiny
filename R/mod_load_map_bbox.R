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
  
  bbox_increment <- 1
  
  tagList(
    shiny::fluidRow(
      column(
        width = 6,
        leaflet::leafletOutput(ns("map_bbox"))
      ),
      # Bounding box inputs on the right (takes 4 columns)
      column(
        width = 6,
        htmltools::h4("Bounding Box Latitude and Longitude"),
        shiny::fluidRow(
          column(
            width = 3,
            htmltools::br(),
            htmltools::br(),
            # West coordinate
            shiny::numericInput(
              inputId = ns("bb_W"),
              label = "West:",
              value = NULL,
              min = -180,
              max = 180,
              step = bbox_increment
            )
          ),
          column(
            width = 3,
            # North coordinate
            shiny::numericInput(
              inputId = ns("bb_N"),
              label = "North:",
              value = NULL,
              min = -90,
              max = 90,
              step = bbox_increment
            ),
            htmltools::br(),
            htmltools::br(),
            # South coordinate
            shiny::numericInput(
              inputId = ns("bb_S"),
              label = "South:",
              value = NULL,
              min = -90,
              max = 90,
              step = bbox_increment
            )
          ),
          column(
            width = 3,
            htmltools::br(),
            htmltools::br(),
            # East coordinate
            shiny::numericInput(
              inputId = ns("bb_E"),
              label = "East:",
              value = NULL,
              min = -180,
              max = 180,
              step = bbox_increment
            )
          )
        ),
        # Clear button
        htmltools::br(),
        shiny::fluidRow(
          column(
            width = 3,
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

    # Flag to prevent infinite loops when syncing
    sync_in_progress <- shiny::reactiveVal(FALSE)

    # Get the default leaflet style
    shape_opts <- leaflet.extras::drawShapeOptions()

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

    # Create leaflet proxy for updates
    map_proxy <- leaflet::leafletProxy("map_bbox", session = session)

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

      # Clear manual rectangle
      map_proxy |> leaflet::clearGroup("manual_bbox")

      bbox_reVal$bBox <- NULL

      # Clear numeric inputs
      sync_in_progress(TRUE)

      shiny::updateNumericInput(session = session, inputId = "bb_W", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_S", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_E", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_N", value = NA)

      sync_in_progress(FALSE)
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

      # Clear any manual rectangle users draw the map
      map_proxy |> leaflet::clearGroup("manual_bbox")
    })

    # Update numeric inputs when bbox changes: Map to inputs
    shiny::observe({
      if (!is.null(bbox_reVal$bBox)) {
        sync_in_progress(TRUE)

        shiny::updateNumericInput(
          session = session,
          inputId = "bb_W",
          value = bbox_reVal$bBox[1]
        ) # xmin = West

        shiny::updateNumericInput(
          session = session,
          inputId = "bb_S",
          value = bbox_reVal$bBox[2]
        ) # ymin = South

        shiny::updateNumericInput(
          session = session,
          inputId = "bb_E",
          value = bbox_reVal$bBox[3]
        ) # xmax = East

        shiny::updateNumericInput(
          session = session,
          inputId = "bb_N",
          value = bbox_reVal$bBox[4]
        ) # ymax = North
        sync_in_progress(FALSE)
      }
    })

    # Create debounced inputs to avoid excessive updates
    bb_W_debounce <- shiny::debounce(shiny::reactive(input$bb_W), 1000)
    bb_S_debounce <- shiny::debounce(shiny::reactive(input$bb_S), 1000)
    bb_E_debounce <- shiny::debounce(shiny::reactive(input$bb_E), 1000)
    bb_N_debounce <- shiny::debounce(shiny::reactive(input$bb_N), 1000)

    # Update map when numeric inputs change: Inputs → Map
    shiny::observe({
      # Don't update if sync is in progress: prevents infinite loops
      if (sync_in_progress()) {
        return()
      }

      # Get debounced values
      west <- bb_W_debounce()
      south <- bb_S_debounce()
      east <- bb_E_debounce()
      north <- bb_N_debounce()

      # Validate all inputs are present
      if (is.na(west) || is.na(south) || is.na(east) || is.na(north)) {
        # If any input is missing, clear the manual rectangle
        map_proxy |> leaflet::clearGroup("manual_bbox")
        return()
      }

      # Validate coordinates are valid
      if (west >= east) {
        # Invalid: west should be less than east
        return()
      }

      if (south >= north) {
        # Invalid: south should be less than north
        return()
      }

      # Validate within bounds
      if (west < -180 || west > 180 || east < -180 || east > 180 ||
        south < -90 || south > 90 || north < -90 || north > 90) {
        return()
      }

      # Clear existing manual rectangle
      map_proxy |> leaflet.extras::removeDrawToolbar(clearFeatures = TRUE)
      map_proxy |> leaflet::clearGroup("manual_bbox")

      # Re-add the toolbar so user can draw again
      map_proxy |> leaflet.extras::addDrawToolbar(
        targetGroup = "drawn_items",
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = leaflet.extras::drawRectangleOptions(
          shapeOptions = leaflet.extras::drawShapeOptions(),
          showArea = FALSE
        ),
        singleFeature = TRUE
      )

      # Draw new rectangle with same color as drawn rectangles
      map_proxy |>
        leaflet::addRectangles(
          lng1 = west,
          lat1 = south,
          lng2 = east,
          lat2 = north,
          stroke = shape_opts$stroke,
          color = shape_opts$color,
          weight = shape_opts$weight,
          opacity = shape_opts$opacity,
          fill = shape_opts$fill,
          fillColor = shape_opts$fillColor,
          fillOpacity = shape_opts$fillOpacity,
          smoothFactor = shape_opts$smoothFactor,
          noClip = shape_opts$noClip,
          group = "manual_bbox"
        )

      # Update bbox_reVal to keep it in sync
      bbox_reVal$bBox <- c(west, south, east, north)
    }) |> shiny::bindEvent(
      bb_W_debounce(), bb_S_debounce(),
      bb_E_debounce(), bb_N_debounce()
    )

    return(bbox_reVal)
  })
}
