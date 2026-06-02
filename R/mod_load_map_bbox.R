#' map_bbox UI Function (two-click drawing; no leaflet.extras)
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

  tagList(shiny::fluidRow(
    column(
      width = 6,
      leaflet::leafletOutput(ns("map_bbox")),
      shiny::helpText(
        "Click two opposite corners on the map to draw a bounding box."
      )
    ),
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
      shiny::fluidRow(column(
        width = 3,
        shiny::actionButton(
          inputId = ns("clear_map"),
          label = label,
          width = "100%"
        )
      ))
    )
  ))
}

mod_map_bboxServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values FIRST
    bbox_reVal <- shiny::reactiveValues(bBox = NULL)

    # Flag to prevent infinite loops when syncing
    sync_in_progress <- shiny::reactiveVal(FALSE)

    # Rectangle style (approximate Leaflet defaults)
    shape_opts <- list(
      stroke = TRUE,
      color = "#3388ff",
      weight = 4,
      opacity = 1,
      fill = TRUE,
      fillColor = "#3388ff",
      fillOpacity = 0.2,
      smoothFactor = 1,
      noClip = FALSE
    )

    # Store first corner for two-click drawing
    draw_state <- shiny::reactiveValues(first_corner = NULL)

    # Render base map
    output$map_bbox <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leafem::addMouseCoordinates() |>
        add_USGS_base() |>
        leaflet::setView(lng = -114, lat = 42, zoom = 3)
    })

    # Create leaflet proxy for updates
    map_proxy <- leaflet::leafletProxy("map_bbox", session = session)

    # Clear the map and inputs
    shiny::observeEvent(input$clear_map, {
      map_proxy |>
        leaflet::clearGroup("manual_bbox") |>
        leaflet::clearGroup("corner_pt")

      bbox_reVal$bBox <- NULL
      draw_state$first_corner <- NULL

      # Clear numeric inputs
      sync_in_progress(TRUE)
      shiny::updateNumericInput(session = session, inputId = "bb_W", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_S", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_E", value = NA)
      shiny::updateNumericInput(session = session, inputId = "bb_N", value = NA)
      sync_in_progress(FALSE)
    })

    # Two-click rectangle creation using map clicks
    shiny::observeEvent(input$map_bbox_click, {
      click <- input$map_bbox_click
      if (is.null(click)) {
        return()
      }

      lng <- click$lng
      lat <- click$lat

      # First click: store corner and show a small marker
      if (is.null(draw_state$first_corner)) {
        draw_state$first_corner <- c(lng = lng, lat = lat)
        map_proxy |>
          leaflet::clearGroup("corner_pt") |>
          leaflet::addCircleMarkers(
            lng = lng,
            lat = lat,
            radius = 6,
            color = "#FF5722",
            fillColor = "#FF5722",
            fillOpacity = 0.9,
            stroke = FALSE,
            group = "corner_pt"
          )
        return()
      }

      # Second click: compute bbox and draw rectangle
      lng1 <- draw_state$first_corner["lng"]
      lat1 <- draw_state$first_corner["lat"]
      west <- min(lng1, lng)
      east <- max(lng1, lng)
      south <- min(lat1, lat)
      north <- max(lat1, lat)

      # Validate bbox
      if (
        west < east &&
          south < north &&
          west >= -180 &&
          east <= 180 &&
          south >= -90 &&
          north <= 90
      ) {
        map_proxy |>
          leaflet::clearGroup("corner_pt") |>
          leaflet::clearGroup("manual_bbox") |>
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

        # Update reactive bbox (numeric inputs will sync from this)
        bbox_reVal$bBox <- c(west, south, east, north)
      }

      # Reset for next draw
      draw_state$first_corner <- NULL
    })

    # Update numeric inputs when bbox changes: Map → inputs
    shiny::observe({
      if (!is.null(bbox_reVal$bBox)) {
        sync_in_progress(TRUE)
        shiny::updateNumericInput(
          session = session,
          inputId = "bb_W",
          value = bbox_reVal$bBox[1]
        ) # west
        shiny::updateNumericInput(
          session = session,
          inputId = "bb_S",
          value = bbox_reVal$bBox[2]
        ) # south
        shiny::updateNumericInput(
          session = session,
          inputId = "bb_E",
          value = bbox_reVal$bBox[3]
        ) # east
        shiny::updateNumericInput(
          session = session,
          inputId = "bb_N",
          value = bbox_reVal$bBox[4]
        ) # north
        sync_in_progress(FALSE)
      }
    })

    # Debounced inputs: numeric → map
    bb_W_debounce <- shiny::debounce(shiny::reactive(input$bb_W), 1000)
    bb_S_debounce <- shiny::debounce(shiny::reactive(input$bb_S), 1000)
    bb_E_debounce <- shiny::debounce(shiny::reactive(input$bb_E), 1000)
    bb_N_debounce <- shiny::debounce(shiny::reactive(input$bb_N), 1000)

    shiny::observe({
      # Prevent feedback loop when we update inputs from the map
      if (sync_in_progress()) {
        return()
      }

      west <- bb_W_debounce()
      south <- bb_S_debounce()
      east <- bb_E_debounce()
      north <- bb_N_debounce()

      # If any is missing, clear any drawn rectangle and corner marker
      if (is.na(west) || is.na(south) || is.na(east) || is.na(north)) {
        map_proxy |>
          leaflet::clearGroup("manual_bbox") |>
          leaflet::clearGroup("corner_pt")
        return()
      }

      # Validate bbox
      if (west >= east) {
        return()
      }
      if (south >= north) {
        return()
      }
      if (
        west < -180 ||
          west > 180 ||
          east < -180 ||
          east > 180 ||
          south < -90 ||
          south > 90 ||
          north < -90 ||
          north > 90
      ) {
        return()
      }

      # Draw new rectangle from numeric inputs
      map_proxy |>
        leaflet::clearGroup("manual_bbox") |>
        leaflet::clearGroup("corner_pt") |>
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

      # Keep reactive bbox in sync
      bbox_reVal$bBox <- c(west, south, east, north)
    }) |>
      shiny::bindEvent(
        bb_W_debounce(),
        bb_S_debounce(),
        bb_E_debounce(),
        bb_N_debounce()
      )

    return(bbox_reVal)
  })
}
