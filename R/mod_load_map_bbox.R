#' map_bbox UI Function (two-click drawing; no leaflet.extras)
#'
#' @description A shiny Module with address search and three-decimal rounding.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label Label for the "Clear" button.
#' @param step Numeric increment for inputs (default 0.001 degrees).
#'
#' @noRd
#' @importFrom shiny NS tagList
mod_map_bboxUI <- function(id, label = "Clear Drawing", step = 0.001) {
  ns <- NS(id)
  
  htmltools::div(
    class = "tada-bbox",
    shiny::fluidRow(
      # Map + address search
      shiny::column(
        width = 6, class = "tada-bbox-map",
        htmltools::div(
          class = "form-group",
          htmltools::tags$label(
            class = "control-label",
            shiny::tags$span(
              "Search address or place ",
              shiny::tags$i(
                class = "glyphicon glyphicon-info-sign",
                `data-toggle` = "tooltip",
                title = "'Search address or place' only pans/zooms the map and does not set the bounding box."
              )
            )
          ),
          shiny::textInput(
            inputId = ns("addr"),
            label = NULL,
            placeholder = "e.g., 1200 Pennsylvania Ave NW, Washington, DC or 20002",
            width = "100%"
          )
        ),
        shiny::actionButton(ns("addr_find"), "Find", icon = shiny::icon("search")),
        # Press Enter to trigger Find
        htmltools::tags$script(htmltools::HTML(sprintf("
  (function(){
    var input = document.getElementById('%s');
    if (input) {
      input.addEventListener('keydown', function(e){
        if (e.key === 'Enter') {
          e.preventDefault();
          // Force value to sync to Shiny, then click Find on next tick
          var btn = document.getElementById('%s');
          this.blur();
          setTimeout(function(){ if (btn) btn.click(); }, 0);
        }
      });
    }
  })();
", ns("addr"), ns("addr_find")))),
        htmltools::br(), htmltools::br(),
        
        leaflet::leafletOutput(ns("map_bbox")),
        htmltools::p(
          class = "tada-note",
          "Click two opposite corners on the map to draw a bounding box."
        )
      ),
      
      # Coordinate inputs
      shiny::column(
        width = 6, class = "tada-bbox-controls",
        # North (top, centered)
        shiny::fluidRow(
          shiny::column(
            width = 6, offset = 3,
            shiny::numericInput(ns("bb_N"), "North", value = NA_real_, min = -90, max = 90, step = step, width = "100%")
          )
        ),
        # West / East (middle)
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::numericInput(
              ns("bb_W"), "West", value = NA_real_,
              min = -180, max = 180, step = step, width = "100%"
            )
          ),
          shiny::column(
            width = 6,
            shiny::numericInput(
              ns("bb_E"), "East", value = NA_real_,
              min = -180, max = 180, step = step, width = "100%"
            )
          )
        ),
        # South (bottom, centered)
        shiny::fluidRow(
          shiny::column(
            width = 6, offset = 3,
            shiny::numericInput(
              ns("bb_S"), "South", value = NA_real_,
              min = -90, max = 90, step = step, width = "100%"
            )
          )
        ),
        # Clear button
        shiny::fluidRow(
          shiny::column(
            width = 6, offset = 3,
            shiny::actionButton(ns("clear_map"), label = label, width = "100%")
          )
        )
      )
    )
  )
}

#' map_bbox Server Function
#'
#' @noRd
mod_map_bboxServer <- function(id, increment = 0.001, debounce_ms = 500) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive state
    bbox_reVal <- shiny::reactiveValues(bBox = NULL)
    sync_in_progress <- shiny::reactiveVal(FALSE)
    last_search <- shiny::reactiveVal(as.numeric(Sys.time()) - 5) # for rate limiting
    
    last_addr <- shiny::reactiveVal("")
    shiny::observeEvent(input$addr, {
      q <- trimws(if (is.null(input$addr)) "" else input$addr)
      if (nzchar(q)) last_addr(q)
    }, ignoreInit = TRUE)
    
    # Snap to increment, clamp to range, round to 3 decimals
    round_to_inc <- function(x, inc, minv, maxv, digits = 3) {
      if (is.null(x) || is.na(x)) return(x)
      out <- round(x / inc) * inc
      out <- max(min(out, maxv), minv)
      round(out, digits)
    }
    
    # Address geocoding helper (Nominatim)
    geocode_nominatim <- function(q) {
      url <- "https://nominatim.openstreetmap.org/search"
      resp <- tryCatch({
        httr2::request(url) |>
          httr2::req_url_query(q = q, format = "json", limit = 1, countrycodes = "us") |>
          httr2::req_user_agent("TADAShiny/1.0 (https://epa.gov; contact: mywaterway@epa.gov)") |>
          httr2::req_timeout(10) |>
          httr2::req_perform()
      }, error = function(e) NULL)
      if (is.null(resp)) return(NULL)
      
      txt <- httr2::resp_body_string(resp)
      dat <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
      if (is.null(dat)) return(NULL)
      
      # Helper to parse lon/lat and boundingbox from a row-like object
      parse_hit <- function(hit) {
        lon <- suppressWarnings(as.numeric(if (!is.null(hit$lon)) hit$lon else NA_real_))
        lat <- suppressWarnings(as.numeric(if (!is.null(hit$lat)) hit$lat else NA_real_))
        if (!is.finite(lon) || !is.finite(lat)) return(NULL)
        
        bb <- NULL
        if (!is.null(hit$boundingbox)) {
          bb_raw <- hit$boundingbox
          # boundingbox can arrive as character vector, list, or data.frame row
          if (is.character(bb_raw)) {
            bb <- suppressWarnings(as.numeric(bb_raw))
          } else if (is.list(bb_raw)) {
            bb <- suppressWarnings(as.numeric(unlist(bb_raw, use.names = FALSE)))
          } else if (is.data.frame(bb_raw)) {
            bb <- suppressWarnings(as.numeric(unlist(bb_raw[1, , drop = TRUE], use.names = FALSE)))
          }
        }
        bbox <- if (!is.null(bb) && length(bb) == 4 && all(is.finite(bb))) c(bb[3], bb[1], bb[4], bb[2]) else NULL
        list(center = c(lon, lat), bbox = bbox)
      }
      
      # dat is usually a data.frame with columns lon/lat/boundingbox (possibly nested)
      if (is.data.frame(dat) && nrow(dat) >= 1) {
        hit <- lapply(names(dat), function(nm) dat[[nm]][1])
        names(hit) <- names(dat)
        return(parse_hit(hit))
      }
      
      # Fallback: if dat is a list (array of hits), take the first element
      if (is.list(dat) && length(dat) >= 1) {
        return(parse_hit(dat[[1]]))
      }
      
      NULL
    }
    
    geocode_census <- function(q) {
      url <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
      resp <- tryCatch({
        httr2::request(url) |>
          httr2::req_url_query(address = q, benchmark = "Public_AR_Current", format = "json") |>
          httr2::req_timeout(10) |>
          httr2::req_perform()
      }, error = function(e) NULL)
      if (is.null(resp)) return(NULL)
      dat <- tryCatch(jsonlite::fromJSON(httr2::resp_body_string(resp)), error = function(e) NULL)
      if (is.null(dat)) return(NULL)
      res <- dat$result$addressMatches
      if (length(res) < 1) return(NULL)
      coords <- res$coordinates[[1]]
      if (is.null(coords$x) || is.null(coords$y)) return(NULL)
      list(center = c(as.numeric(coords$x), as.numeric(coords$y)), bbox = NULL)
    }
    
    # Render base map
    output$map_bbox <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leafem::addMouseCoordinates() |>
        add_USGS_base() |>
        leaflet::setView(lng = -114, lat = 42, zoom = 3)
    })
    
    # Leaflet proxy for most updates
    map_proxy <- leaflet::leafletProxy("map_bbox", session = session)
    
    # Address search handler
    shiny::observeEvent(input$addr_find, {
      q <- trimws(if (is.null(input$addr)) "" else input$addr)
      if (!nzchar(q)) {
        q <- if (is.null(last_addr())) "" else last_addr()
      }
      
      # ZIP-aware minimum validation
      is_zip5 <- grepl("^[0-9]{5}$", q)
      if (!is_zip5 && nchar(q) < 3) {
        shiny::showNotification(
          "Enter a 5-digit ZIP code or at least 3 characters to search.",
          type = "message", duration = 3
        )
        return()
      }
      
      # Rate limit: <= 1 req/sec
      now <- as.numeric(Sys.time())
      if ((now - last_search()) < 1) {
        shiny::showNotification("Please wait a moment before searching again.", type = "message", duration = 3)
        return()
      }
      last_search(now)
      
      # Optional: disable button during request if shinyjs is available
      if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::disable(ns("addr_find"))
      on.exit({
        if (requireNamespace("shinyjs", quietly = TRUE)) shinyjs::enable(ns("addr_find"))
      }, add = TRUE)
      
      # Clear previous search marker
      leaflet::leafletProxy("map_bbox", session = session) |>
        leaflet::clearGroup("search_center")
      
      # Offline/CI safety
      offline <- tryCatch(isTRUE(.tadas_offline()), error = function(...) FALSE)
      if (offline) {
        shiny::showNotification("Address lookup is unavailable in offline mode.", type = "warning", duration = 5)
        return()
      }
      
      # Geocode via Nominatim, then Census as fallback
      res <- geocode_nominatim(q)
      if (is.null(res)) res <- geocode_census(q)
      if (is.null(res)) {
        shiny::showNotification("No results found for that address.", type = "warning", duration = 5)
        return()
      }
      
      lon <- res$center[1]; lat <- res$center[2]
      
      # Zoom: fit bounds if available, otherwise fly to center
      if (!is.null(res$bbox) && length(res$bbox) == 4 && all(is.finite(res$bbox))) {
        leaflet::leafletProxy("map_bbox", session = session) |>
          leaflet::fitBounds(lng1 = res$bbox[1], lat1 = res$bbox[2], lng2 = res$bbox[3], lat2 = res$bbox[4])
      } else {
        leaflet::leafletProxy("map_bbox", session = session) |>
          leaflet::flyTo(lng = lon, lat = lat, zoom = 12)
      }
      
      # Drop a marker to show search center
      leaflet::leafletProxy("map_bbox", session = session) |>
        leaflet::addCircleMarkers(
          lng = lon, lat = lat,
          radius = 5, color = "#111827", fillColor = "#111827",
          fillOpacity = 0.9, stroke = FALSE,
          group = "search_center", label = "Search result"
        )
    })
    
    # Rectangle style
    shape_opts <- list(
      stroke = TRUE, color = "#3388ff", weight = 4, opacity = 1,
      fill = TRUE, fillColor = "#3388ff", fillOpacity = 0.2,
      smoothFactor = 1, noClip = FALSE
    )
    
    # Two-click drawing state
    draw_state <- shiny::reactiveValues(first_corner = NULL)
    
    # Clear the map and inputs
    shiny::observeEvent(input$clear_map, {
      map_proxy |>
        leaflet::clearGroup("manual_bbox") |>
        leaflet::clearGroup("corner_pt") |>
        leaflet::clearGroup("search_center")
      bbox_reVal$bBox <- NULL
      draw_state$first_corner <- NULL
      
      sync_in_progress(TRUE)
      shiny::updateNumericInput(session, "bb_W", value = NA)
      shiny::updateNumericInput(session, "bb_S", value = NA)
      shiny::updateNumericInput(session, "bb_E", value = NA)
      shiny::updateNumericInput(session, "bb_N", value = NA)
      sync_in_progress(FALSE)
    })
    
    # Two-click rectangle creation using map clicks
    shiny::observeEvent(input$map_bbox_click, {
      click <- input$map_bbox_click
      if (is.null(click)) return()
      
      lng <- click$lng
      lat <- click$lat
      
      # First click: store corner
      if (is.null(draw_state$first_corner)) {
        draw_state$first_corner <- c(lng = lng, lat = lat)
        map_proxy |>
          leaflet::clearGroup("corner_pt") |>
          leaflet::addCircleMarkers(
            lng = lng, lat = lat,
            radius = 6, color = "#FF5722", fillColor = "#FF5722",
            fillOpacity = 0.9, stroke = FALSE, group = "corner_pt"
          )
        return()
      }
      
      # Second click: compute bbox and draw rectangle
      lng1 <- draw_state$first_corner["lng"]
      lat1 <- draw_state$first_corner["lat"]
      west  <- min(lng1, lng)
      east  <- max(lng1, lng)
      south <- min(lat1, lat)
      north <- max(lat1, lat)
      
      # Snap to increment and clamp
      west  <- round_to_inc(west,  increment, -180, 180)
      east  <- round_to_inc(east,  increment, -180, 180)
      south <- round_to_inc(south, increment,  -90,  90)
      north <- round_to_inc(north, increment,  -90,  90)
      
      # Validate bbox
      if (west < east && south < north &&
          west >= -180 && east <= 180 &&
          south >= -90 && north <= 90) {
        
        map_proxy |>
          leaflet::clearGroup("corner_pt") |>
          leaflet::clearGroup("manual_bbox") |>
          leaflet::addRectangles(
            lng1 = west, lat1 = south, lng2 = east, lat2 = north,
            stroke = shape_opts$stroke, color = shape_opts$color,
            weight = shape_opts$weight, opacity = shape_opts$opacity,
            fill = shape_opts$fill, fillColor = shape_opts$fillColor,
            fillOpacity = shape_opts$fillOpacity, smoothFactor = shape_opts$smoothFactor,
            noClip = shape_opts$noClip, group = "manual_bbox"
          )
        
        # Update reactive bbox
        bbox_reVal$bBox <- c(west, south, east, north)
      }
      
      # Reset for next draw
      draw_state$first_corner <- NULL
    })
    
    # Map inputs: update numeric inputs when bbox changes
    shiny::observe({
      if (!is.null(bbox_reVal$bBox)) {
        sync_in_progress(TRUE)
        shiny::updateNumericInput(session, "bb_W", value = bbox_reVal$bBox[1]) # west
        shiny::updateNumericInput(session, "bb_S", value = bbox_reVal$bBox[2]) # south
        shiny::updateNumericInput(session, "bb_E", value = bbox_reVal$bBox[3]) # east
        shiny::updateNumericInput(session, "bb_N", value = bbox_reVal$bBox[4]) # north
        sync_in_progress(FALSE)
      }
    })
    
    # Inputs map: debounced observers
    bb_W_debounce <- shiny::debounce(shiny::reactive(input$bb_W), debounce_ms)
    bb_S_debounce <- shiny::debounce(shiny::reactive(input$bb_S), debounce_ms)
    bb_E_debounce <- shiny::debounce(shiny::reactive(input$bb_E), debounce_ms)
    bb_N_debounce <- shiny::debounce(shiny::reactive(input$bb_N), debounce_ms)
    
    shiny::observe({
      if (sync_in_progress()) return()
      
      west  <- bb_W_debounce()
      south <- bb_S_debounce()
      east  <- bb_E_debounce()
      north <- bb_N_debounce()
      
      # Treat NULL/NA as missing
      is_missing <- function(x) is.null(x) || is.na(x)
      if (is_missing(west) || is_missing(south) || is_missing(east) || is_missing(north)) {
        map_proxy |>
          leaflet::clearGroup("manual_bbox") |>
          leaflet::clearGroup("corner_pt")
        return()
      }
      
      # Snap to increment and clamp
      r_west  <- round_to_inc(west,  increment, -180, 180)
      r_east  <- round_to_inc(east,  increment, -180, 180)
      r_south <- round_to_inc(south, increment,  -90,  90)
      r_north <- round_to_inc(north, increment,  -90,  90)
      
      # Reflect any rounding back into inputs
      if (!identical(c(west, south, east, north), c(r_west, r_south, r_east, r_north))) {
        sync_in_progress(TRUE)
        shiny::updateNumericInput(session, "bb_W", value = r_west)
        shiny::updateNumericInput(session, "bb_S", value = r_south)
        shiny::updateNumericInput(session, "bb_E", value = r_east)
        shiny::updateNumericInput(session, "bb_N", value = r_north)
        sync_in_progress(FALSE)
        west <- r_west; south <- r_south; east <- r_east; north <- r_north
      }
      
      # Validate bbox
      if (west >= east || south >= north) {
        map_proxy |> leaflet::clearGroup("manual_bbox") |> leaflet::clearGroup("corner_pt")
        return()
      }
      if (west < -180 || west > 180 || east < -180 || east > 180 ||
          south < -90 || south > 90 || north < -90 || north > 90) {
        map_proxy |> leaflet::clearGroup("manual_bbox") |> leaflet::clearGroup("corner_pt")
        return()
      }
      
      # Draw new rectangle from numeric inputs
      map_proxy |>
        leaflet::clearGroup("manual_bbox") |>
        leaflet::clearGroup("corner_pt") |>
        leaflet::addRectangles(
          lng1 = west, lat1 = south, lng2 = east, lat2 = north,
          stroke = shape_opts$stroke, color = shape_opts$color,
          weight = shape_opts$weight, opacity = shape_opts$opacity,
          fill = shape_opts$fill, fillColor = shape_opts$fillColor,
          fillOpacity = shape_opts$fillOpacity, smoothFactor = shape_opts$smoothFactor,
          noClip = shape_opts$noClip, group = "manual_bbox"
        )
      
      # Keep reactive bbox in sync
      bbox_reVal$bBox <- c(west, south, east, north)
    }) |> shiny::bindEvent(
      bb_W_debounce(), bb_S_debounce(), bb_E_debounce(), bb_N_debounce(),
      ignoreInit = TRUE
    )
    
    return(bbox_reVal)
  })
}
