# R/mod_map.R

#' Map Module
#'
#' @param id          Internal parameters for {shiny}
#' @param data        A **reactive** returning an sf (or Spatial*) with column `huc8`
#' @param selected    A **reactive** character vector of `huc8` to highlight externally
#' @return            A list with `selected` reactive
#' @noRd

mod_mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

mod_mapServer <- function(id, 
                          data, 
                          selected = reactive({ character(0) }),
                          highlight_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Add a safeguard for initial map rendering
    map_ready <- reactiveVal(FALSE)
    
    # 1) render base map
    output$map <- leaflet::renderLeaflet({
      req(data())
      # Create the map
      m <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(
          data = data(),
          layerId   = ~huc8,
          fill       = TRUE,
          fillColor  = "blue",
          color      = "blue",
          weight     = 1,
          group      = "base_map",
          label      = ~huc8
        ) %>%
        add_USGS_base()
      
      # Signal that the map is ready
      map_ready(TRUE)
      
      return(m)
    })
    
    # 2) track selection in a reactiveValues
    events <- reactiveValues(selected = character(0))
    
    # a) map‐click → toggle
    observeEvent(input$map_shape_click, {
      req(map_ready(), input$map_shape_click$id)
      # Ensure data is available
      req(data())
      
      sel <- events$selected
      click_id <- input$map_shape_click$id
      
      # Validate the click_id exists in the data
      if (click_id %in% data()$huc8) {
        events$selected <- if (click_id %in% sel) {
          setdiff(sel, click_id)
        } else {
          c(sel, click_id)
        }
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # b) external `selected()` → overwrite
    observe({
      # Ensure selected() is ready
      req(selected)
      new_sel <- selected()
      
      # coerce NULL → empty character
      events$selected <- if (is.null(new_sel)) {
        character(0)
      } else {
        # Ensure values exist in data if data is available
        if (!is.null(data()) && length(new_sel) > 0) {
          new_sel[new_sel %in% data()$huc8]
        } else {
          new_sel
        }
      }
    })
    
    # 3) highlight whatever's in events$selected **or** highlight_data()
    observe({
      # Make sure highlight_data exists before trying to use it
      req(!is.null(highlight_data), map_ready())
      
      proxy <- leaflet::leafletProxy(ns("map"), session)
      proxy %>% leaflet::clearGroup("highlighted_polygon")
      
      # Use isolate to avoid dependency cycle if needed
      sel_sf <- highlight_data()
      
      # Verify we have valid data
      if (!is.null(sel_sf) && inherits(sel_sf, "sf") && nrow(sel_sf) > 0) {
        proxy %>% leaflet::addPolylines(
          data = sel_sf,
          group = "highlighted_polygon",
          color = "red",
          weight = 2
        )
      }
    })
    
    # 4) expose a single reactive for selected IDs
    list(
      selected = reactive(events$selected)
    )
  })
}