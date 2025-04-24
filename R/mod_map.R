# R/mod_map.R

#’ Map Module
#’
#’ @param id          Internal parameters for {shiny}
#’ @param data        A **reactive** returning an sf (or Spatial*) with column `huc8`
#’ @param selected    A **reactive** character vector of `huc8` to highlight externally
#’ @return            A list with `selected` reactive
#’ @noRd

mod_mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"))
  )
}

mod_mapServer <- function(id, data, selected = reactive({ character(0) })) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1) render base map
    output$map <- leaflet::renderLeaflet({
      req(data())
      leaflet::leaflet() %>%
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
    })
    
    # 2) track selection in a reactiveValues
    events <- reactiveValues(selected = character(0))
    
    # a) map‐click → toggle
    observeEvent(input$map_shape_click, {
      req(input$map_shape_click$id)
      sel <- events$selected
      click_id <- input$map_shape_click$id
      events$selected <- if (click_id %in% sel) {
        setdiff(sel, click_id)
      } else {
        c(sel, click_id)
      }
    })
    
    # b) external `selected()` → overwrite
    # any time the parent “selected” reactive changes, push it into our internal state
    observe({
      new_sel <- selected()
      # coerce NULL → empty character
      events$selected <- if (is.null(new_sel)) character(0) else new_sel
    })
    
    # 3) highlight whatever’s in events$selected
    observe({
      proxy <- leaflet::leafletProxy(ns("map"), session)
      proxy %>% leaflet::clearGroup("highlighted_polygon")
      sel <- events$selected
      if (length(sel)) {
        sel_sf <- data() %>% dplyr::filter(huc8 %in% sel)
        if (nrow(sel_sf)) {
          proxy %>% leaflet::addPolylines(
            data  = sel_sf,
            group = "highlighted_polygon",
            color = "red",
            weight = 2
          )
        }
      }
    })
    
    # 4) expose a single reactive for selected IDs
    list(
      selected = reactive(events$selected)
    )
  })
}
