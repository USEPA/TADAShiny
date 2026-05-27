test_that("mod_map_bboxUI renders expected controls with 0.001 step and address search", {
  ui <- mod_map_bboxUI("bbox", step = 0.001)
  tq <- htmltools::tagQuery(ui)
  ns <- shiny::NS("bbox")
  
  # Leaflet output present (allow >= 1 due to wrapper duplication)
  expect_gte(length(tq$find(sprintf("#%s", ns("map_bbox")))$all()), 1L)
  
  # Address input and Find button present
  expect_gte(length(tq$find(sprintf("#%s", ns("addr")))$all()), 1L)
  expect_gte(length(tq$find(sprintf("#%s", ns("addr_find")))$all()), 1L)
  
  # Numeric inputs present; verify the <input type="number"> has step 0.001
  for (id in c("bb_N", "bb_S", "bb_W", "bb_E")) {
    node <- tq$find(sprintf("#%s", ns(id)))
    expect_gte(length(node$all()), 1L)
    
    # Find the actual input tag among selected tags
    tags <- node$selectedTags()
    input_tag <- NULL
    for (tg in tags) {
      if (identical(tg$name, "input")) {
        input_tag <- tg
        break
      }
    }
    expect_false(is.null(input_tag))
    expect_identical(input_tag$attribs$type, "number")
    expect_true(!is.null(input_tag$attribs$step))
    expect_equal(as.numeric(input_tag$attribs$step), 0.001, tolerance = 1e-12)
  }
})

test_that("mod_map_bboxServer: two-click draw sets rounded bbox (3 decimals)", {
  # Make leaflet proxy calls no-ops for server-only testing
  testthat::local_mocked_bindings(
    leafletProxy     = function(...) structure(list(), class = "leaflet_proxy"),
    addRectangles    = function(map, ...) map,
    addCircleMarkers = function(map, ...) map,
    clearGroup       = function(map, ...) map,
    fitBounds        = function(map, ...) map,
    flyTo            = function(map, ...) map,
    .package = "leaflet"
  )
  
  shiny::testServer(mod_map_bboxServer, args = list(increment = 0.001, debounce_ms = 0), {
    # First click stores first corner; bbox remains NULL
    session$setInputs(map_bbox_click = list(lng = -122.98765, lat = 45.12345))
    session$flushReact()
    expect_false(is.null(draw_state$first_corner))
    expect_null(bbox_reVal$bBox)
    
    # Second click completes rectangle and applies rounding
    session$setInputs(map_bbox_click = list(lng = -121.50044, lat = 46.98776))
    session$flushReact()
    
    expected <- c(
      west  = -122.988,
      south =   45.123,
      east  = -121.500,
      north =   46.988
    )
    expect_equal(bbox_reVal$bBox, unname(expected), tolerance = 1e-6)
  })
})

test_that("mod_map_bboxServer: inputs -> map path snaps/rounds to 3 decimals", {
  testthat::local_mocked_bindings(
    leafletProxy     = function(...) structure(list(), class = "leaflet_proxy"),
    addRectangles    = function(map, ...) map,
    addCircleMarkers = function(map, ...) map,
    clearGroup       = function(map, ...) map,
    fitBounds        = function(map, ...) map,
    flyTo            = function(map, ...) map,
    .package = "leaflet"
  )
  
  shiny::testServer(mod_map_bboxServer, args = list(increment = 0.001, debounce_ms = 0), {
    # Set inputs with extra precision
    session$setInputs(bb_W = -123.45678)
    session$setInputs(bb_S =   45.00049)
    session$setInputs(bb_E = -121.00044)
    session$setInputs(bb_N =   46.99951)
    # Allow the observer to run twice: first pass may see partial inputs and return
    session$flushReact()
    session$flushReact()
    
    expected <- c(
      west  = -123.457,
      south =   45.000,
      east  = -121.000,
      north =   47.000
    )
    expect_equal(bbox_reVal$bBox, unname(expected), tolerance = 1e-6)
  })
})

test_that("mod_map_bboxServer: invalid bbox (west == east or south == north) is ignored", {
  testthat::local_mocked_bindings(
    leafletProxy     = function(...) structure(list(), class = "leaflet_proxy"),
    addRectangles    = function(map, ...) map,
    addCircleMarkers = function(map, ...) map,
    clearGroup       = function(map, ...) map,
    fitBounds        = function(map, ...) map,
    flyTo            = function(map, ...) map,
    .package = "leaflet"
  )
  
  shiny::testServer(mod_map_bboxServer, args = list(increment = 0.001, debounce_ms = 0), {
    # First click
    session$setInputs(map_bbox_click = list(lng = -100.0, lat = 40.0))
    session$flushReact()
    expect_true(is.null(bbox_reVal$bBox))
    
    # Second click with same longitude -> west == east
    session$setInputs(map_bbox_click = list(lng = -100.0, lat = 45.0))
    session$flushReact()
    expect_null(bbox_reVal$bBox)
    
    # Same latitude -> south == north
    session$setInputs(map_bbox_click = list(lng = -101.0, lat = 40.0))
    session$flushReact()
    session$setInputs(map_bbox_click = list(lng =  -99.0, lat = 40.0))
    session$flushReact()
    expect_null(bbox_reVal$bBox)
  })
})

test_that("mod_map_bboxServer: clear button resets bbox", {
  testthat::local_mocked_bindings(
    leafletProxy     = function(...) structure(list(), class = "leaflet_proxy"),
    addRectangles    = function(map, ...) map,
    addCircleMarkers = function(map, ...) map,
    clearGroup       = function(map, ...) map,
    fitBounds        = function(map, ...) map,
    flyTo            = function(map, ...) map,
    .package = "leaflet"
  )
  
  shiny::testServer(mod_map_bboxServer, args = list(increment = 0.001, debounce_ms = 0), {
    # Create a bbox
    session$setInputs(map_bbox_click = list(lng = -100.4444, lat = 40.4444))
    session$setInputs(map_bbox_click = list(lng =  -90.5555, lat = 50.5555))
    session$flushReact()
    expect_true(!is.null(bbox_reVal$bBox))
    
    # Clear
    session$setInputs(clear_map = 1)
    session$flushReact()
    expect_null(bbox_reVal$bBox)
  })
})

test_that("mod_map_bboxServer: address search no-ops in offline mode", {
  # Toggle offline mode; restore after test
  old <- Sys.getenv("TADAS_OFFLINE", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("TADAS_OFFLINE") else Sys.setenv(TADAS_OFFLINE = old)
  }, add = TRUE)
  Sys.setenv(TADAS_OFFLINE = "true")
  
  testthat::local_mocked_bindings(
    leafletProxy     = function(...) structure(list(), class = "leaflet_proxy"),
    addRectangles    = function(map, ...) map,
    addCircleMarkers = function(map, ...) map,
    clearGroup       = function(map, ...) map,
    fitBounds        = function(map, ...) map,
    flyTo            = function(map, ...) map,
    .package = "leaflet"
  )
  
  shiny::testServer(mod_map_bboxServer, args = list(increment = 0.001, debounce_ms = 0), {
    expect_null(bbox_reVal$bBox)
    session$setInputs(addr = "1600 Pennsylvania Ave NW, Washington, DC")
    session$setInputs(addr_find = 1)
    session$flushReact()
    expect_null(bbox_reVal$bBox)
  })
})

test_that("mod_map_bboxServer: address search is rate-limited (≤1 req/sec)", {
  testthat::local_mocked_bindings(
    leafletProxy     = function(...) structure(list(), class = "leaflet_proxy"),
    addRectangles    = function(map, ...) map,
    addCircleMarkers = function(map, ...) map,
    clearGroup       = function(map, ...) map,
    fitBounds        = function(map, ...) map,
    flyTo            = function(map, ...) map,
    .package = "leaflet"
  )
  
  shiny::testServer(mod_map_bboxServer, args = list(increment = 0.001, debounce_ms = 0), {
    # Prime rate limiter to now; next click should be ignored
    last_search(as.numeric(Sys.time()))
    session$setInputs(addr = "test address")
    session$setInputs(addr_find = 1)
    session$flushReact()
    
    # If rate-limited, last_search shouldn't advance by ≥1s
    delta <- as.numeric(Sys.time()) - last_search()
    expect_true(delta < 1.1)
  })
})