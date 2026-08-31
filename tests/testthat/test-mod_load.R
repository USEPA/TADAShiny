# Basic wiring: run a minimal server check inside a test block
testthat::test_that("mod_query_data_server basic NS wiring", {
  tadat <- shiny::reactiveValues()
  
  shiny::testServer(mod_query_data_server, args = list(id = "test", tadat = tadat), {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl("test", ns("")))
    expect_true(grepl("test", ns("test")))
  })
})

testthat::test_that("module ui works", {
  ui <- mod_query_data_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_query_data_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

# tests/testthat/test-example-data-map.R
testthat::test_that("example data map returns data for each entry", {
  testthat::skip_if_not_installed("EPATADA")

  m <- get_example_data_map()

  expect_type(m, "list")
  expect_true(length(m) > 0L)

  for (nm in names(m)) {
    f <- m[[nm]]
    expect_type(f, "closure")

    obj <- f()
    expect_false(is.null(obj), paste0(nm, " returned NULL"))
    expect_true(
      is.data.frame(obj),
      paste0(nm, " did not return a data.frame/tibble")
    )
    expect_true(NROW(obj) > 0L, paste0(nm, " has zero rows"))
    expect_true(NCOL(obj) > 0L, paste0(nm, " has zero columns"))
  }
})

testthat::test_that("restrict_to_keep_cols preserves order, drops extras, and reports messages", {
  # Create a df with a mix of keep and extra columns
  keep_cols <- c("A", "B", "C", "D")
  df <- data.frame(
    C = 3:4,
    X = 1:2, # extra
    A = 5:6,
    Y = 7:8, # extra
    B = 9:10,
    stringsAsFactors = FALSE
  )

  expect_message(
    out <- restrict_to_keep_cols(df, keep_cols = keep_cols, verbose = TRUE),
    regexp = "Removing 2 column\\(s\\): X, Y"
  )
  expect_message(
    out <- restrict_to_keep_cols(df, keep_cols = keep_cols, verbose = TRUE),
    regexp = "Requested but not present in input \\(not added\\): D"
  )

  expect_identical(names(out), c("A", "B", "C"))
  expect_identical(ncol(out), 3L)
})

testthat::test_that("restrict_to_keep_cols emits no messages when verbose = FALSE", {
  keep_cols <- c("A", "B")
  df <- data.frame(A = 1, C = 2, B = 3)
  expect_silent(
    out <- restrict_to_keep_cols(df, keep_cols = keep_cols, verbose = FALSE)
  )
  expect_identical(names(out), c("A", "B"))
})

testthat::test_that("return_tribal_sf returns an sf subset for chosen layer/name", {
  testthat::skip_if_not_installed("sf")

  # Ensure tribal_list is available (loaded from extdata at package load)
  testthat::skip_if_not(is.list(tribal_list), "tribal_list is not available")

  layers <- names(tribal_list)
  testthat::skip_if(length(layers) == 0, "tribal_list has no layers")

  layer <- layers[[1]]
  df_layer <- tribal_list[[layer]]

  testthat::skip_if_not(is.data.frame(df_layer))
  testthat::skip_if_not("TRIBE_NAME" %in% names(df_layer))

  # Take one or two names present in the layer
  take <- unique(df_layer$TRIBE_NAME)[1]
  testthat::skip_if(is.na(take) || length(take) == 0)

  sub <- return_tribal_sf(
    tribal_layer = layer,
    tribal_name = take,
    tribal_list = tribal_list
  )
  expect_true(inherits(sub, "sf") || "sf_column" %in% names(attributes(sub)))
  expect_true(all(sub$TRIBE_NAME %in% take))
})

testthat::test_that("mod_query_data_ui builds a namespaced UI with expected controls", {
  ui <- mod_query_data_ui("query_data_1")
  rendered <- htmltools::renderTags(ui)$html

  # Spot-check a few important inputs are properly namespaced
  expect_match(rendered, 'id="query_data_1-example_data"', perl = TRUE)
  expect_match(rendered, 'id="query_data_1-example_data_go"', perl = TRUE)
  expect_match(rendered, 'id="query_data_1-state"', perl = TRUE)
  expect_match(rendered, 'id="query_data_1-county"', perl = TRUE)
  expect_match(rendered, 'id="query_data_1-querynow"', perl = TRUE)
  expect_match(rendered, 'id="query_data_1-providers"', perl = TRUE)
})

# tests/testthat/test-mod-query-data-server-example.R
testthat::test_that("mod_query_data_server loads example data and initializes tadat", {
  testthat::skip_if_not_installed("shinyjs")
  testthat::skip_if_not_installed("shinybusy")
  testthat::skip_if_not_installed("EPATADA")

  # Ensure the example map exists and has entries
  testthat::skip_if(!exists("example_data_map"), "example_data_map not found")
  testthat::skip_if(
    length(names(example_data_map)) == 0,
    "No example datasets available"
  )

  # Mock the bbox submodule so it doesn't need a real UI
  testthat::local_mocked_bindings(
    mod_map_bboxServer = function(id, ...) {
      # match the shape your server expects
      list(bBox = NULL)
    },
    .env = asNamespace("TADAShiny")
  )

  # A fresh reactiveValues store for the module to populate
  tadat <- shiny::reactiveValues()

  shiny::testServer(mod_query_data_server, args = list(tadat = tadat), {
    # Seed inputs to avoid NULL -> length-0 logical errors
    session$setInputs(
      match_type_selector = "contains",
      text_string = "",
      media = NULL,
      org = NULL,
      project = NULL,
      state = "",
      county = "",
      providers = "all",
      tribe_layer = "",
      tribe_name = ""
    )
    session$flushReact()

    # Trigger the example-data flow
    session$setInputs(example_data = names(example_data_map)[1])
    session$flushReact()
    session$setInputs(example_data_go = 1)
    session$flushReact()

    # Validate side effects
    testthat::expect_true(isTRUE(tadat$ready_for_download))
    testthat::expect_true(is.data.frame(tadat$raw))
    testthat::expect_gt(nrow(tadat$raw), 0L)
    testthat::expect_identical(tadat$original_source, "Example")
    testthat::expect_true(all(names(tadat$raw) %in% all.cols))
  })
})

testthat::test_that("return_tribal_sf filters a provided tribal_list by layer and name", {
  # Build a small fake tribal_list (list of tibbles or data.frames)
  fake_layer <- data.frame(
    TRIBE_NAME = c("Foo", "Bar"),
    x = 1:2,
    stringsAsFactors = FALSE
  )
  fake_tribal_list <- list(layer1 = fake_layer)
  res <- return_tribal_sf("layer1", "Foo", tribal_list = fake_tribal_list)
  testthat::expect_true(is.data.frame(res))
  testthat::expect_equal(nrow(res), 1)
  testthat::expect_equal(res$TRIBE_NAME, "Foo")
})

# initializeTable() and disableLoading() are currently only used by this module
# Helper to create a simple "tadat" environment that behaves like a list
make_tadat_env <- function() {
  tadat <- new.env(parent = emptyenv())
  # Expose list-like $ assignment/get
  class(tadat) <- c("tadat_env", class(tadat))
  tadat
}

testthat::test_that("initializeTable marks tadat as reup when TADA.Remove present", {
  tadat <- make_tadat_env()
  # Create a raw data.frame that already has TADA.Remove column (simulating a previously worked-on dataset)
  raw <- data.frame(
    A = 1:3,
    TADA.Remove = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  # Mock shinyjs::enable so it does nothing (prevents errors)
  mock_enable <- function(...) NULL
  mockery::stub(initializeTable, "shinyjs::enable", mock_enable)
  initializeTable(tadat, raw)
  # Check that tadat$reup and $ovgo are set accordingly
  testthat::expect_true(isTRUE(tadat$reup))
  testthat::expect_false(isTRUE(tadat$ovgo))
  # The raw should be assigned back to tadat$raw
  testthat::expect_true(is.data.frame(tadat$raw))
  testthat::expect_equal(nrow(tadat$raw), 3)
  # tadat$removals should be a data.frame with nrow = nrow(raw)
  testthat::expect_true(is.data.frame(tadat$removals))
  testthat::expect_equal(nrow(tadat$removals), nrow(raw))
  testthat::expect_true(isTRUE(tadat$ready_for_download))
})

testthat::test_that("initializeTable handles a fresh dataset by adding TADA.Remove and setting flags", {
  tadat <- make_tadat_env()
  raw <- data.frame(A = 1:2, stringsAsFactors = FALSE)
  mock_enable <- function(...) NULL
  mockery::stub(initializeTable, "shinyjs::enable", mock_enable)
  initializeTable(tadat, raw)
  testthat::expect_true(isTRUE(tadat$new))
  testthat::expect_true(isTRUE(tadat$ovgo))
  testthat::expect_true("TADA.Remove" %in% names(tadat$raw))
  testthat::expect_equal(nrow(tadat$removals), nrow(raw))
  testthat::expect_true(isTRUE(tadat$ready_for_download))
})

testthat::test_that("disableLoading calls shiny update functions and attempts to insert UI (all mocked)", {
  # Create mock functions for the various shiny/shinyjs/UI insertion functions used in disableLoading
  mock_updateSelectInput <- function(session, id, ...) {
    # emulate updateSelectInput signature; return a list capturing the id
    list(id = id)
  }
  mock_shinyjs_disable <- function(...) NULL
  mock_insertUI <- function(selector, where, ui) {
    list(selector = selector, where = where)
  }
  # Stub the functions within disableLoading
  mockery::stub(
    disableLoading,
    "shiny::updateSelectInput",
    mock_updateSelectInput
  )
  mockery::stub(disableLoading, "shinyjs::disable", mock_shinyjs_disable)
  mockery::stub(disableLoading, "shiny::insertUI", mock_insertUI)
  
  # Call disableLoading with a fake session object (not used by our mocks, but keep something sensible)
  fake_session <- list(user = "fake")
  testthat::expect_silent(disableLoading(fake_session))
})
