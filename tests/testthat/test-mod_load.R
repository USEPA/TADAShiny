# Basic wiring: run a minimal server check inside a test block
testthat::test_that("mod_query_data_server basic NS wiring", {
  shiny::testServer(mod_query_data_server, args = list(), {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))
    # Examples for future tests:
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
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

testthat::test_that(".tadas_offline honors TADAS_OFFLINE env var", {
  withr::local_envvar(TADAS_OFFLINE = "true")
  expect_true(.tadas_offline())

  withr::local_envvar(TADAS_OFFLINE = "")
  expect_false(.tadas_offline())
})

testthat::test_that(".safe_req_string returns NULL when offline", {
  withr::local_envvar(TADAS_OFFLINE = "true")
  expect_null(.safe_req_string("https://example.com"))
})

testthat::test_that(".safe_fetch_csv_column returns default when offline", {
  testthat::skip_if_not_installed("data.table")
  withr::local_envvar(TADAS_OFFLINE = "true")
  out <- .safe_fetch_csv_column(
    "http://does-not-matter",
    "Name",
    default = c("X")
  )
  expect_identical(out, c("X"))
})

testthat::test_that(".safe_fetch_csv_column returns unique values when column present", {
  testthat::skip_if_not_installed("data.table")
  # Mock the internal string fetcher to avoid network
  testthat::local_mocked_bindings(
    .safe_req_string = function(u, ...) {
      # headered CSV; fread can read from a single string containing data
      "Name,Other\nA,1\nB,2\nA,3\n"
    },
    .env = environment(.safe_fetch_csv_column)
  )
  out <- .safe_fetch_csv_column("http://dummy", "Name", default = character())
  expect_identical(sort(out), c("A", "B"))
})

testthat::test_that(".safe_fetch_csv_column returns default if column missing", {
  testthat::skip_if_not_installed("data.table")
  testthat::local_mocked_bindings(
    .safe_req_string = function(u, ...) {
      "ID,Other\n1,2\n3,4\n"
    },
    .env = environment(.safe_fetch_csv_column)
  )
  out <- .safe_fetch_csv_column("http://dummy", "Name", default = "fallback")
  expect_identical(out, "fallback")
})

testthat::test_that(".safe_fetch_projects returns unique ProjectIdentifier list or empty on offline", {
  testthat::skip_if_not_installed("data.table")

  # Offline path -> empty character()
  withr::local_envvar(TADAS_OFFLINE = "true")
  expect_identical(.safe_fetch_projects("http://dummy"), character())

  # Happy path with mock
  withr::local_envvar(TADAS_OFFLINE = "")
  testthat::local_mocked_bindings(
    .safe_req_string = function(u, ...) {
      "ProjectIdentifier,Other\nP1,x\nP2,x\nP1,y\n"
    },
    .env = environment(.safe_fetch_projects)
  )
  out <- .safe_fetch_projects("http://dummy")
  expect_identical(sort(out), c("P1", "P2"))
})

testthat::test_that(".safe_fetch_county returns empty df with expected cols when offline", {
  withr::local_envvar(TADAS_OFFLINE = "true")
  df <- .safe_fetch_county("http://dummy")
  expected_cols <- c(
    "STATE_CD",
    "STATE_FIPS",
    "COUNTY_FIPS",
    "COUNTY_NAME",
    "COUNTY_FOOBAR"
  )
  expect_true(is.data.frame(df))
  expect_identical(names(df), expected_cols)
  expect_identical(nrow(df), 0L)
})

testthat::test_that(".safe_fetch_county parses headerless census rows", {
  testthat::skip_if_not_installed("data.table")
  withr::local_envvar(TADAS_OFFLINE = "")

  # Provide two rows; fread(header = FALSE, col.names = cols) is used inside
  text_rows <- paste(
    "AL,01,001,Autauga,foo",
    "AL,01,003,Baldwin,bar",
    sep = "\n"
  )
  testthat::local_mocked_bindings(
    .safe_req_string = function(u, ...) text_rows,
    .env = environment(.safe_fetch_county)
  )
  df <- .safe_fetch_county("http://dummy")
  expect_true(is.data.frame(df))
  expect_identical(nrow(df), 2L)
  expect_identical(
    names(df),
    c("STATE_CD", "STATE_FIPS", "COUNTY_FIPS", "COUNTY_NAME", "COUNTY_FOOBAR")
  )
  expect_identical(df$STATE_CD, c("AL", "AL"))
  expect_identical(df$COUNTY_NAME, c("Autauga", "Baldwin"))
})

testthat::test_that(".format_wqp_query_error_message gives bbox guidance for timeout errors", {
  msg <- .format_wqp_query_error_message("504 Gateway Timeout")

  expect_match(msg, "timed out", ignore.case = TRUE)
  expect_match(msg, "bounding box", ignore.case = TRUE)
})

testthat::test_that(".format_wqp_query_error_message preserves non-timeout details", {
  msg <- .format_wqp_query_error_message("certificate verification failed")

  expect_match(msg, "An error occurred while querying WQX \\(EPA\\):")
  expect_match(msg, "certificate verification failed")
})

testthat::test_that(".format_wqp_query_error_message handles empty message safely", {
  expect_identical(
    .format_wqp_query_error_message(""),
    "An error occurred while querying WQX (EPA). Please try again."
  )
  expect_identical(
    .format_wqp_query_error_message(NULL),
    "An error occurred while querying WQX (EPA). Please try again."
  )
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
