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
