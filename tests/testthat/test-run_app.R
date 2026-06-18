test_that("run_app returns a shiny.appobj and forwards options and onStart when available", {
  # Save/restore shiny.maxRequestSize
  old_opt <- getOption("shiny.maxRequestSize", NULL)
  on.exit(options(shiny.maxRequestSize = old_opt), add = TRUE)

  # Stub get_golem_config only; let shiny::shinyApp build a real app
  testthat::with_mocked_bindings(
    get_golem_config = function(...) 500,
    .env = asNamespace("TADAShiny"),
    {
      on_start_fn <- function() invisible(NULL)

      app <- run_app(
        onStart = on_start_fn,
        options = list(launch.browser = FALSE),
        enableBookmarking = "url",
        uiPattern = "/custom"
      )

      # Returned object is a shiny app
      expect_s3_class(app, "shiny.appobj")

      # server and ui may be present depending on Shiny version
      if (!is.null(app$server)) {
        expect_true(is.function(app$server))
      } else {
        testthat::skip("app$server not exposed by this Shiny version")
      }

      # options list should include forwarded options (if exposed)
      if (!is.null(app$options)) {
        expect_identical(app$options$launch.browser, FALSE)
      } else {
        testthat::skip("app$options not exposed by this Shiny version")
      }

      # onStart might be stored on the app object (if exposed)
      if (!is.null(app$onStart)) {
        expect_identical(app$onStart, on_start_fn)
      } else {
        testthat::skip("app$onStart not exposed by this Shiny version")
      }
    }
  )
})

test_that("run_app sets shiny.maxRequestSize for valid numeric MB_LIMIT", {
  old_opt <- getOption("shiny.maxRequestSize", NULL)
  on.exit(options(shiny.maxRequestSize = old_opt), add = TRUE)

  # Start from a known value
  options(shiny.maxRequestSize = 1L)

  testthat::with_mocked_bindings(
    get_golem_config = function(...) 123,
    .env = asNamespace("TADAShiny"),
    {
      run_app()
      expect_equal(getOption("shiny.maxRequestSize"), 123 * 1024^2)
    }
  )
})

test_that("run_app parses numeric strings for MB_LIMIT", {
  old_opt <- getOption("shiny.maxRequestSize", NULL)
  on.exit(options(shiny.maxRequestSize = old_opt), add = TRUE)

  options(shiny.maxRequestSize = 1L)

  testthat::with_mocked_bindings(
    get_golem_config = function(...) "250",
    .env = asNamespace("TADAShiny"),
    {
      run_app()
      expect_equal(getOption("shiny.maxRequestSize"), 250 * 1024^2)
    }
  )
})

test_that("run_app uses default-like 500 MB when config reports 500", {
  old_opt <- getOption("shiny.maxRequestSize", NULL)
  on.exit(options(shiny.maxRequestSize = old_opt), add = TRUE)

  options(shiny.maxRequestSize = 1L)

  testthat::with_mocked_bindings(
    get_golem_config = function(...) 500,
    .env = asNamespace("TADAShiny"),
    {
      run_app()
      expect_equal(getOption("shiny.maxRequestSize"), 500 * 1024^2)
    }
  )
})

test_that("run_app does not change shiny.maxRequestSize for invalid or non-positive MB_LIMIT", {
  old_opt <- getOption("shiny.maxRequestSize", NULL)
  on.exit(options(shiny.maxRequestSize = old_opt), add = TRUE)

  # Invalid numeric (e.g., '500MB') -> no change
  options(shiny.maxRequestSize = 123L)
  testthat::with_mocked_bindings(
    get_golem_config = function(...) "500MB",
    .env = asNamespace("TADAShiny"),
    {
      run_app()
      expect_identical(getOption("shiny.maxRequestSize"), 123L)
    }
  )

  # Negative value -> no change
  options(shiny.maxRequestSize = 234L)
  testthat::with_mocked_bindings(
    get_golem_config = function(...) -10,
    .env = asNamespace("TADAShiny"),
    {
      run_app()
      expect_identical(getOption("shiny.maxRequestSize"), 234L)
    }
  )

  # Zero -> no change
  options(shiny.maxRequestSize = 345L)
  testthat::with_mocked_bindings(
    get_golem_config = function(...) 0,
    .env = asNamespace("TADAShiny"),
    {
      run_app()
      expect_identical(getOption("shiny.maxRequestSize"), 345L)
    }
  )
})

test_that("run_app accepts enableBookmarking and uiPattern without error", {
  testthat::with_mocked_bindings(
    get_golem_config = function(...) 500,
    .env = asNamespace("TADAShiny"),
    {
      expect_no_error(run_app(enableBookmarking = "url", uiPattern = "/custom"))
    }
  )
})
