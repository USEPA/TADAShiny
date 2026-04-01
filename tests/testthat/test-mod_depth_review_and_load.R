# tests/testthat/test-mod_depth_review_and_load.R
library(testthat)
library(shiny)
library(withr)

# If running tests outside a package load context, uncomment to source the module file:
# source("R/mod_depth.R")

test_that("depth module handles sample raw", {
  tadat <- shiny::reactiveValues()
  tadat$raw <- make_depth_profile_sample_raw(n_sites = 1, n_dates = 1, depths = c(0.5, 1.5, 3.0))
  # reset global depth_profile if needed
  depth_profile$loaded <- FALSE
  depth_profile$no_data <- FALSE
  # run module
  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    # set inputs that trigger behavior
    session$setInputs(review_depth_profile_data = 1)
    session$flushReactives()
    # assertions...
  })
})

test_that("review_depth_profile_data loads when EPATADA functions return expected results", {
  tadat <- reactiveValues(raw = NULL, tab = NULL)
  # Create a minimal tadat$raw that looks like TADA data:
  tadat$raw <- data.frame(
    OrganizationIdentifier = rep("ORG1", 6),
    TADA.MonitoringLocationIdentifier = rep("SITE1", 6),
    ActivityStartDate = rep(as.character(Sys.Date()), 6),
    TADA.ComparableDataIdentifier = rep("CHAR1", 6),
    TADA.ResultMeasureValue = as.character(1:6),
    TADA.ConsolidatedDepth = as.character(c(0.5, 1, 2, 3, 4, 5)),
    stringsAsFactors = FALSE
  )

  # Prepare a "flagged" depth categorization result: simulate EPATADA::TADA_FlagDepthCategory output.
  depth_flagged <- shiny::isolate(tadat$raw)
  # Ensure TADA.ConsolidatedDepth is present and TADA.ComparableDataIdentifier present.
  depth_flagged$TADA.ConsolidatedDepth <- depth_flagged$TADA.ConsolidatedDepth

  # Create a site_date_char_groups_df emulating TADA_IDDepthProfiles output.
  site_date_char_groups_df <- data.frame(
    OrganizationIdentifier = "ORG1",
    TADA.MonitoringLocationIdentifier = "SITE1",
    ActivityStartDate = as.character(Sys.Date()),
    TADA.CharacteristicsForDepthProfile = "CHAR1 (6)",
    ResultIdentifier = 'ORG1_SITE1_2024-06-01_CHAR1',
    stringsAsFactors = FALSE
  )

  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    # stub EPATADA and shinybusy functions used in review observer
    testthat::local_mock(
      `EPATADA::TADA_FlagDepthCategory` = function(df, ...) {
        depth_flagged
      },
      `EPATADA::TADA_IDDepthProfiles` = function(df, ...) site_date_char_groups_df,
      `shinybusy::show_modal_spinner` = function(...) NULL,
      `shinybusy::remove_modal_spinner` = function(...) NULL,
      `shiny::showModal` = function(...) NULL
    )

    # simulate clicking the review button: need to trigger the observeEvent
    session$setInputs(review_depth_profile_data = 1)

    # After the observer runs, depth_profile should be loaded
    expect_true(isTRUE(depth_profile$loaded))
    expect_false(isTRUE(depth_profile$no_data))

    # site_date_pairs should be a data frame with MonitoringLocationIdentifier and ActivityStartDate
    expect_true(is.data.frame(depth_profile$site_date_pairs))
    expect_true("MonitoringLocationIdentifier" %in% names(depth_profile$site_date_pairs))
    expect_true(nrow(depth_profile$site_date_pairs) >= 1)

    # available_characteristics_df should be initialized (empty)
    expect_true(is.data.frame(depth_profile$available_characteristics_df))
  })
})