# tests/testthat/test-mod_depth_available_chars_and_plot.R
library(testthat)
library(shiny)
library(withr)
library(plotly)

# source("R/mod_depth.R") # uncomment if not testing in package context

test_that("selecting site/date computes available_characteristics_df and pre-selects defaults; update builds plot", {
  tadat <- reactiveValues(raw = NULL, tab = NULL)

  # Build sample depth_categorized_df with one site/date and a complex TADA.CharacteristicsForDepthProfile string
  depth_categorized_df <- data.frame(
    OrganizationIdentifier = c("ORG1","ORG1","ORG1"),
    TADA.MonitoringLocationIdentifier = c("SITE1","SITE1","SITE1"),
    ActivityStartDate = rep(as.character(Sys.Date()), 3),
    TADA.ComparableDataIdentifier = c("TEMP_NORM","DO_NORM","PH_NORM"),
    TADA.ResultMeasureValue = c("10", "8", "7"),
    TADA.ConsolidatedDepth = c("0.5", "1.5", "3.0"),
    stringsAsFactors = FALSE
  )

  # Simulate site_date_char_groups_df: contains TADA.CharacteristicsForDepthProfile strings
  site_date_char_groups_df <- data.frame(
    OrganizationIdentifier = "ORG1",
    TADA.MonitoringLocationIdentifier = "SITE1",
    ActivityStartDate = as.character(Sys.Date()),
    TADA.CharacteristicsForDepthProfile = c("TEMPERATURE, WATER_NA_NA_DEG C (1); DISSOLVED OXYGEN (DO)_NA_NA_MG/L (1)"),
    stringsAsFactors = FALSE
  )

  # Put these into depth_profile global (module relies on it)
  local({
    # ensure clean state
    depth_profile$loaded <- TRUE
    depth_profile$depth_categorized_df <- depth_categorized_df
    depth_profile$site_date_char_groups_df <- site_date_char_groups_df

    tadat$raw <- data.frame(TADA.CharacteristicName = c("A"), ResultIdentifier = c("r1"), stringsAsFactors = FALSE)
  })

  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    # stub DT::selectRows so it doesn't rely on JS/DT
    testthat::local_mock(
      `DT::selectRows` = function(proxy, rows) invisible(rows),
      `EPATADA::TADA_DepthProfilePlot` = function(df_sel, ...) {
        # return a simple plotly object to emulate success
        plotly::plot_ly(data = data.frame(x = 1:3, y = 1:3), x = ~x, y = ~y, type = "scatter", mode = "lines")
      },
      `shiny::showModal` = function(...) NULL
    )

    # simulate user choosing site and date (these update triggers will run observers)
    session$setInputs(depth_profile_site_id = "SITE1")
    session$setInputs(activity_date = as.character(Sys.Date()))

    # after selecting activity_date, available_characteristics_df should be populated
    # Give a small pause to allow observers to run (testServer runs synchronously; set inputs triggers observers immediately)
    expect_true(is.data.frame(depth_profile$available_characteristics_df))
    expect_true(nrow(depth_profile$available_characteristics_df) >= 1)
    expect_true(all(c("Characteristic", "N", "CompID") %in% names(depth_profile$available_characteristics_df)))

    # Simulate selecting the available characteristics rows (e.g., first one)
    session$setInputs(available_characteristics_rows_selected = c(1L))

    # Trigger the update action to build the plot (eventReactive tied to input$update)
    session$setInputs(update = 1L)

    # Now call the reactive that renders the plotly (via output$depthPlotly)
    p <- session$getReturned() # not applicable; instead request depth_plot_obj through the module internal reactive?

    # The easiest check: call the eventReactive by setting inputs and then read output$depthPlotly via render
    # We can obtain the plot by evaluating the output expression. testServer exposes output list
    expect_true(!is.null(output$depthPlotly))
    # renderPlotly returns a plotly object when called
    plot_obj <- output$depthPlotly()
    # plot_obj should be plotly htmlwidget
    expect_true(inherits(plot_obj, "plotly") || inherits(plot_obj, "htmlwidget"))
  })
})

test_that("depthPlotly returns safe message when no numeric values present", {
  tadat <- reactiveValues(raw = NULL, tab = NULL)

  # depth dataset has only non-numeric measure/depth
  depth_categorized_df <- data.frame(
    OrganizationIdentifier = "ORG1",
    TADA.MonitoringLocationIdentifier = "SITE1",
    ActivityStartDate = as.character(Sys.Date()),
    TADA.ComparableDataIdentifier = "CHAR1",
    TADA.ResultMeasureValue = c("NA", "NA"),
    TADA.ConsolidatedDepth = c("a", "b"),
    stringsAsFactors = FALSE
  )

  depth_profile$loaded <- TRUE
  depth_profile$depth_categorized_df <- depth_categorized_df
  depth_profile$site_date_char_groups_df <- data.frame(
    OrganizationIdentifier = "ORG1",
    TADA.MonitoringLocationIdentifier = "SITE1",
    ActivityStartDate = as.character(Sys.Date()),
    TADA.CharacteristicsForDepthProfile = "CHAR1 (2)",
    stringsAsFactors = FALSE
  )

  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    testthat::local_mock(
      `shiny::showModal` = function(...) NULL
    )
    session$setInputs(depth_profile_site_id = "SITE1")
    session$setInputs(activity_date = as.character(Sys.Date()))
    session$setInputs(update = 1L)

    # output$depthPlotly() should exist and be a plotly message (safe_message_plot returns plotly)
    plot_obj <- output$depthPlotly()
    expect_true(inherits(plot_obj, "plotly") || inherits(plot_obj, "htmlwidget"))
    # The safe message uses title text equal to the message; check it's a plotly object with layout$title
    layout_title <- plot_obj$x$layout$title
    expect_true(!is.null(layout_title))
    expect_true(grepl("no numeric", tolower(as.character(layout_title)) ) || grepl("no records", tolower(as.character(layout_title))) || grepl("no characteristic", tolower(as.character(layout_title))), info = "expected safe message in plot title")
  })
})