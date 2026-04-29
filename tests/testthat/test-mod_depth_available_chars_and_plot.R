# tests/testthat/test-mod_depth_available_chars_and_plot.R
library(testthat)
library(shiny)
library(EPATADA)
library(plotly)

reset_depth_profile_state <- function() {
  depth_profile$depth_categorized_df <- NULL
  depth_profile$site_date_char_groups_df <- NULL
  depth_profile$site_date_pairs <- NULL
  depth_profile$available_characteristics_df <- NULL
  depth_profile$loaded <- FALSE
  depth_profile$no_data <- FALSE
}

extract_plot_title_text <- function(plot_obj) {
  title_obj <- plot_obj$x$layout$title
  if (is.null(title_obj)) {
    return("")
  }
  if (is.list(title_obj) && !is.null(title_obj$text)) {
    return(as.character(title_obj$text))
  }
  paste(as.character(unlist(title_obj)), collapse = " ")
}

test_that("selecting site/date computes available_characteristics_df and update builds plot", {
  reset_depth_profile_state()
  on.exit(reset_depth_profile_state(), add = TRUE)

  tadat <- reactiveValues(raw = NULL, tab = NULL)

  depth_categorized_df <- data.frame(
    OrganizationIdentifier = c("ORG1", "ORG1", "ORG1"),
    TADA.MonitoringLocationIdentifier = c("SITE1", "SITE1", "SITE1"),
    MonitoringLocationIdentifier = c("SITE1", "SITE1", "SITE1"),
    OrganizationFormalName = c("Org 1", "Org 1", "Org 1"),
    ActivityStartDate = rep(as.character(Sys.Date()), 3),
    TADA.ComparableDataIdentifier = c("TEMP_NORM", "DO_NORM", "PH_NORM"),
    TADA.ResultMeasureValue = c("10", "8", "7"),
    TADA.ConsolidatedDepth = c("0.5", "1.5", "3.0"),
    ResultIdentifier = c("r1", "r2", "r3"),
    stringsAsFactors = FALSE
  )

  site_date_char_groups_df <- data.frame(
    OrganizationIdentifier = "ORG1",
    TADA.MonitoringLocationIdentifier = "SITE1",
    ActivityStartDate = as.character(Sys.Date()),
    TADA.CharacteristicsForDepthProfile =
      "TEMPERATURE, WATER_NA_NA_DEG C (1); DISSOLVED OXYGEN (DO)_NA_NA_MG/L (1)",
    stringsAsFactors = FALSE
  )

  depth_profile$loaded <- TRUE
  depth_profile$depth_categorized_df <- depth_categorized_df
  depth_profile$site_date_char_groups_df <- site_date_char_groups_df

  tadat$raw <- data.frame(
    TADA.CharacteristicName = "A",
    ResultIdentifier = "r1",
    stringsAsFactors = FALSE
  )

  old_depth_plot <- get("TADA_DepthProfilePlot", envir = asNamespace("EPATADA"))
  assignInNamespace(
    "TADA_DepthProfilePlot",
    function(df_sel, ...) {
      plotly::plot_ly(
        data = data.frame(x = 1:3, y = 1:3),
        x = ~x,
        y = ~y,
        type = "scatter",
        mode = "lines"
      )
    },
    ns = "EPATADA"
  )
  on.exit(
    assignInNamespace("TADA_DepthProfilePlot", old_depth_plot, ns = "EPATADA"),
    add = TRUE
  )

  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    session$setInputs(depth_profile_site_id = "SITE1")
    session$setInputs(activity_date = as.character(Sys.Date()))

    expect_s3_class(depth_profile$available_characteristics_df, "data.frame")
    expect_gte(nrow(depth_profile$available_characteristics_df), 1)
    expect_true(
      all(c("Characteristic", "N", "CompID") %in%
            names(depth_profile$available_characteristics_df))
    )

    session$setInputs(available_characteristics_rows_selected = c(1L))
    session$setInputs(update = 1L)

    plot_obj <- output$depthPlotly
    expect_false(is.null(plot_obj))
    expect_gte(length(plot_obj), 1)
  })
})

test_that("depthPlotly returns safe message when no numeric values present", {
  reset_depth_profile_state()
  on.exit(reset_depth_profile_state(), add = TRUE)

  tadat <- reactiveValues(raw = NULL, tab = NULL)

  depth_categorized_df <- data.frame(
    OrganizationIdentifier = c("ORG1", "ORG1"),
    TADA.MonitoringLocationIdentifier = c("SITE1", "SITE1"),
    MonitoringLocationIdentifier = c("SITE1", "SITE1"),
    OrganizationFormalName = c("Org 1", "Org 1"),
    ActivityStartDate = c(as.character(Sys.Date()), as.character(Sys.Date())),
    TADA.ComparableDataIdentifier = c("CHAR1", "CHAR1"),
    TADA.ResultMeasureValue = c("NA", "NA"),
    TADA.ConsolidatedDepth = c("a", "b"),
    ResultIdentifier = c("r1", "r2"),
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
    session$setInputs(depth_profile_site_id = "SITE1")
    session$setInputs(activity_date = as.character(Sys.Date()))
    session$setInputs(update = 1L)

    plot_obj <- output$depthPlotly
    expect_false(is.null(plot_obj))
    expect_gte(length(plot_obj), 1)
  })
})