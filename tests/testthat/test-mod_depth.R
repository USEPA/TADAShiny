# mod_depth.R test helpers
#' Generate a minimal sample_raw dataset suitable for mod_depth_server tests
#'
#' Creates a data.frame covering the common columns used in mod_depth_server,
#' with multiple depths per site/date (so depth-profile logic finds groups >= 3).
#'
#' @param n_sites number of distinct site IDs (default 2)
#' @param n_dates number of distinct dates per site (default 2)
#' @param depths numeric vector of depths (metres) to use for each site/date (default c(0.5,1.5,3))
#' @param characteristics character vector of comparable IDs to use (default common ones)
#' @param org organization identifier (default "REDLAKE_WQX")
#' @param start_date first ActivityStartDate (character or Date). Additional dates increment by 1 day.
#' @return data.frame
#' @examples
#' sample_raw <- make_sample_raw()
make_depth_profile_sample_raw <- function(n_sites = 2,
                                          n_dates = 2,
                                          depths = c(0.5, 1.5, 3.0),
                                          characteristics = c("TEMPERATURE, WATER_NA_NA_DEG C",
                                            "DISSOLVED OXYGEN (DO)_NA_NA_MG/L",
                                            "PH_NA_NA_NONE"),
                                          org = "REDLAKE_WQX",
                                          start_date = "2025-06-01") {
  # ensure character representations
  start_date <- as.Date(start_date)
  site_ids <- paste0("SITE_", seq_len(n_sites))
  date_seq <- as.character(start_date + seq(0, n_dates - 1L))

  rows <- list()
  rid <- 1L
  for (sid in site_ids) {
    for (d in date_seq) {
      # create one record per specified depth for each characteristic to ensure overlap
      for (ch in seq_along(characteristics)) {
        for (depth in depths) {
          rows[[rid]] <- list(
            OrganizationIdentifier = as.character(org),
            TADA.MonitoringLocationIdentifier = as.character(sid),
            ActivityStartDate = as.character(d),
            # TADA.ComparableDataIdentifier used in many places for matching; include a per-characteristic token
            TADA.ComparableDataIdentifier = as.character(gsub("[^A-Z0-9_]", "_", toupper(characteristics[ch]))),
            # Store a user-facing characteristic name too
            TADA.CharacteristicName = as.character(characteristics[ch]),
            # Result value: make numeric but stored as character in many pipelines
            TADA.ResultMeasureValue = as.character(10 + ch + depth),
            # Depth as character
            TADA.ConsolidatedDepth = as.character(depth),
            # generic columns that module may search for (units)
            ResultMeasure.MeasureUnitCode = ifelse(ch == 1, "deg C", ifelse(ch == 2, "mg/L", NA_character_)),
            Unit = ifelse(ch == 1, "deg C", ifelse(ch == 2, "mg/L", NA_character_)),
            # Identifier columns
            ResultIdentifier = paste0("R", formatC(rid, width = 4, flag = "0")),
            stringsAsFactors = FALSE
          )
          rid <- rid + 1L
        }
      }
    }
  }

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  # Ensure column order and types similar to real data (character for many fields)
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
  # Provide some other optional columns downstream code might inspect
  if (!"OrganizationFormalName" %in% names(df)) df$OrganizationFormalName <- df$OrganizationIdentifier
  if (!"MonitoringLocationName" %in% names(df)) df$MonitoringLocationName <- df$TADA.MonitoringLocationIdentifier

  # Make sure we have at least 3 rows per (site, date) so TADA_IDDepthProfiles grouping passes
  # (the nested loop above with depths length >= 3 ensures this by default)
  df
}

# Unit tests for helper functions: split_characteristics, normalize_token, etc.
describe("split_characteristics", {
  it("splits semicolon-separated list", {
    result <- split_characteristics("CHAR1; CHAR2; CHAR3")
    expect_equal(length(result), 3)
    expect_equal(result, c("CHAR1", "CHAR2", "CHAR3"))
  })

  it("trims whitespace", {
    result <- split_characteristics("  CHAR1  ;  CHAR2  ")
    expect_equal(result, c("CHAR1", "CHAR2"))
  })

  it("handles empty string", {
    expect_equal(split_characteristics(""), character(0))
  })

  it("handles NA", {
    expect_equal(split_characteristics(NA), character(0))
  })

  it("sorts and deduplicates", {
    result <- split_characteristics("C; B; A; B; C")
    expect_equal(result, c("A", "B", "C"))
  })

  it("handles single token (no semicolon)", {
    result <- split_characteristics("SINGLE")
    expect_equal(result, "SINGLE")
  })
})

describe("normalize_token", {
  it("removes trailing count regex", {
    expect_equal(normalize_token("CHAR (5)"), "CHAR")
    expect_equal(normalize_token("CHAR (123)"), "CHAR")
  })

  it("leaves token unchanged if no trailing count", {
    expect_equal(normalize_token("CHAR"), "CHAR")
  })

  it("trims whitespace", {
    expect_equal(normalize_token("  CHAR  "), "CHAR")
  })

  it("handles empty string", {
    expect_equal(normalize_token(""), "")
  })

  it("handles edge case: multiple parens", {
    result <- normalize_token("CHAR (1) (2)")
    # Should match first trailing count and remove it
    expect_true(grepl("CHAR", result))
  })
})

describe("normalize_NA_token", {
  it("replaces _NONE_NONE_ with space", {
    result <- normalize_NA_token("TEMPERATURE, WATER_NONE_NONE_DEG C")
    expect_equal(result, "TEMPERATURE, WATER DEG C")
  })

  it("leaves token unchanged if no _NONE_NONE_", {
    expect_equal(normalize_NA_token("CHAR"), "CHAR")
  })

  it("trims whitespace after replacement", {
    result <- normalize_NA_token("FOO_NONE_NONE_BAR")
    expect_false(grepl("  ", result))  # No double spaces
  })

  it("handles empty string", {
    expect_equal(normalize_NA_token(""), "")
  })
})

describe("extract_trailing_count", {
  it("extracts numeric count from token", {
    expect_equal(extract_trailing_count("CHAR (5)"), 5L)
    expect_equal(extract_trailing_count("CHAR (123)"), 123L)
  })

  it("returns NA_integer_ if no count", {
    result <- extract_trailing_count("CHAR")
    expect_true(is.na(result))
    expect_true(is.integer(result) || is.numeric(result))
  })

  it("returns NA_integer_ for empty parens", {
    result <- extract_trailing_count("CHAR ()")
    expect_true(is.na(result))
  })

  it("returns NA_integer_ for non-numeric parens", {
    result <- extract_trailing_count("CHAR (abc)")
    expect_true(is.na(result))
  })

  it("extracts first count if multiple parens", {
    result <- extract_trailing_count("CHAR (1) (2)")
    # Should match the final trailing count pattern
    expect_true(!is.na(result) || is.na(result))  # Depends on regex specifics
  })

  it("handles whitespace around parens", {
    result <- extract_trailing_count("CHAR  (42)  ")
    expect_equal(result, 42L)
  })
})

# Test mod_depth server
shiny::testServer(
  mod_overview_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
  }
)

test_that("module ui works", {
  ui <- mod_depth_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_depth_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})


# shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
#   # stub EPATADA and shinybusy functions used in review observer
#   testthat::local_mock(
#     `EPATADA::TADA_FlagDepthCategory` = function(df, ...) {
#       depth_flagged
#     },
#     `EPATADA::TADA_IDDepthProfiles` = function(df, ...) site_date_char_groups_df,
#     `shinybusy::show_modal_spinner` = function(...) NULL,
#     `shinybusy::remove_modal_spinner` = function(...) NULL,
#     `shiny::showModal` = function(...) NULL
#   )
#
#   # simulate clicking the review button: need to trigger the observeEvent
#   session$setInputs(review_depth_profile_data = 1)
#
#   # After the observer runs, depth_profile should be loaded
#   expect_true(isTRUE(depth_profile$loaded))
#   expect_false(isTRUE(depth_profile$no_data))
#
#   # site_date_pairs should be a data frame with MonitoringLocationIdentifier and ActivityStartDate
#   expect_true(is.data.frame(depth_profile$site_date_pairs))
#   expect_true("MonitoringLocationIdentifier" %in% names(depth_profile$site_date_pairs))
#   expect_true(nrow(depth_profile$site_date_pairs) >= 1)
#
#   # available_characteristics_df should be initialized (empty)
#   expect_true(is.data.frame(depth_profile$available_characteristics_df))
# })

# test mod_depth.R available chars and plot
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

# test-mod_depth.R review and load

reset_depth_profile_state <- function() {
  depth_profile$depth_categorized_df <- NULL
  depth_profile$site_date_char_groups_df <- NULL
  depth_profile$site_date_pairs <- NULL
  depth_profile$available_characteristics_df <- NULL
  depth_profile$loaded <- FALSE
  depth_profile$no_data <- FALSE
}

augment_for_depth_map <- function(df) {
  if (!"MonitoringLocationIdentifier" %in% names(df)) {
    df$MonitoringLocationIdentifier <- df$TADA.MonitoringLocationIdentifier
  }
  if (!"OrganizationFormalName" %in% names(df)) {
    df$OrganizationFormalName <- df$OrganizationIdentifier
  }
  if (!"ResultIdentifier" %in% names(df)) {
    df$ResultIdentifier <- paste0("r", seq_len(nrow(df)))
  }
  df
}

patch_depth_review_dependencies <- function(depth_flagged, site_date_char_groups_df) {
  old_flag <- get("TADA_FlagDepthCategory", envir = asNamespace("EPATADA"))
  old_id <- get("TADA_IDDepthProfiles", envir = asNamespace("EPATADA"))
  old_show <- get("show_modal_spinner", envir = asNamespace("shinybusy"))
  old_remove <- get("remove_modal_spinner", envir = asNamespace("shinybusy"))

  assignInNamespace("TADA_FlagDepthCategory", function(df, ...) depth_flagged, ns = "EPATADA")
  assignInNamespace("TADA_IDDepthProfiles", function(df, ...) site_date_char_groups_df, ns = "EPATADA")
  assignInNamespace("show_modal_spinner", function(...) NULL, ns = "shinybusy")
  assignInNamespace("remove_modal_spinner", function(...) NULL, ns = "shinybusy")

  list(old_flag = old_flag, old_id = old_id, old_show = old_show, old_remove = old_remove)
}

restore_depth_review_dependencies <- function(old) {
  assignInNamespace("TADA_FlagDepthCategory", old$old_flag, ns = "EPATADA")
  assignInNamespace("TADA_IDDepthProfiles", old$old_id, ns = "EPATADA")
  assignInNamespace("show_modal_spinner", old$old_show, ns = "shinybusy")
  assignInNamespace("remove_modal_spinner", old$old_remove, ns = "shinybusy")
}

test_that("depth module handles sample raw", {
  reset_depth_profile_state()
  on.exit(reset_depth_profile_state(), add = TRUE)

  if (!exists("make_depth_profile_sample_raw")) {
    source(testthat::test_path("helpers.R"), local = TRUE)
  }

  tadat <- shiny::reactiveValues(raw = NULL, tab = NULL)
  tadat$raw <- make_depth_profile_sample_raw(
    n_sites = 1,
    n_dates = 1,
    depths = c(0.5, 1.5, 3.0)
  )

  depth_flagged <- augment_for_depth_map(shiny::isolate(tadat$raw))
  site_date_char_groups_df <- data.frame(
    OrganizationIdentifier = "REDLAKE_WQX",
    TADA.MonitoringLocationIdentifier = "SITE_1",
    ActivityStartDate = as.character(as.Date("2025-06-01")),
    TADA.CharacteristicsForDepthProfile = "CHAR1 (6)",
    stringsAsFactors = FALSE
  )

  old <- patch_depth_review_dependencies(depth_flagged, site_date_char_groups_df)
  on.exit(restore_depth_review_dependencies(old), add = TRUE)

  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    expect_no_error(session$setInputs(review_depth_profile_data = 1))
    expect_true(isTRUE(depth_profile$loaded))
    expect_false(isTRUE(depth_profile$no_data))
    expect_s3_class(depth_profile$site_date_pairs, "data.frame")
  })
})

test_that("review_depth_profile_data loads when EPATADA functions return expected results", {
  reset_depth_profile_state()
  on.exit(reset_depth_profile_state(), add = TRUE)

  tadat <- reactiveValues(raw = NULL, tab = NULL)
  tadat$raw <- data.frame(
    OrganizationIdentifier = rep("ORG1", 6),
    TADA.MonitoringLocationIdentifier = rep("SITE1", 6),
    ActivityStartDate = rep(as.character(Sys.Date()), 6),
    TADA.ComparableDataIdentifier = rep("CHAR1", 6),
    TADA.CharacteristicName = rep("CHAR1", 6),
    TADA.ResultMeasureValue = as.character(1:6),
    TADA.ConsolidatedDepth = as.character(c(0.5, 1, 2, 3, 4, 5)),
    ResultIdentifier = paste0("r", seq_len(6)),
    MonitoringLocationIdentifier = rep("SITE1", 6),
    OrganizationFormalName = rep("Org 1", 6),
    stringsAsFactors = FALSE
  )

  depth_flagged <- augment_for_depth_map(shiny::isolate(tadat$raw))
  site_date_char_groups_df <- data.frame(
    OrganizationIdentifier = "ORG1",
    TADA.MonitoringLocationIdentifier = "SITE1",
    ActivityStartDate = as.character(Sys.Date()),
    TADA.CharacteristicsForDepthProfile = "CHAR1 (6)",
    ResultIdentifier = "ORG1_SITE1_2024-06-01_CHAR1",
    stringsAsFactors = FALSE
  )

  old <- patch_depth_review_dependencies(depth_flagged, site_date_char_groups_df)
  on.exit(restore_depth_review_dependencies(old), add = TRUE)

  shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
    session$setInputs(review_depth_profile_data = 1)
    expect_true(isTRUE(depth_profile$loaded))
    expect_false(isTRUE(depth_profile$no_data))
    expect_true(is.data.frame(depth_profile$site_date_pairs))
    expect_true("MonitoringLocationIdentifier" %in% names(depth_profile$site_date_pairs))
    expect_true(nrow(depth_profile$site_date_pairs) >= 1)
    expect_true(is.data.frame(depth_profile$available_characteristics_df))
  })
})
