# tests/testthat/test-mod_depth_review_and_load.R

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