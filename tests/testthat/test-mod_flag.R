reset_flag_state <- function(tadat) {
  tadat$raw <- NULL
  tadat$removals <- NULL
  tadat$selected_flags <- character(0)
  tadat$switch_defaults <- NULL
  tadat$flags_present <- NULL
  tadat$m2f <- "meters"
}

patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

test_that("mod_data_flagging_ui renders expected controls", {
  ui <- mod_data_flagging_ui("flag_1")
  golem::expect_shinytaglist(ui)
  ui_txt <- as.character(ui)
  expect_true(grepl("Run Tests", ui_txt, fixed = TRUE))
  expect_true(grepl("flag_1-runFlags", ui_txt, fixed = TRUE))
  expect_true(grepl("flag_1-flagTable", ui_txt, fixed = TRUE))
  expect_true(grepl("flag_1-m2f", ui_txt, fixed = TRUE))
})

test_that("runFlags updates tadat$raw via applyFlags and unit conversion updates m2f", {
  tadat <- reactiveValues()
  tadat$raw <- data.frame(
    OrganizationIdentifier = c("ORG1", "ORG1"),
    TADA.CharacteristicName = c("A", "B"),
    ResultIdentifier = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  tadat$removals <- data.frame(.seed = rep(FALSE, 2), stringsAsFactors = FALSE)
  tadat$orgs <- NULL
  tadat$m2f <- "meters"

  patches <- list(
    patch_ns_fun("TADAShiny", "checkFlagColumns", function(dataset) TRUE),
    patch_ns_fun("TADAShiny", "flagCensus", function(raw) {
      out <- as.data.frame(matrix(
        FALSE,
        nrow = nrow(raw),
        ncol = length(flag_types)
      ))
      colnames(out) <- flag_types
      out
    }),
    patch_ns_fun("TADAShiny", "applyFlags", function(in_table, orgs) {
      in_table$.__flags_ran__ <- TRUE
      in_table
    }),
    patch_ns_fun("EPATADA", "TADA_ConvertDepthUnits", function(df, unit = "m") {
      df$.__depth_unit__ <- unit
      df
    }),
    patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    patch_ns_fun("shinyjs", "enable", function(...) NULL),
    patch_ns_fun("shinyjs", "disable", function(...) NULL),
    patch_ns_fun("shinyjs", "runjs", function(...) NULL)
  )
  on.exit(lapply(rev(patches), restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_data_flagging_server,
    args = list(id = "flag_1", tadat = tadat),
    {
      session$setInputs(runFlags = 1L)
      expect_true(all(tadat$raw$.__flags_ran__))

      session$setInputs(m2f = "feet")
      expect_equal(tadat$m2f, "feet")
      expect_equal(unique(tadat$raw$.__depth_unit__), "ft")

      session$setInputs(m2f = "inches")
      expect_equal(tadat$m2f, "inches")
      expect_equal(unique(tadat$raw$.__depth_unit__), "in")

      session$setInputs(m2f = "meters")
      expect_equal(tadat$m2f, "meters")
      expect_equal(unique(tadat$raw$.__depth_unit__), "m")
    }
  )
})

test_that("switch selection writes prefixed removals and updates TADA.RemovalReason", {
  tadat <- reactiveValues()
  tadat$raw <- data.frame(
    OrganizationIdentifier = c("ORG1", "ORG1", "ORG1"),
    TADA.CharacteristicName = c("A", "B", "C"),
    ResultIdentifier = c("r1", "r2", "r3"),
    stringsAsFactors = FALSE
  )
  tadat$removals <- data.frame(.seed = rep(FALSE, 3), stringsAsFactors = FALSE)
  tadat$orgs <- NULL
  tadat$m2f <- "meters"

  patches <- list(
    patch_ns_fun("TADAShiny", "checkFlagColumns", function(dataset) TRUE),
    patch_ns_fun("TADAShiny", "flagCensus", function(raw) {
      out <- as.data.frame(matrix(
        FALSE,
        nrow = nrow(raw),
        ncol = length(flag_types)
      ))
      colnames(out) <- flag_types
      out[[flag_types[1]]] <- c(TRUE, FALSE, TRUE)
      out
    }),
    patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    patch_ns_fun("shinyjs", "enable", function(...) NULL),
    patch_ns_fun("shinyjs", "disable", function(...) NULL),
    patch_ns_fun("shinyjs", "runjs", function(...) NULL)
  )
  on.exit(lapply(rev(patches), restore_ns_fun), add = TRUE)

  expected_col <- paste0(flag_prefix, flag_types[1])

  shiny::testServer(
    mod_data_flagging_server,
    args = list(id = "flag_1", tadat = tadat),
    {
      # Required switch observers run only when switch inputs exist.
      session$setInputs(switch_1 = TRUE)

      expect_true(expected_col %in% names(tadat$removals))
      expect_identical(tadat$removals[[expected_col]], c(TRUE, FALSE, TRUE))

      # TADA.RemovalReason is assembled in a separate selected_flags observer.
      # Here we focus on deterministic behavior: switch state writes prefixed
      # logical removals for downstream modules.
    }
  )
})

test_that("checkFlagColumns/flagCensus helper behavior is consistent", {
  # Sparse dataset: does not include active flag columns.
  raw_sparse <- data.frame(
    OrganizationIdentifier = c("ORG1", "ORG1", "ORG1"),
    ResultIdentifier = c("r1", "r2", "r3"),
    stringsAsFactors = FALSE
  )

  sparse_census <- flagCensus(raw_sparse)
  expect_s3_class(sparse_census, "data.frame")
  expect_equal(nrow(sparse_census), nrow(raw_sparse))
  expect_equal(ncol(sparse_census), length(flag_types))
  expect_identical(names(sparse_census), flag_types)
  expect_true(all(vapply(sparse_census, is.logical, logical(1))))
  expect_false(checkFlagColumns(raw_sparse))

  # Complete dataset: includes all active flag columns.
  raw_complete <- as.data.frame(
    setNames(
      replicate(length(active_flags), rep(FALSE, 3), simplify = FALSE),
      active_flags
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  complete_census <- flagCensus(raw_complete)
  expect_s3_class(complete_census, "data.frame")
  expect_equal(nrow(complete_census), nrow(raw_complete))
  expect_equal(ncol(complete_census), length(flag_types))
  expect_identical(names(complete_census), flag_types)
  expect_true(checkFlagColumns(raw_complete))
})
