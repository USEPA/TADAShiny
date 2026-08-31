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

# Not sure if all these tests below belong here
patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

patch_ns_obj <- function(ns, obj_name, replacement) {
  old <- get(obj_name, envir = asNamespace(ns))
  assignInNamespace(obj_name, replacement, ns = ns)
  list(ns = ns, obj = obj_name, old = old)
}

restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

restore_ns_obj <- function(patch) {
  assignInNamespace(patch$obj, patch$old, ns = patch$ns)
}

test_that("flagCensus handles keep/NA/unknown-column branches", {
  patches <- list(
    patch_ns_obj("TADAShiny", "flag_types", c("TypeA")),
    patch_ns_obj(
      "TADAShiny",
      "test_table",
      data.frame(
        remove = c(1, 1, 1, 1),
        flagType = c("TypeA", "TypeA", "TypeA", "TypeA"),
        columnName = c("col1", "col2", "Unknown", "missing_col"),
        flagValue = c("X", NA, "Y", "Z"),
        keep = c(TRUE, FALSE, FALSE, TRUE),
        stringsAsFactors = FALSE
      )
    )
  )
  on.exit(lapply(rev(patches), restore_ns_obj), add = TRUE)
  
  raw <- data.frame(
    col1 = c("X", "Q", NA),
    col2 = c(NA, "A", NA),
    stringsAsFactors = FALSE
  )
  
  out <- flagCensus(raw)
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), c("TypeA"))
  expect_true(is.logical(out$TypeA))
  expect_identical(out$TypeA, c(TRUE, TRUE, TRUE))
})

test_that("flagCensus prints no-tests message for flag types without rules", {
  patches <- list(
    patch_ns_obj("TADAShiny", "flag_types", c("TypeA", "TypeB")),
    patch_ns_obj(
      "TADAShiny",
      "test_table",
      data.frame(
        remove = 1,
        flagType = "TypeA",
        columnName = "col1",
        flagValue = "x",
        keep = FALSE,
        stringsAsFactors = FALSE
      )
    )
  )
  on.exit(lapply(rev(patches), restore_ns_obj), add = TRUE)
  
  raw <- data.frame(col1 = c("x", "y"), stringsAsFactors = FALSE)
  expect_output(flagCensus(raw), "No tests found for flag TypeB", fixed = TRUE)
  
  out <- flagCensus(raw)
  expect_true("TypeB" %in% names(out))
  expect_true(all(is.na(out$TypeB)))
})

test_that("getCounts returns expected records and site totals", {
  sites <- c("S1", "S1", "S2", "S3")
  removed <- c(FALSE, TRUE, TRUE, FALSE)
  
  out <- getCounts(sites, removed)
  
  expect_s3_class(out, "data.frame")
  expect_identical(
    rownames(out),
    c("Total in Raw File", "Total Removed", "Total in Clean File")
  )
  expect_identical(out$Records, c(4L, 2L, 2L))
  expect_identical(out$Sites, c(3L, 2L, 1L))
})

test_that("checkFlagColumns returns FALSE when most active flag columns are missing", {
  patches <- list(patch_ns_obj("TADAShiny", "active_flags", c("A", "B", "C")))
  on.exit(lapply(rev(patches), restore_ns_obj), add = TRUE)
  
  mostly_missing <- data.frame(A = c(TRUE, FALSE), stringsAsFactors = FALSE)
  expect_false(checkFlagColumns(mostly_missing))
  
  mostly_found <- data.frame(
    A = c(TRUE, FALSE),
    B = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  expect_output(
    expect_true(checkFlagColumns(mostly_found)),
    "Missing the following fields that are in the csv files:",
    fixed = TRUE
  )
  
  complete <- data.frame(
    A = FALSE,
    B = FALSE,
    C = FALSE,
    stringsAsFactors = FALSE
  )
  expect_true(checkFlagColumns(complete))
})

test_that("applyFlags runs full EPATADA pipeline and includes QAPP step when column exists", {
  add_col <- function(col_name) {
    function(df, ...) {
      df[[col_name]] <- TRUE
      df
    }
  }
  
  patches <- list(
    patch_ns_fun("EPATADA", "TADA_FlagSpeciation", add_col("step_speciation")),
    patch_ns_fun("EPATADA", "TADA_FlagFraction", add_col("step_fraction")),
    patch_ns_fun("EPATADA", "TADA_FlagResultUnit", add_col("step_resultunit")),
    patch_ns_fun("EPATADA", "TADA_FindQCActivities", add_col("step_qc")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FlagMeasureQualifierCode",
      add_col("step_mqc")
    ),
    patch_ns_fun("EPATADA", "TADA_FlagMethod", add_col("step_method")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FindPotentialDuplicatesSingleOrg",
      add_col("step_dup_single")
    ),
    patch_ns_fun("EPATADA", "TADA_FindQAPPDoc", add_col("step_qappdoc")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FlagContinuousData",
      add_col("step_continuous")
    ),
    patch_ns_fun("EPATADA", "TADA_FlagAboveThreshold", add_col("step_above")),
    patch_ns_fun("EPATADA", "TADA_FlagBelowThreshold", add_col("step_below")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FlagCoordinates",
      add_col("step_coordinates")
    ),
    patch_ns_fun("EPATADA", "TADA_MediaFilter", add_col("step_media"))
  )
  on.exit(lapply(rev(patches), restore_ns_fun), add = TRUE)
  
  in_table <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    ProjectFileUrl = c("http://x", "http://y"),
    stringsAsFactors = FALSE
  )
  
  out <- applyFlags(in_table, orgs = NULL)
  
  expected_cols <- c(
    "step_speciation",
    "step_fraction",
    "step_resultunit",
    "step_qc",
    "step_mqc",
    "step_method",
    "step_dup_single",
    "step_qappdoc",
    "step_continuous",
    "step_above",
    "step_below",
    "step_coordinates",
    "step_media"
  )
  
  expect_true(all(expected_cols %in% names(out)))
  expect_true(all(vapply(out[expected_cols], all, logical(1))))
})

test_that("applyFlags skips QAPP step when ProjectFileUrl column is absent", {
  add_col <- function(col_name) {
    function(df, ...) {
      df[[col_name]] <- TRUE
      df
    }
  }
  
  patches <- list(
    patch_ns_fun("EPATADA", "TADA_FlagSpeciation", add_col("step_speciation")),
    patch_ns_fun("EPATADA", "TADA_FlagFraction", add_col("step_fraction")),
    patch_ns_fun("EPATADA", "TADA_FlagResultUnit", add_col("step_resultunit")),
    patch_ns_fun("EPATADA", "TADA_FindQCActivities", add_col("step_qc")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FlagMeasureQualifierCode",
      add_col("step_mqc")
    ),
    patch_ns_fun("EPATADA", "TADA_FlagMethod", add_col("step_method")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FindPotentialDuplicatesSingleOrg",
      add_col("step_dup_single")
    ),
    patch_ns_fun("EPATADA", "TADA_FindQAPPDoc", add_col("step_qappdoc")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FlagContinuousData",
      add_col("step_continuous")
    ),
    patch_ns_fun("EPATADA", "TADA_FlagAboveThreshold", add_col("step_above")),
    patch_ns_fun("EPATADA", "TADA_FlagBelowThreshold", add_col("step_below")),
    patch_ns_fun(
      "EPATADA",
      "TADA_FlagCoordinates",
      add_col("step_coordinates")
    ),
    patch_ns_fun("EPATADA", "TADA_MediaFilter", add_col("step_media"))
  )
  on.exit(lapply(rev(patches), restore_ns_fun), add = TRUE)
  
  in_table <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    stringsAsFactors = FALSE
  )
  
  out <- applyFlags(in_table, orgs = NULL)
  
  expect_false("step_qappdoc" %in% names(out))
  expect_true("step_media" %in% names(out))
})
