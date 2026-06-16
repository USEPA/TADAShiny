summary2_patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

summary2_restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

new_summary2_tadat <- function(raw_df, removals_df = NULL, outfile = "tada_output_ut") {
  rv <- shiny::reactiveValues()
  rv$raw <- raw_df
  rv$removals <- if (is.null(removals_df)) {
    data.frame(matrix(nrow = nrow(raw_df), ncol = 0))
  } else {
    removals_df
  }
  rv$default_outfile <- outfile
  rv
}

test_that("mod_TADA_summary_ui renders expected controls", {
  ui <- mod_TADA_summary_ui("summary2_1")
  golem::expect_shinytaglist(ui)
  ui_txt <- as.character(ui)

  expect_true(grepl("Results Summary", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-download_working_button", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-download_final_button", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-disclaimer", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-dwn_working", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-dwn_final", ui_txt, fixed = TRUE))
})

test_that("summary text outputs show zeros when tadat$raw is NULL", {
  tadat <- shiny::reactiveValues()
  tadat$raw <- NULL
  tadat$removals <- data.frame(matrix(nrow = 0, ncol = 0))
  tadat$default_outfile <- "x"

  patches <- list(
    summary2_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "enable", function(...) NULL)
  )
  on.exit(lapply(rev(patches), summary2_restore_ns_fun), add = TRUE)

  shiny::testServer(mod_TADA_summary_server, args = list(id = "summary2_1", tadat = tadat), {
    session$flushReact()

    expect_equal(output$rec_tot, "Total Results in Dataset: 0")
    expect_equal(output$rec_rem, "Results Flagged for Removal: 0")
    expect_equal(output$rec_clean, "Results Retained: 0")
    expect_equal(output$site_tot, "Total Sites in Dataset: 0")
    expect_equal(output$site_rem, "Total Sites Flagged for Removal: 0")
    expect_equal(output$site_clean, "Total Sites Retained: 0")
  })
})

test_that("summary text outputs compute expected values with data", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2", "r3", "r4"),
    MonitoringLocationIdentifier = c("S1", "S1", "S2", "S3"),
    TADA.Remove = c(FALSE, TRUE, FALSE, TRUE),
    TADA.RemovalReason = c(NA, "Flag: a", NA, "Filter: b"),
    stringsAsFactors = FALSE
  )
  rem <- data.frame(
    `Flag: test` = c(FALSE, TRUE, FALSE, FALSE),
    `Filter: test` = c(FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  tadat <- new_summary2_tadat(raw, rem)

  patches <- list(
    summary2_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "enable", function(...) NULL)
  )
  on.exit(lapply(rev(patches), summary2_restore_ns_fun), add = TRUE)

  shiny::testServer(mod_TADA_summary_server, args = list(id = "summary2_1", tadat = tadat), {
    session$flushReact()

    expect_equal(output$rec_tot, "Total Results in Dataset: 4")
    expect_equal(output$rec_rem, "Results Flagged for Removal: 2")
    expect_equal(output$rec_clean, "Results Retained: 2")
    expect_equal(output$site_tot, "Total Sites in Dataset: 3")
    expect_equal(output$site_clean, "Total Sites Retained: 2")
    expect_equal(output$site_rem, "Total Sites Flagged for Removal: 1")
  })
})

test_that("working download button path prepares files and triggers hidden download click", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.Remove = c(FALSE, TRUE),
    TADA.RemovalReason = c(NA, "Flag"),
    stringsAsFactors = FALSE
  )
  tadat <- new_summary2_tadat(raw)

  clicked <- character(0)
  write_xlsx_path <- NULL
  saved_progress_name <- NULL

  patches <- list(
    summary2_patch_ns_fun("EPATADA", "TADA_OrderCols", function(df) df),
    summary2_patch_ns_fun("TADAShiny", "writeNarrativeDataFrame", function(tadat) {
      data.frame(Parameter = "x", Value = "y", stringsAsFactors = FALSE)
    }),
    summary2_patch_ns_fun("TADAShiny", "writeFile", function(tadat, file) {
      saved_progress_name <<- file
      invisible(NULL)
    }),
    summary2_patch_ns_fun("writexl", "write_xlsx", function(x, path, use_zip64 = TRUE) {
      write_xlsx_path <<- path
      invisible(path)
    }),
    summary2_patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    summary2_patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "click", function(id) {
      clicked <<- c(clicked, id)
      invisible(NULL)
    }),
    summary2_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "enable", function(...) NULL)
  )
  on.exit(lapply(rev(patches), summary2_restore_ns_fun), add = TRUE)

  shiny::testServer(mod_TADA_summary_server, args = list(id = "summary2_1", tadat = tadat), {
    session$setInputs(download_working_button = 1L)
    session$flushReact()

    expect_true(any(clicked == "dwn_working"))
    expect_true(grepl("_working\\.xlsx$", write_xlsx_path))
    expect_true(grepl("_prog\\.RData$", saved_progress_name))
    expect_equal(
      paste0(tadat$default_outfile, "_working.zip"),
      "tada_output_ut_working.zip"
    )
  })
})

test_that("final download button filters removed rows and drops TADA removal columns", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2", "r3"),
    MonitoringLocationIdentifier = c("S1", "S1", "S2"),
    TADA.Remove = c(FALSE, TRUE, FALSE),
    TADA.RemovalReason = c(NA, "Flag", NA),
    Value = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  tadat <- new_summary2_tadat(raw)

  captured_data_sheet <- NULL
  clicked <- character(0)

  patches <- list(
    summary2_patch_ns_fun("EPATADA", "TADA_OrderCols", function(df) df),
    summary2_patch_ns_fun("EPATADA", "TADA_RetainRequired", function(df) df),
    summary2_patch_ns_fun("TADAShiny", "writeNarrativeDataFrame", function(tadat) {
      data.frame(Parameter = "x", Value = "y", stringsAsFactors = FALSE)
    }),
    summary2_patch_ns_fun("TADAShiny", "writeFile", function(tadat, file) invisible(NULL)),
    summary2_patch_ns_fun("writexl", "write_xlsx", function(x, path, use_zip64 = TRUE) {
      captured_data_sheet <<- x$Data
      invisible(path)
    }),
    summary2_patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    summary2_patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "click", function(id) {
      clicked <<- c(clicked, id)
      invisible(NULL)
    }),
    summary2_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "enable", function(...) NULL)
  )
  on.exit(lapply(rev(patches), summary2_restore_ns_fun), add = TRUE)

  shiny::testServer(mod_TADA_summary_server, args = list(id = "summary2_1", tadat = tadat), {
    session$setInputs(download_final_button = 1L)
    session$flushReact()

    expect_true(any(clicked == "dwn_final"))
    expect_false("TADA.Remove" %in% names(captured_data_sheet))
    expect_false("TADA.RemovalReason" %in% names(captured_data_sheet))
    expect_equal(nrow(captured_data_sheet), 2)
    expect_equal(
      paste0(tadat$default_outfile, "_final.zip"),
      "tada_output_ut_final.zip"
    )
  })
})

test_that("disclaimer button shows modal", {
  raw <- data.frame(
    ResultIdentifier = "r1",
    MonitoringLocationIdentifier = "S1",
    TADA.Remove = FALSE,
    TADA.RemovalReason = NA,
    stringsAsFactors = FALSE
  )
  tadat <- new_summary2_tadat(raw)

  modal_count <- 0L
  patches <- list(
    summary2_patch_ns_fun("shiny", "showModal", function(...) {
      modal_count <<- modal_count + 1L
      invisible(NULL)
    }),
    summary2_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    summary2_patch_ns_fun("shinyjs", "enable", function(...) NULL)
  )
  on.exit(lapply(rev(patches), summary2_restore_ns_fun), add = TRUE)

  shiny::testServer(mod_TADA_summary_server, args = list(id = "summary2_1", tadat = tadat), {
    session$setInputs(disclaimer = 1L)
    session$flushReact()

    expect_equal(modal_count, 1L)
  })
})

test_that("sort_removals returns expected reason buckets", {
  rem <- data.frame(
    `Flag: a` = c(TRUE, FALSE, TRUE, FALSE, FALSE),
    `Filter: b` = c(FALSE, TRUE, TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  out <- sort_removals(rem)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("Reason", "Count") %in% names(out)))

  out_map <- stats::setNames(out$Count, out$Reason)
  expect_equal(unname(out_map["Flag only"]), 1)
  expect_equal(unname(out_map["Filter only"]), 1)
  expect_equal(unname(out_map["Flag and Filter"]), 1)
  expect_equal(unname(out_map["Retained"]), 2)
})

test_that("sort_removals returns NULL for empty object", {
  expect_null(sort_removals(data.frame()))
})

