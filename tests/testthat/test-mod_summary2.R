summary2_patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

summary2_restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

new_summary2_tadat <- function(
    raw_df,
    removals_df = NULL,
    outfile = "tada_output_ut"
) {
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
  expect_true(grepl("summary2_1-dwn_working", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-dwn_final", ui_txt, fixed = TRUE))
  expect_true(grepl("summary2_1-disclaimer", ui_txt, fixed = TRUE))
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
  
  shiny::testServer(
    mod_TADA_summary_server,
    args = list(id = "summary2_1", tadat = tadat),
    {
      session$flushReact()
      
      expect_equal(output$rec_tot, "Total Results in Dataset: 0")
      expect_equal(output$rec_rem, "Results Flagged for Removal: 0")
      expect_equal(output$rec_clean, "Results Retained: 0")
      expect_equal(output$site_tot, "Total Sites in Dataset: 0")
      expect_equal(output$site_rem, "Total Sites Flagged for Removal: 0")
      expect_equal(output$site_clean, "Total Sites Retained: 0")
    }
  )
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
  
  shiny::testServer(
    mod_TADA_summary_server,
    args = list(id = "summary2_1", tadat = tadat),
    {
      session$flushReact()
      
      expect_equal(output$rec_tot, "Total Results in Dataset: 4")
      expect_equal(output$rec_rem, "Results Flagged for Removal: 2")
      expect_equal(output$rec_clean, "Results Retained: 2")
      expect_equal(output$site_tot, "Total Sites in Dataset: 3")
      expect_equal(output$site_clean, "Total Sites Retained: 2")
      expect_equal(output$site_rem, "Total Sites Flagged for Removal: 1")
    }
  )
})

test_that("working download logic retains all rows and all columns", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    MonitoringLocationIdentifier = c("S1", "S2"),
    TADA.Remove = c(FALSE, TRUE),
    TADA.RemovalReason = c(NA, "Flag"),
    Value = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  out_data <- EPATADA::TADA_OrderCols(raw)
  
  expect_equal(nrow(out_data), 2)
  expect_true("TADA.Remove" %in% names(out_data))
  expect_true("TADA.RemovalReason" %in% names(out_data))
  expect_true("Value" %in% names(out_data))
})

test_that("final download logic removes flagged rows and drops removal columns", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2", "r3"),
    MonitoringLocationIdentifier = c("S1", "S1", "S2"),
    TADA.Remove = c(FALSE, TRUE, FALSE),
    TADA.RemovalReason = c(NA, "Flag", NA),
    TADA.ResultMeasureValue = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  out_data <- raw[raw$TADA.Remove == FALSE, ]
  out_data <- EPATADA::TADA_OrderCols(out_data)
  out_data <- dplyr::select(out_data, -dplyr::any_of(c("TADA.Remove", "TADA.RemovalReason")))
  out_data <- EPATADA::TADA_RetainRequired(out_data)
  
  expect_equal(nrow(out_data), 2)
  expect_false("TADA.Remove" %in% names(out_data))
  expect_false("TADA.RemovalReason" %in% names(out_data))
  expect_true("TADA.ResultMeasureValue" %in% names(out_data))
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
  
  shiny::testServer(
    mod_TADA_summary_server,
    args = list(id = "summary2_1", tadat = tadat),
    {
      session$setInputs(disclaimer = 1L)
      session$flushReact()
      
      expect_equal(modal_count, 1L)
    }
  )
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