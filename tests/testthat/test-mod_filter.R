# Helpers
new_tadat <- function(raw_df) {
  rv <- shiny::reactiveValues()
  rv$raw <- raw_df
  rv$removals <- data.frame(matrix(nrow = nrow(raw_df), ncol = 0))
  rv$selected_filters <- data.frame(
    Fields = character(), Value = character(), Filter = character(),
    Count = integer(), stringsAsFactors = FALSE
  )
  rv
}

tiny_data <- function() {
  data.frame(
    FieldA = c("x", "y", "x", "", NA, "NA", "z"),
    FieldB = c(1, 2, 1, 3, 3, 2, NA),
    TADA.Remove = c(FALSE, FALSE, TRUE, FALSE, NA, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
}

wait_until <- function(expr, session, timeout_ms = 6000, step_ms = 25) {
  start <- as.numeric(Sys.time()) * 1000
  repeat {
    if (isTRUE(expr())) return(TRUE)
    if ((as.numeric(Sys.time()) * 1000 - start) > timeout_ms) return(FALSE)
    session$flushReact()
    Sys.sleep(step_ms / 1000)
  }
}

test_that("'Remove All Filters' clears per-field removals and restores Step 2 values", {
  skip_on_cran()

  d <- tiny_data()
  tadat <- new_tadat(d)
  prefix <- "Filter (module): "

  shiny::testServer(mod_filtering_server, args = list(tadat = tadat), {
    values$selected_field <- "FieldA"
    session$flushReact()

    vals0 <- shiny::isolate(filter_values())
    expect_true("x" %in% vals0$Value_label)
    baseline_x <- vals0$Count[match("x", vals0$Value_label)]
    expect_true(is.finite(baseline_x))
    expect_gt(baseline_x, 0)

    i_x <- which(vals0$Value_label == "x")
    add_filters_exclude(rows = i_x)
    session$flushReact()

    expect_gt(nrow(shiny::isolate(tadat$selected_filters)), 0)

    ok <- wait_until(
      expr = function() any(startsWith(colnames(tadat$removals), paste0(prefix, "Exclude FieldA"))),
      session = session
    )
    expect_true(ok)

    cn <- colnames(shiny::isolate(tadat$removals))
    colname_before <- cn[startsWith(cn, paste0(prefix, "Exclude FieldA"))]
    expect_gte(length(colname_before), 1)

    vals1 <- shiny::isolate(filter_values())
    if ("x" %in% vals1$Value_label) {
      expect_equal(vals1$Count[match("x", vals1$Value_label)], 0)
    } else {
      expect_false("x" %in% vals1$Value_label)
    }

    session$setInputs(removeAllFilters = 1)
    session$flushReact()

    ok2 <- wait_until(
      expr = function() !any(startsWith(colnames(tadat$removals), paste0(prefix, "Exclude FieldA"))),
      session = session
    )
    expect_true(ok2)

    vals2 <- shiny::isolate(filter_values())
    expect_true("x" %in% vals2$Value_label)
    expect_equal(vals2$Count[match("x", vals2$Value_label)], baseline_x)

    expect_equal(nrow(shiny::isolate(tadat$selected_filters)), 0)

    reasons <- shiny::isolate(tadat$raw$TADA.RemovalReason)
    expect_equal(length(reasons), nrow(tadat$raw))
    expect_true(all(is.na(reasons)))
  })
})

test_that("Exclude and Include Only update selected_filters and per-field removals correctly", {
  skip_on_cran()

  d <- tiny_data()
  tadat <- new_tadat(d)
  prefix <- "Filter (module): "

  shiny::testServer(mod_filtering_server, args = list(tadat = tadat), {
    values$selected_field <- "FieldA"
    session$flushReact()

    vals <- shiny::isolate(filter_values())
    i_x  <- which(vals$Value_label == "x")
    i_na <- which(vals$Value_label == "NA - Not Available")

    add_filters_exclude(rows = i_x)
    session$flushReact()
    expect_gt(nrow(shiny::isolate(tadat$selected_filters)), 0)

    ok <- wait_until(
      expr = function() any(startsWith(colnames(tadat$removals), paste0(prefix, "Exclude FieldA"))),
      session = session
    )
    expect_true(ok)

    sf1 <- shiny::isolate(tadat$selected_filters)
    expect_true(nrow(sf1) >= 1)
    expect_true(all(sf1$Filter == "Exclude"))
    expect_true("x" %in% sf1$Value)

    cn <- colnames(shiny::isolate(tadat$removals))
    colname <- cn[startsWith(cn, paste0(prefix, "Exclude FieldA"))]
    expect_gte(length(colname), 1)
    expect_true(any(shiny::isolate(tadat$removals[[colname[1]]]), na.rm = TRUE))

    add_filters_include_only(rows = i_na)
    session$flushReact()
    expect_gt(nrow(shiny::isolate(tadat$selected_filters)), 0)

    ok2 <- wait_until(
      expr = function() nrow(tadat$selected_filters) > 0 &&
        all(tadat$selected_filters$Fields == "FieldA") &&
        all(tadat$selected_filters$Filter == "Exclude"),
      session = session
    )
    expect_true(ok2)

    sf2 <- shiny::isolate(tadat$selected_filters)
    expect_false("NA - Not Available" %in% sf2$Value)

    ok3 <- wait_until(
      expr = function() is.character(tadat$raw$TADA.RemovalReason) &&
        length(tadat$raw$TADA.RemovalReason) == nrow(tadat$raw),
      session = session
    )
    expect_true(ok3)

    reasons <- shiny::isolate(tadat$raw$TADA.RemovalReason)
    expect_true(any(is.na(reasons)))
    expect_true(any(!is.na(reasons)))
  })
})

test_that("Labelization aggregates NA-like values and pie source reflects applied removals", {
  skip_on_cran()

  d <- tiny_data()
  tadat <- new_tadat(d)
  prefix <- "Filter (module): "

  shiny::testServer(mod_filtering_server, args = list(tadat = tadat), {
    values$selected_field <- "FieldA"
    session$flushReact()

    # Baseline: labelization aggregates NA-like values
    vals <- shiny::isolate(filter_values())
    expect_true("NA - Not Available" %in% vals$Value_label)
    na_count <- vals$Count[match("NA - Not Available", vals$Value_label)]
    expect_gte(na_count, 3)

    # Baseline count for "x" from active data (honors TADA.Remove)
    baseline_x <- vals$Count[match("x", vals$Value_label)]
    expect_true(is.finite(baseline_x))
    expect_gt(baseline_x, 0)

    # Exclude "x" in Step 2
    i_x <- which(vals$Value_label == "x")
    add_filters_exclude(rows = i_x)
    session$flushReact()
    expect_gt(nrow(shiny::isolate(tadat$selected_filters)), 0)

    # Ensure removals applied (observer ran) before reading pie source
    ok <- wait_until(
      expr = function() any(startsWith(colnames(tadat$removals), paste0(prefix, "Exclude FieldA"))),
      session = session
    )
    expect_true(ok)

    # Expected pie count for "x" based on current removals (including own-field)
    keep_all <- keep_mask_for(NULL)
    expected_x_after <- sum(labelize(tadat$raw$FieldA)[keep_all] == "x", na.rm = TRUE)

    # Pie source reflects applied removals
    pie_src <- shiny::isolate(pie_source())
    sum_x <- sum(pie_src$FieldA == "x", na.rm = TRUE)
    expect_equal(as.integer(sum_x), as.integer(expected_x_after))
  })
})

test_that("mod_filtering_server: field list observer is robust to empty/missing FieldCounts", {

  # Helper to extract the underlying data.frame from a DT htmlwidget
  extract_dt_data <- function(widget) {
    if (is.list(widget) && !is.null(widget$x) && is.list(widget$x) && !is.null(widget$x$data)) {
      widget$x$data
    } else {
      NULL
    }
  }
  
  # Case 1: Empty dataset (nrow = 0) -> should not call FieldCounts and should not error
  tadat1 <- shiny::reactiveValues(
    raw = data.frame(),            # 0-row df
    removals = NULL,
    selected_filters = NULL,
    field_sel = NULL
  )
  
  expect_silent(
    shiny::testServer(mod_filtering_server, args = list(tadat = tadat1), {
      session$setInputs(field_sel = "key")
      session$flushReact()
      
      # The DT should render without error
      expect_false(is.null(output$filterStep1))
      
      # If the widget exposes data, it should have Fields + Description columns
      dt_data <- extract_dt_data(output$filterStep1)
      if (!is.null(dt_data)) {
        expect_true(all(c("Fields", "Description") %in% names(dt_data)))
        # empty dataset => likely 0 rows
        expect_equal(nrow(dt_data), 0)
      }
    })
  )
  
  # Case 2: Non-empty dataset but EPATADA::TADA_FieldCounts likely unavailable
  # The tryCatch should yield an empty 'Fields' data frame with a Description column.
  tadat2 <- shiny::reactiveValues(
    raw = data.frame(A = c(1, 2)), # non-empty
    removals = NULL,
    selected_filters = NULL,
    field_sel = NULL
  )
  
  expect_silent(
    shiny::testServer(mod_filtering_server, args = list(tadat = tadat2), {
      session$setInputs(field_sel = "most")
      session$flushReact()
      
      expect_false(is.null(output$filterStep1))
      dt_data <- extract_dt_data(output$filterStep1)
      if (!is.null(dt_data)) {
        expect_true(all(c("Fields", "Description") %in% names(dt_data)))
        # Without EPATADA available, tryCatch path returns 0-row data
        expect_equal(nrow(dt_data), 0)
      }
    })
  )
})
