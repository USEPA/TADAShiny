# tests/testthat/test-mod_filtering.R

library(testthat)
library(shiny)

# Helpers
new_tadat <- function(raw_df) {
  rv <- reactiveValues()
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
  rds_path <- system.file("extdata", "filter_descriptions.rds", package = "TADAShiny")
  skip_if_not(file.exists(rds_path), "filter_descriptions.rds not found")
  
  d <- tiny_data()
  tadat <- new_tadat(d)
  prefix <- "Filter (module): "
  
  testServer(mod_filtering_server, args = list(tadat = tadat), {
    values$selected_field <- "FieldA"
    session$flushReact()
    
    vals0 <- isolate(filter_values())
    expect_true("x" %in% vals0$Value_label)
    baseline_x <- vals0$Count[match("x", vals0$Value_label)]
    expect_true(is.finite(baseline_x))
    expect_gt(baseline_x, 0)
    
    i_x <- which(vals0$Value_label == "x")
    add_filters_exclude(rows = i_x)
    session$flushReact()
    
    # Ensure helper updated state
    expect_gt(nrow(isolate(tadat$selected_filters)), 0)
    
    ok <- wait_until(
      expr = function() any(grepl(paste0("^", prefix, "Exclude FieldA is "), colnames(tadat$removals))),
      session = session
    )
    expect_true(ok)
    
    colname_before <- grep(paste0("^", prefix, "Exclude FieldA is "), colnames(isolate(tadat$removals)), value = TRUE)
    expect_length(colname_before, 1)
    
    vals1 <- isolate(filter_values())
    if ("x" %in% vals1$Value_label) {
      expect_equal(vals1$Count[match("x", vals1$Value_label)], 0)
    } else {
      expect_false("x" %in% vals1$Value_label)
    }
    
    session$setInputs(removeAllFilters = 1)
    session$flushReact()
    
    ok2 <- wait_until(
      expr = function() !any(grepl(paste0("^", prefix, "Exclude FieldA is "), colnames(tadat$removals))),
      session = session
    )
    expect_true(ok2)
    
    vals2 <- isolate(filter_values())
    expect_true("x" %in% vals2$Value_label)
    expect_equal(vals2$Count[match("x", vals2$Value_label)], baseline_x)
    
    expect_equal(nrow(isolate(tadat$selected_filters)), 0)
  })
})

test_that("Exclude and Include Only update selected_filters and per-field removals correctly", {
  skip_on_cran()
  rds_path <- system.file("extdata", "filter_descriptions.rds", package = "TADAShiny")
  skip_if_not(file.exists(rds_path), "filter_descriptions.rds not found")
  
  d <- tiny_data()
  tadat <- new_tadat(d)
  prefix <- "Filter (module): "
  
  testServer(mod_filtering_server, args = list(tadat = tadat), {
    values$selected_field <- "FieldA"
    session$flushReact()
    
    vals <- isolate(filter_values())
    i_x  <- which(vals$Value_label == "x")
    i_na <- which(vals$Value_label == "NA - Not Available")
    
    add_filters_exclude(rows = i_x)
    session$flushReact()
    expect_gt(nrow(isolate(tadat$selected_filters)), 0)
    
    ok <- wait_until(
      expr = function() any(grepl(paste0("^", prefix, "Exclude FieldA is "), colnames(tadat$removals))),
      session = session
    )
    expect_true(ok)
    
    sf1 <- isolate(tadat$selected_filters)
    expect_true(nrow(sf1) >= 1)
    expect_true(all(sf1$Filter == "Exclude"))
    expect_true("x" %in% sf1$Value)
    
    colname <- grep(paste0("^", prefix, "Exclude FieldA is "), colnames(isolate(tadat$removals)), value = TRUE)
    expect_length(colname, 1)
    expect_true(any(isolate(tadat$removals[[colname]]), na.rm = TRUE))
    
    add_filters_include_only(rows = i_na)
    session$flushReact()
    expect_gt(nrow(isolate(tadat$selected_filters)), 0)
    
    ok2 <- wait_until(
      expr = function() nrow(tadat$selected_filters) > 0 &&
        all(tadat$selected_filters$Fields == "FieldA") &&
        all(tadat$selected_filters$Filter == "Exclude"),
      session = session
    )
    expect_true(ok2)
    
    sf2 <- isolate(tadat$selected_filters)
    expect_false("NA - Not Available" %in% sf2$Value)
    
    ok3 <- wait_until(
      expr = function() is.character(tadat$raw$TADA.RemovalReason) &&
        length(tadat$raw$TADA.RemovalReason) == nrow(tadat$raw),
      session = session
    )
    expect_true(ok3)
    
    reasons <- isolate(tadat$raw$TADA.RemovalReason)
    expect_true(any(is.na(reasons)))
    expect_true(any(!is.na(reasons)))
  })
})

test_that("Labelization aggregates NA-like values and pie source ignores own-field removals", {
  skip_on_cran()
  rds_path <- system.file("extdata", "filter_descriptions.rds", package = "TADAShiny")
  skip_if_not(file.exists(rds_path), "filter_descriptions.rds not found")
  
  d <- tiny_data()
  tadat <- new_tadat(d)
  
  testServer(mod_filtering_server, args = list(tadat = tadat), {
    values$selected_field <- "FieldA"
    session$flushReact()
    
    vals <- isolate(filter_values())
    expect_true("NA - Not Available" %in% vals$Value_label)
    na_count <- vals$Count[match("NA - Not Available", vals$Value_label)]
    expect_gte(na_count, 3)
    
    i_x <- which(vals$Value_label == "x")
    add_filters_exclude(rows = i_x)
    session$flushReact()
    expect_gt(nrow(isolate(tadat$selected_filters)), 0)
    
    pie_src <- isolate(pie_source())
    expect_true(any(pie_src$FieldA == "x"))
  })
})
