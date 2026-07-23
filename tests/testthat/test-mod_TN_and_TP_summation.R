sum_patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

sum_restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

new_sum_tadat <- function(raw_df) {
  rv <- shiny::reactiveValues()
  rv$raw <- raw_df
  rv$removals <- data.frame(
    seed = rep(FALSE, nrow(raw_df)),
    stringsAsFactors = FALSE
  )
  rv
}

test_that("mod_TN_and_TP_summation_ui renders expected controls", {
  ui <- mod_TN_and_TP_summation_ui("sum_1")
  golem::expect_shinytaglist(ui)
  ui_txt <- as.character(ui)

  expect_true(grepl(
    "Total Nitrogen and Phosphorus Summation",
    ui_txt,
    fixed = TRUE
  ))
  expect_true(grepl("sum_1-sum_dwn", ui_txt, fixed = TRUE))
  expect_true(grepl("sum_1-sum_apply", ui_txt, fixed = TRUE))
})

test_that("sum_apply UI appears only when harmonized flag exists", {
  d1 <- data.frame(
    ResultIdentifier = "r1",
    TADA.Remove = FALSE,
    stringsAsFactors = FALSE
  )
  d2 <- data.frame(
    ResultIdentifier = "r1",
    TADA.Remove = FALSE,
    TADA.Harmonized.Flag = TRUE,
    stringsAsFactors = FALSE
  )

  tadat1 <- new_sum_tadat(d1)
  shiny::testServer(
    mod_TN_and_TP_summation_server,
    args = list(id = "sum_a", tadat = tadat1),
    {
      session$flushReact()
      expect_null(output$sum_apply)
    }
  )

  tadat2 <- new_sum_tadat(d2)
  shiny::testServer(
    mod_TN_and_TP_summation_server,
    args = list(id = "sum_b", tadat = tadat2),
    {
      session$flushReact()
      expect_false(is.null(output$sum_apply))
    }
  )
})

test_that("sum_apply computes totals and extends removals for new TADA rows", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    TADA.Remove = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  tadat <- new_sum_tadat(raw)
  tadat$removals <- data.frame(
    flag_a = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  patches <- list(
    sum_patch_ns_fun(
      "EPATADA",
      "TADA_CalculateTotalNP",
      function(dat, daily_agg = "max") {
        new_row <- dat[1, , drop = FALSE]
        new_row$ResultIdentifier <- "TADA-NEW-1"
        new_row$TADA.NutrientSummation.Flag <- "New row added: Nutrient summation from one or more subspecies."
        dat$TADA.NutrientSummation.Flag <- NA_character_
        plyr::rbind.fill(dat, new_row)
      }
    ),
    sum_patch_ns_fun("EPATADA", "TADA_OrderCols", function(df) df),
    sum_patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    sum_patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    sum_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    sum_patch_ns_fun("shiny", "showModal", function(...) invisible(NULL))
  )
  on.exit(lapply(rev(patches), sum_restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_TN_and_TP_summation_server,
    args = list(id = "sum_1", tadat = tadat),
    {
      before_n_raw <- nrow(tadat$raw)
      before_n_rem <- nrow(tadat$removals)

      session$setInputs(sum_apply = 1L)
      session$flushReact()

      expect_gt(nrow(tadat$raw), before_n_raw)
      expect_gt(nrow(tadat$removals), before_n_rem)
      expect_true(any(grepl("TADA-", tadat$raw$ResultIdentifier)))
      expect_equal(ncol(tadat$removals), 1)
    }
  )
})

test_that("sum_dwn output is available", {
  raw <- data.frame(
    ResultIdentifier = "r1",
    TADA.Remove = FALSE,
    stringsAsFactors = FALSE
  )
  tadat <- new_sum_tadat(raw)

  patches <- list(sum_patch_ns_fun(
    "EPATADA",
    "TADA_GetNutrientSummationRef",
    function() {
      data.frame(a = 1, b = 2)
    }
  ))
  on.exit(lapply(rev(patches), sum_restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_TN_and_TP_summation_server,
    args = list(id = "sum_1", tadat = tadat),
    {
      session$flushReact()
      expect_false(is.null(output$sum_dwn))
    }
  )
})
