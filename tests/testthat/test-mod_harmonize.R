harm_patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

harm_restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

new_harm_tadat <- function(raw_df) {
  rv <- shiny::reactiveValues()
  rv$raw <- raw_df
  rv$removals <- data.frame(
    seed = rep(FALSE, nrow(raw_df)),
    stringsAsFactors = FALSE
  )
  rv
}

make_harmonize_ref <- function() {
  data.frame(
    TADA.CharacteristicName = c("Nitrogen", "Phosphorus"),
    Target.TADA.CharacteristicName = c("Total Nitrogen", "Total Phosphorus"),
    TADA.CharacteristicNameAssumptions = c("assume", "assume"),
    TADA.ResultSampleFractionText = c("Dissolved", "Total"),
    Target.TADA.ResultSampleFractionText = c("Total", "Total"),
    TADA.FractionAssumptions = c("assume", "assume"),
    TADA.MethodSpeciationName = c("as N", "as P"),
    Target.TADA.MethodSpeciationName = c("as N", "as P"),
    TADA.SpeciationAssumptions = c("assume", "assume"),
    Target.TADA.SpeciationConversionFactor = c(1, 1),
    HarmonizationGroup = c("N", "P"),
    stringsAsFactors = FALSE
  )
}

test_that("mod_harmonize_np_ui renders expected controls", {
  ui <- mod_harmonize_np_ui("harm_1")
  golem::expect_shinytaglist(ui)
  ui_txt <- as.character(ui)
  expect_true(grepl("Compose Synonym Table", ui_txt, fixed = TRUE))
  expect_true(grepl("harm_1-harm_go", ui_txt, fixed = TRUE))
  expect_true(grepl("harm_1-syn_table", ui_txt, fixed = TRUE))
  expect_true(grepl("harm_1-harm_file", ui_txt, fixed = TRUE))
})

test_that("harm_go builds synonym table and download/apply UI", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    TADA.Remove = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  tadat <- new_harm_tadat(raw)

  patches <- list(
    harm_patch_ns_fun("EPATADA", "TADA_GetSynonymRef", function(df) {
      make_harmonize_ref()
    }),
    harm_patch_ns_fun("shinyjs", "disable", function(...) NULL)
  )
  on.exit(lapply(rev(patches), harm_restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_harmonize_np_server,
    args = list(id = "harm_1", tadat = tadat),
    {
      session$setInputs(harm_go = 1L)
      session$flushReact()

      expect_false(is.null(harm$ref))
      expect_equal(nrow(harm$ref), 2)

      # UI appears once ref exists
      expect_false(is.null(output$harm_dwn))
      expect_false(is.null(output$harm_apply))
      expect_false(is.null(output$syn_table))
    }
  )
})

test_that("harm_file upload accepts valid csv and rejects invalid csv", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    TADA.Remove = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  tadat <- new_harm_tadat(raw)

  valid <- make_harmonize_ref()
  valid_path <- tempfile(fileext = ".csv")
  utils::write.csv(valid, valid_path, row.names = FALSE)

  invalid <- data.frame(foo = 1:2, bar = 3:4)
  invalid_path <- tempfile(fileext = ".csv")
  utils::write.csv(invalid, invalid_path, row.names = FALSE)

  modal_count <- 0L
  patches <- list(harm_patch_ns_fun("shiny", "showModal", function(...) {
    modal_count <<- modal_count + 1L
    invisible(NULL)
  }))
  on.exit(lapply(rev(patches), harm_restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_harmonize_np_server,
    args = list(id = "harm_1", tadat = tadat),
    {
      session$setInputs(
        harm_file = list(datapath = valid_path, name = "valid.csv")
      )
      session$flushReact()
      expect_false(is.null(harm$ref))
      expect_equal(nrow(harm$ref), nrow(valid))

      session$setInputs(
        harm_file = list(datapath = invalid_path, name = "invalid.csv")
      )
      session$flushReact()
      expect_gte(modal_count, 1)
    }
  )
})

test_that("harm_apply success and undo restore original data", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2", "r3"),
    TADA.Remove = c(FALSE, TRUE, FALSE),
    Value = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  tadat <- new_harm_tadat(raw)

  patches <- list(
    harm_patch_ns_fun("EPATADA", "TADA_GetSynonymRef", function(df) {
      make_harmonize_ref()
    }),
    harm_patch_ns_fun("EPATADA", "TADA_HarmonizeSynonyms", function(dat, ref) {
      dat$TADA.Harmonized.Flag <- TRUE
      dat$HarmMarker <- "done"
      dat
    }),
    harm_patch_ns_fun("EPATADA", "TADA_OrderCols", function(df) df),
    harm_patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    harm_patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    harm_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    harm_patch_ns_fun("shinyjs", "enable", function(...) NULL),
    harm_patch_ns_fun("shiny", "showModal", function(...) invisible(NULL))
  )
  on.exit(lapply(rev(patches), harm_restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_harmonize_np_server,
    args = list(id = "harm_1", tadat = tadat),
    {
      original_raw <- tadat$raw

      session$setInputs(harm_go = 1L)
      session$flushReact()
      session$setInputs(harm_apply = 1L)
      session$flushReact()

      expect_false(is.null(tadat$raw_unharmonized))
      expect_true("HarmMarker" %in% names(tadat$raw))
      expect_true(any(tadat$raw$HarmMarker == "done", na.rm = TRUE))

      session$setInputs(undo_harm_apply = 1L)
      session$flushReact()

      expect_equal(tadat$raw, original_raw)
    }
  )
})

test_that("harm_apply error path keeps data unchanged", {
  raw <- data.frame(
    ResultIdentifier = c("r1", "r2"),
    TADA.Remove = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  tadat <- new_harm_tadat(raw)

  patches <- list(
    harm_patch_ns_fun("EPATADA", "TADA_GetSynonymRef", function(df) {
      make_harmonize_ref()
    }),
    harm_patch_ns_fun("EPATADA", "TADA_HarmonizeSynonyms", function(dat, ref) {
      stop("boom")
    }),
    harm_patch_ns_fun("shinybusy", "show_modal_spinner", function(...) NULL),
    harm_patch_ns_fun("shinybusy", "remove_modal_spinner", function(...) NULL),
    harm_patch_ns_fun("shinyjs", "disable", function(...) NULL),
    harm_patch_ns_fun("shinyjs", "enable", function(...) NULL),
    harm_patch_ns_fun("shiny", "showModal", function(...) invisible(NULL))
  )
  on.exit(lapply(rev(patches), harm_restore_ns_fun), add = TRUE)

  shiny::testServer(
    mod_harmonize_np_server,
    args = list(id = "harm_1", tadat = tadat),
    {
      before <- tadat$raw

      session$setInputs(harm_go = 1L)
      session$flushReact()
      session$setInputs(harm_apply = 1L)
      session$flushReact()

      expect_equal(tadat$raw, before)
    }
  )
})

