summary2_helpers_patch_ns_fun <- function(ns, fn_name, replacement) {
  old <- get(fn_name, envir = asNamespace(ns))
  assignInNamespace(fn_name, replacement, ns = ns)
  list(ns = ns, fn = fn_name, old = old)
}

summary2_helpers_restore_ns_fun <- function(patch) {
  assignInNamespace(patch$fn, patch$old, ns = patch$ns)
}

new_summary2_helpers_tadat <- function() {
  list2env(
    list(
      original_source = "Query",
      job_id = "job-123",
      statecode = "06",
      countycode = "001",
      example_data = "example.csv",
      siteid = c("S1"),
      siteType = c("River", "Lake"),
      characteristicName = c("Nitrate", "Phosphorus"),
      characteristicType = c("Nutrient"),
      sampleMedia = c("Water"),
      project = c("P1"),
      organization = c("ORG1"),
      startDate = "2020-01-01",
      endDate = "2020-12-31",
      org_table = data.frame(
        OrganizationFormalName = c("Org A", "Org B"),
        stringsAsFactors = FALSE
      ),
      selected_flags = c("Flag A", "Flag B"),
      m2f = "meters",
      selected_filters = data.frame(
        Fields = c("FieldA"),
        Field = c("FieldA"),
        Value = c("x"),
        Filter = c("Exclude"),
        stringsAsFactors = FALSE
      ),
      nd_method = "x times DL",
      od_method = "x times RL",
      nd_mult = "0.5",
      od_mult = "1.5",
      field_sel = "all"
    ),
    parent = emptyenv()
  )
}

write_progress_file <- function(path, vals) {
  e <- list2env(vals, parent = emptyenv())
  save(list = names(vals), file = path, envir = e)
}

test_that("writeFile writes progress fields including selected_filters subset", {
  tadat <- new_summary2_helpers_tadat()
  out_file <- tempfile(fileext = ".RData")

  writeFile(tadat, out_file)
  expect_true(file.exists(out_file))

  e <- new.env(parent = emptyenv())
  load(out_file, envir = e)

  expect_equal(e$job_id, "job-123")
  expect_true("selected_filters" %in% ls(e))
  expect_identical(names(e$selected_filters), c("Fields", "Value", "Filter"))
  expect_equal(e$selected_filters$Fields, "FieldA")
})

test_that("fetchExisting and updateExisting round-trip values", {
  tadat <- new_summary2_helpers_tadat()
  existing <- fetchExisting(tadat)

  expect_equal(existing$job_id, "job-123")
  expect_equal(existing$field_sel, "all")

  new_vals <- existing
  new_vals$job_id <- "job-999"
  new_vals$statecode <- "12"
  new_vals$field_sel <- "key"

  updateExisting(tadat, new_vals)

  expect_equal(tadat$job_id, "job-999")
  expect_equal(tadat$statecode, "12")
  expect_equal(tadat$field_sel, "key")
})

test_that("writeNarrativeDataFrame covers Query branch and selected filters", {
  tadat <- new_summary2_helpers_tadat()
  tadat$original_source <- "Query"

  df <- writeNarrativeDataFrame(tadat)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("Parameter", "Value") %in% names(df)))
  expect_true(any(df$Parameter == "TADA Shiny Job ID"))
  expect_true(any(df$Parameter == "State Code"))
  expect_true(any(df$Parameter == "Selected Flag"))
  expect_true(any(df$Parameter == "Selected Filter"))
  expect_true(any(grepl("Exclude: FieldA = x", df$Value, fixed = TRUE)))
  expect_true(any(df$Parameter == "Depth unit conversion"))
  expect_true(any(df$Parameter == "Non-Detect Handling Method"))
  expect_true(any(df$Parameter == "Over-Detect Handling Method"))
})

test_that("writeNarrativeDataFrame covers Example branch and null multipliers", {
  tadat <- new_summary2_helpers_tadat()
  tadat$original_source <- "Example"
  tadat$m2f <- NULL
  tadat$nd_mult <- NULL
  tadat$od_mult <- NULL
  tadat$selected_filters <- tadat$selected_filters[0, ]

  df <- writeNarrativeDataFrame(tadat)

  expect_true(any(df$Parameter == "Example data file"))
  expect_true(any(df$Parameter == "Depth unit conversion" & df$Value == "None"))
  expect_true(any(
    df$Parameter == "Non-Detect Handling Method" & df$Value == "n/a times DL"
  ))
  expect_true(any(
    df$Parameter == "Over-Detect Handling Method" & df$Value == "n/a times RL"
  ))
})

test_that("invalidFile prints expected failure message", {
  expect_output(invalidFile("trigger"), "Failure: Invalid File", fixed = TRUE)
})

test_that("readFile updates tadat fields and emits success notification", {
  tadat <- new_summary2_helpers_tadat()
  tadat$job_id <- "before"
  tadat$m2f <- "feet"
  tadat$selected_flags <- "old_flag"

  progress_file <- tempfile(fileext = ".RData")
  vals <- list(
    original_source = "Query",
    job_id = "loaded-job",
    example_data = "loaded.csv",
    statecode = "48",
    countycode = "201",
    siteid = c("A1"),
    siteType = c("Stream"),
    characteristicName = c("DO"),
    characteristicType = c("Core"),
    sampleMedia = c("Water"),
    project = c("PRJ"),
    organization = c("ORG"),
    startDate = "2021-01-01",
    endDate = "2021-12-31",
    org_table = data.frame(
      OrganizationFormalName = "Loaded Org",
      stringsAsFactors = FALSE
    ),
    selected_flags = c("loaded_flag"),
    selected_filters = data.frame(
      Fields = "FieldA",
      Value = "y",
      Filter = "Exclude",
      stringsAsFactors = FALSE
    ),
    nd_method = "x times DL",
    od_method = "x times RL",
    nd_mult = "0.25",
    od_mult = "2",
    field_sel = "most",
    m2f = "inches"
  )
  write_progress_file(progress_file, vals)

  note_calls <- character(0)
  patch <- summary2_helpers_patch_ns_fun(
    "shiny",
    "showNotification",
    function(ui, type = "default", duration = 5, id = NULL, ...) {
      msg <- if (is.character(ui)) ui else as.character(ui)
      note_calls <<- c(note_calls, paste(msg, collapse = " "))
      id
    }
  )
  on.exit(summary2_helpers_restore_ns_fun(patch), add = TRUE)

  readFile(tadat, progress_file)

  expect_equal(tadat$load_progress_file, progress_file)
  expect_equal(tadat$job_id, "loaded-job")
  expect_equal(tadat$statecode, "48")
  expect_equal(tadat$m2f, "inches")
  expect_equal(tadat$selected_flags, "loaded_flag")
  expect_equal(tadat$field_sel, "most")
  expect_true(any(grepl("Successfully loaded progress file", note_calls)))
})

test_that("readFile keeps existing m2f and selected_flags when loaded values are NULL", {
  tadat <- new_summary2_helpers_tadat()
  tadat$m2f <- "feet"
  tadat$selected_flags <- c("keep_me")

  progress_file <- tempfile(fileext = ".RData")
  vals <- list(
    original_source = "Query",
    job_id = "loaded-job-2",
    example_data = "loaded2.csv",
    statecode = "36",
    countycode = "001",
    siteid = c("B1"),
    siteType = c("River"),
    characteristicName = c("Nitrate"),
    characteristicType = c("Nutrient"),
    sampleMedia = c("Water"),
    project = c("PRJ2"),
    organization = c("ORG2"),
    startDate = "2022-01-01",
    endDate = "2022-12-31",
    org_table = data.frame(
      OrganizationFormalName = "Loaded Org 2",
      stringsAsFactors = FALSE
    ),
    selected_flags = NULL,
    selected_filters = data.frame(
      Fields = "FieldB",
      Value = "z",
      Filter = "Exclude",
      stringsAsFactors = FALSE
    ),
    nd_method = "x times DL",
    od_method = "x times RL",
    nd_mult = "0.1",
    od_mult = "1",
    field_sel = "all",
    m2f = NULL
  )
  write_progress_file(progress_file, vals)

  patch <- summary2_helpers_patch_ns_fun(
    "shiny",
    "showNotification",
    function(...) NULL
  )
  on.exit(summary2_helpers_restore_ns_fun(patch), add = TRUE)

  readFile(tadat, progress_file)

  expect_equal(tadat$m2f, "feet")
  expect_equal(tadat$selected_flags, "keep_me")
  expect_equal(tadat$job_id, "loaded-job-2")
  expect_equal(tadat$field_sel, "all")
})
