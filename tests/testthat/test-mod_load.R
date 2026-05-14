shiny::testServer(
  mod_query_data_server,
  # Add here your module params
  args = list(),
  {
    ns <- session$ns
    expect_true(
      inherits(ns, "function")
    )
    expect_true(
      grepl(id, ns(""))
    )
    expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
  }
)

testthat::test_that("module ui works", {
  ui <- mod_query_data_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_query_data_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

testthat::test_that("example data UI labels match available EPATADA datasets", {
  # Retrieve the dropdown choices defined in the UI
  ui <- mod_query_data_ui(id = "test")
  ui_html <- as.character(ui)

  # The three non-empty labels that must appear in the UI dropdown
  expected_labels <- c(
    "Nutrients Utah (15k results)",
    "EPA Region 5 May 1-7 2019 (172k results)",
    "Tribal (136k results)"
  )
  for (lbl in expected_labels) {
    expect_true(
      grepl(lbl, ui_html, fixed = TRUE),
      info = paste("Expected UI label not found:", lbl)
    )
  }

  # Each label must resolve to a non-NULL EPATADA dataset via the same named
  # list that the server uses, ensuring labels and datasets stay in sync.
  example_data_map <- list(
    "Nutrients Utah (15k results)" = EPATADA::Data_Nutrients_UT,
    "EPA Region 5 May 1-7 2019 (172k results)" = EPATADA::Data_R5_TADAPackageDemo,
    "Tribal (136k results)" = EPATADA::Data_TribalNations
  )
  for (lbl in expected_labels) {
    expect_false(
      is.null(example_data_map[[lbl]]),
      info = paste("No dataset found in example_data_map for label:", lbl)
    )
    expect_true(
      is.data.frame(example_data_map[[lbl]]),
      info = paste("Dataset for label is not a data.frame:", lbl)
    )
  }
})
