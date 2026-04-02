shiny::testServer(
  mod_overview_server,
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

test_that("module ui works", {
  ui <- mod_depth_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_depth_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})


  # shiny::testServer(mod_depth_server, args = list(id = "depth_1", tadat = tadat), {
  #   # stub EPATADA and shinybusy functions used in review observer
  #   testthat::local_mock(
  #     `EPATADA::TADA_FlagDepthCategory` = function(df, ...) {
  #       depth_flagged
  #     },
  #     `EPATADA::TADA_IDDepthProfiles` = function(df, ...) site_date_char_groups_df,
  #     `shinybusy::show_modal_spinner` = function(...) NULL,
  #     `shinybusy::remove_modal_spinner` = function(...) NULL,
  #     `shiny::showModal` = function(...) NULL
  #   )
  # 
  #   # simulate clicking the review button: need to trigger the observeEvent
  #   session$setInputs(review_depth_profile_data = 1)
  # 
  #   # After the observer runs, depth_profile should be loaded
  #   expect_true(isTRUE(depth_profile$loaded))
  #   expect_false(isTRUE(depth_profile$no_data))
  # 
  #   # site_date_pairs should be a data frame with MonitoringLocationIdentifier and ActivityStartDate
  #   expect_true(is.data.frame(depth_profile$site_date_pairs))
  #   expect_true("MonitoringLocationIdentifier" %in% names(depth_profile$site_date_pairs))
  #   expect_true(nrow(depth_profile$site_date_pairs) >= 1)
  # 
  #   # available_characteristics_df should be initialized (empty)
  #   expect_true(is.data.frame(depth_profile$available_characteristics_df))
  # })


