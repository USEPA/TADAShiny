testthat::test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")) {
    expect_true(i %in% names(fmls))
  }
})

testthat::test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")) {
    expect_true(i %in% names(fmls))
  }
})

testthat::test_that(
  "app_sys works",
  {
    expect_true(
      app_sys("golem-config.yml") != ""
    )
  }
)

testthat::test_that(
  "golem-config works",
  {
    config_file <- app_sys("golem-config.yml")
    skip_if(config_file == "")

    expect_true(
      get_golem_config(
        "app_prod",
        config = "production"
      )
    )
    expect_false(
      get_golem_config(
        "app_prod",
        config = "dev"
      )
    )
  }
)
