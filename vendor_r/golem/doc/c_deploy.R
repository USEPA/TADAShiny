## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  run_app <- function(...) {
#    with_golem_options(
#      app = shinyApp(
#        ui = app_ui,
#        server = app_server
#      ),
#      golem_opts = list(...)
#    )
#  }

## -----------------------------------------------------------------------------
#  run_app(this = "that")
#  # And in the app
#  this <- get_golem_options("this")

## -----------------------------------------------------------------------------
#  golem::add_rstudioconnect_file()
#  golem::add_shinyappsio_file()
#  golem::add_shinyserver_file()

## -----------------------------------------------------------------------------
#  # If you want to deploy via a generic Dockerfile
#  golem::add_dockerfile()
#  
#  # If you want to deploy to ShinyProxy
#  golem::add_dockerfile_shinyproxy()
#  
#  # If you want to deploy to Heroku
#  golem::add_dockerfile_heroku()

## -----------------------------------------------------------------------------
#  # If you want to deploy via a generic Dockerfile
#  golem::add_dockerfile_with_renv(output_dir = "deploy")
#  
#  # If you want to deploy to ShinyProxy
#  golem::add_dockerfile_with_renv_shinyproxy(output_dir = "deploy")

## -----------------------------------------------------------------------------
#  attachment::create_renv_for_dev(dev_pkg = c(
#    "renv",
#    "devtools",
#    "roxygen2",
#    "usethis",
#    "pkgload",
#    "testthat",
#    "remotes",
#    "covr",
#    "attachment",
#    "pak",
#    "dockerfiler",
#    "golem"
#  ))

## -----------------------------------------------------------------------------
#  renv::activate()

## -----------------------------------------------------------------------------
#  
#  # If you want to deploy via a generic Dockerfile
#  golem::add_dockerfile_with_renv(output_dir = "deploy", lockfile = "renv.lock")
#  
#  # If you want to deploy to ShinyProxy
#  golem::add_dockerfile_with_renv_shinyproxy(output_dir = "deploy", lockfile = "renv.lock")

