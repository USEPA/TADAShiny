## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  attachment::att_amend_desc()

## -----------------------------------------------------------------------------
#  golem::add_module(name = "my_first_module") # Name of the module

## -----------------------------------------------------------------------------
#  # mod_UI
#  mod_my_first_module_ui <- function(id) {
#    ns <- NS(id)
#    tagList()
#  }
#  
#  mod_my_first_module_server <- function(input, output, session) {
#    ns <- session$ns
#  }
#  
#  ## To be copied in the UI
#  # mod_my_first_module_ui("my_first_module_1")
#  
#  ## To be copied in the server
#  # callModule(mod_my_first_module_server, "my_first_module_1")

## -----------------------------------------------------------------------------
#  golem::add_fct("helpers")
#  golem::add_utils("helpers")

## -----------------------------------------------------------------------------
#  golem::add_js_file("script")
#  golem::add_js_handler("script")
#  golem::add_css_file("custom")
#  golem::add_sass_file("custom")

## -----------------------------------------------------------------------------
#  golem::use_external_css_file(url = "url", name = "your_provided_name")
#  golem::use_external_js_file(url = "url", name = "your_provided_name")

## -----------------------------------------------------------------------------
#  tags$img(src = "www/my.png")

## -----------------------------------------------------------------------------
#  usethis::use_data_raw()

## -----------------------------------------------------------------------------
#  usethis::use_test("app")

## -----------------------------------------------------------------------------
#  usethis::use_vignette("shinyexample")
#  devtools::build_vignettes()

## -----------------------------------------------------------------------------
#  usethis::use_travis()
#  usethis::use_appveyor()
#  usethis::use_coverage()

## ----eval = TRUE--------------------------------------------------------------
options("golem.app.prod" = TRUE)
golem::cat_dev("hey\n")
options("golem.app.prod" = FALSE)
golem::cat_dev("hey\n")

## ----eval = TRUE--------------------------------------------------------------
log_dev <- golem::make_dev(log)
log_dev(10)
options("golem.app.prod" = TRUE)
log_dev(10)

