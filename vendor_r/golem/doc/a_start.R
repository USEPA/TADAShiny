## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  install.packages("golem")

## -----------------------------------------------------------------------------
#  remotes::install_github("Thinkr-open/golem")

## ---- echo=FALSE, out.width="80%", fig.align="center", eval=TRUE--------------
knitr::include_graphics("golemtemplate.png")

## -----------------------------------------------------------------------------
#  golem::create_golem(path = "path/to/package")

## ----include = FALSE, eval = TRUE, error = TRUE-------------------------------
x <- fs::path(tempdir(), "golex")
try(fs::dir_delete(x), silent = TRUE)
golem::create_golem(path = x, package_name = "golex", open = FALSE)

## ----echo = FALSE, eval = TRUE------------------------------------------------
z <- capture.output(fs::dir_tree(x))
z <- z[-1]
w <- lapply(
  z,
  function(x) {
    cat(x, "\n")
  }
)

## -----------------------------------------------------------------------------
#  golem::fill_desc(
#    pkg_name = "shinyexample", # The Name of the package containing the App
#    pkg_title = "PKG_TITLE", # The Title of the package containing the App
#    pkg_description = "PKG_DESC.", # The Description of the package containing the App
#    author_first_name = "AUTHOR_FIRST", # Your First Name
#    author_last_name = "AUTHOR_LAST", # Your Last Name
#    author_email = "AUTHOR@MAIL.COM", # Your Email
#    repo_url = NULL, # The (optional) URL of the GitHub Repo
#    pkg_version = "0.0.0.9000" # The Version of the package containing the App
#  )

## -----------------------------------------------------------------------------
#  golem::set_golem_options()

## ----eval = FALSE-------------------------------------------------------------
#  ## See ?usethis for more information
#  usethis::use_mit_license("Golem User") # You can set another license here
#  usethis::use_readme_rmd(open = FALSE)
#  usethis::use_code_of_conduct()
#  usethis::use_lifecycle_badge("Experimental")
#  usethis::use_news_md(open = FALSE)

## -----------------------------------------------------------------------------
#  golem::use_recommended_tests()

## -----------------------------------------------------------------------------
#  golem::use_recommended_deps()

## -----------------------------------------------------------------------------
#  # Remove current favicon
#  golem::remove_favicon()
#  # Add a new one
#  golem::use_favicon(path = "path/to/favicon")

## -----------------------------------------------------------------------------
#  golem::use_utils_ui()
#  golem::use_utils_server()

## -----------------------------------------------------------------------------
#  golem::run_dev()

## -----------------------------------------------------------------------------
#  rstudioapi::navigateToFile("dev/02_dev.R")

## ----eval = TRUE--------------------------------------------------------------
try(fs::dir_delete(x), silent = TRUE)

