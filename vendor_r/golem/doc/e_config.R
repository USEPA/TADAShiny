## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(golem)
x <- file.path(
  tempdir(),
  "golex"
)
unlink(x, TRUE, TRUE)

x <- golem::create_golem(x, package_name = "golex", open = FALSE)
old <- setwd(x)
knitr::opts_knit$set(root.dir = x)

## ----setup--------------------------------------------------------------------
library(golem)

## ----echo = FALSE-------------------------------------------------------------
old <- setwd(x)

## -----------------------------------------------------------------------------
set_golem_options()

## ----echo = FALSE, comment= ""------------------------------------------------
cat(
  sep = "\n",
  readLines(
    "inst/golem-config.yml"
  )
)

## -----------------------------------------------------------------------------
get_golem_name()
get_golem_wd()
get_golem_version()

## ----eval = FALSE-------------------------------------------------------------
#  set_golem_name("this")
#  set_golem_wd(".")
#  set_golem_version("0.0.1")

## ----echo = FALSE, comment= ""------------------------------------------------
cat(
  sep = "\n",
  readLines(
    "inst/golem-config.yml"
  )
)

## -----------------------------------------------------------------------------
amend_golem_config(
  key = "where",
  value = "indev"
)
amend_golem_config(
  key = "where",
  value = "inprod",
  config = "production"
)

## ----echo = FALSE, comment= ""------------------------------------------------
cat(
  sep = "\n",
  readLines(
    file.path(x, "inst/golem-config.yml")
  )
)

## -----------------------------------------------------------------------------
pkgload::load_all()
get_golem_config(
  "where"
)
get_golem_config(
  "where",
  config = "production"
)

## -----------------------------------------------------------------------------
Sys.setenv("R_CONFIG_ACTIVE" = "production")
get_golem_config("where")

## ----echo = FALSE-------------------------------------------------------------
setwd(old)

