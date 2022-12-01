## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(thinkr)

## ----examples-1---------------------------------------------------------------
ncol_to_excel(35)
excel_to_ncol("BF")
excel_col()
ncol_to_excel(1:6)
excel_to_ncol(c('AF', 'AG', 'AH'))

