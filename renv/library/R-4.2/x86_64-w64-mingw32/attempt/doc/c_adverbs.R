## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>", 
  eval = FALSE
)
library(attempt)

## -----------------------------------------------------------------------------
#  silent_log <- silently(log)
#  silent_log(1)
#  silent_log("a")
#  # Error in .f(...) : non-numeric argument to mathematical function

## -----------------------------------------------------------------------------
#  silent_matrix <- silently(matrix)
#  silent_matrix(1:3, 2)
#  #Warning message:
#  #In .f(...) :
#  #  data length [3] is not a sub-multiple or multiple of the number of rows [2]

## -----------------------------------------------------------------------------
#  sure_log <- surely(log)
#  sure_log(1)
#  # [1] 0
#  sure_log("a")
#  # Error: non-numeric argument to mathematical function

## -----------------------------------------------------------------------------
#  as_num_msg <- with_message(as.numeric, msg = "We're performing a numeric conversion")
#  as_num_warn <- with_warning(as.numeric, msg = "We're performing a numeric conversion")
#  as_num_msg("1")
#  as_num_warn("1")

## -----------------------------------------------------------------------------
#  matrix(1:3, ncol = 2)
#  no_warning_matrix <- without_warning(matrix)
#  no_warning_matrix(1:3, ncol = 2)

