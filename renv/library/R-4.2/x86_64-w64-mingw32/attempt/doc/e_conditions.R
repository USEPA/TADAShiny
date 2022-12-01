## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>", 
  eval = FALSE
)
library(attempt)

## -----------------------------------------------------------------------------
#  x <- 12
#  # Stop if .x is numeric
#  stop_if(.x = x,
#          .p = is.numeric)
#  
#  y <- "20"
#  # stop if .x is not numeric
#  stop_if_not(.x = y,
#              .p = is.numeric,
#              msg = "y should be numeric")
#  a  <- "this is not numeric"
#  # Warn if .x is charcter
#  warn_if(.x = a,
#          .p = is.character)
#  
#  b  <- 20
#  # Warn if .x is not equal to 10
#  warn_if_not(.x = b,
#          .p = ~ .x == 10 ,
#          msg = "b should be 10")
#  
#  c <- "a"
#  # Message if c is a character
#  message_if(.x = c,
#             .p = is.character,
#             msg = "You entered a character element")
#  
#  # Build more complex predicates
#  d <- 100
#  message_if(.x = d,
#             .p = ~ sqrt(.x) < 42,
#             msg = "The square root of your element must be more than 42")
#  
#  # Or, if you're kind of old school, you can still pass classic functions
#  
#  e <- 30
#  message_if(.x = e,
#             .p = function(vec){
#               return(sqrt(vec) < 42)
#             },
#             msg = "The square root of your element must be more than 42")

## -----------------------------------------------------------------------------
#  stop_if(.x = curl::has_internet(), msg = "You shouldn't have internet to do that")
#  
#  warn_if(.x = curl::has_internet(),
#              msg = "You shouldn't have internet to do that")
#  
#  message_if(.x = curl::has_internet(),
#              msg = "Huray, you have internet \\o/")

## -----------------------------------------------------------------------------
#  a <- is.na(airquality$Ozone)
#  message_if_any(a, msg = "NA found")

## -----------------------------------------------------------------------------
#  my_fun <- function(x){
#    stop_if_not(.x = curl::has_internet(),
#                msg = "You should have internet to do that")
#    warn_if_not(x,
#            is.character,
#            msg =  "x is not a character vector. The output may not be what you're expecting.")
#    paste(x, "is the value.")
#  }
#  
#  my_fun(head(iris))

## -----------------------------------------------------------------------------
#  stop_if_any(iris, is.factor, msg = "Factors here. This might be due to stringsAsFactors.")
#  
#  warn_if_none(1:10, ~ .x < 0, msg = "You need to have at least one number under zero.")
#  
#  message_if_all(1:100, is.numeric, msg = "That makes a lot of numbers.")

