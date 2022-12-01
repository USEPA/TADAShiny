## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>"
)
library(attempt)

## ----eval=TRUE----------------------------------------------------------------
if_all(1:10, ~ .x < 11, ~ return(letters[1:10]))

if_any(1:10, is.numeric, ~ "Yay!")

if_none(1:10, is.character, ~ rnorm(10))

## ----eval=TRUE----------------------------------------------------------------
a <- c(FALSE, TRUE, TRUE, TRUE)

if_any(a, .f = ~ "nop!")

## ----eval=TRUE----------------------------------------------------------------
if_then(1, is.numeric, ~ "nop!")

## ----eval = TRUE--------------------------------------------------------------
if_not(.x = 1, .p = is.character, ~ ".x is not a character")

## ----eval=TRUE----------------------------------------------------------------
a <- if_else(1, is.numeric, ~ "Yay", ~ "Nay")
a

