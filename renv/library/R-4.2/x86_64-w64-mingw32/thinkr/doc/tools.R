## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(thinkr)

## -----------------------------------------------------------------------------
dataset <- data.frame(
  col_a = as.factor(letters)[1:7], 
  col_b = letters[1:7],
  col_c = 1:7,
  col_d = paste0(letters[1:7], letters[1:7]),
  stringsAsFactors = FALSE) 

# Show original dataset
dataset

# replace pattern
replace_pattern(dataset, "a", 'XXX-')

## -----------------------------------------------------------------------------
replace_pattern(dataset, "a", 'XXX-', exact = TRUE)

## ---- error=TRUE, message=TRUE------------------------------------------------
## returns TRUE because all levels of iris$species are in c("setosa", "versicolor", "virginica")
is_likert(iris$Species, c("setosa", "versicolor", "virginica"))

## returns TRUE because all levels of iris$species are in c("setosa", "versicolor", "virginica", "banana"), even though there is actually no level "banana"
# A message is printed
is_likert(iris$Species, c("setosa", "versicolor", "virginica", "banana"))

## returns FALSE because the "virginica" level of iris$species is missing
is_likert(iris$Species, c("setosa", "versicolor"))

## returns an error
is_likert(iris$Species, c(1, 2))

## returns no error as the numeric is coerced to a character.
is_likert(iris$Species, c("setosa", 2))


