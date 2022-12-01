## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)
library(attempt)

## -----------------------------------------------------------------------------
#  attempt(log("a"))
#  # Error: non-numeric argument to mathematical function
#  attempt(log("a"), msg = "Nop !")
#  # Error: Nop !

## -----------------------------------------------------------------------------
#  attempt(log("a"), msg = "Nop !", verbose = TRUE)
#  # Error in log("a"): Nop !

## -----------------------------------------------------------------------------
#  attempt(log(1), msg = "Nop !", verbose = TRUE)
#  # [1] 0

## -----------------------------------------------------------------------------
#  a <- attempt(log("a"), msg = "Nop !", verbose = TRUE)
#  a
#  # [1] "Error in log(\"a\"): Nop !\n"
#  # attr(,"class")
#  # [1] "try-error"
#  # attr(,"condition")
#  # <simpleError in log("a"): Nop !>

## -----------------------------------------------------------------------------
#  a <- attempt(log("a"), msg = "Nop !", verbose = FALSE)
#  is_try_error(a)

## -----------------------------------------------------------------------------
#  silent_attempt(log("a"))
#  # Error: non-numeric argument to mathematical function
#  silent_attempt(log(1))

## -----------------------------------------------------------------------------
#  try_catch(expr = log("a"),
#            .e = ~ paste0("There is an error: ", .x),
#            .w = ~ paste0("This is a warning: ", .x))
#  #[1] "There is an error: Error in log(\"a\"): non-numeric argument to mathematical function\n"
#  
#  try_catch(log("a"),
#            .e = ~ stop(.x),
#            .w = ~ warning(.x))
#  # Error in log("a") : non-numeric argument to mathematical function
#  
#  try_catch(matrix(1:3, nrow= 2),
#            .e = ~ print(.x),
#            .w = ~ print(.x))
#  #<simpleWarning in matrix(1:3, nrow = 2): data length [3] is not a sub-multiple or multiple of the number of rows [2]>
#  
#  try_catch(expr = 2 + 2 ,
#            .f = ~ print("Using R for addition... ok I'm out!"))
#  # [1] "Using R for addition... ok I'm out!"
#  # [1] 4

## -----------------------------------------------------------------------------
#  try_catch(matrix(1:3, nrow = 2), .e = ~ print("error"))
#  #      [,1] [,2]
#  # [1,]    1    3
#  # [2,]    2    1
#  # Warning message:
#  # In matrix(1:3, nrow = 2) :
#  # data length [3] is not a sub-multiple or multiple of the number of rows [2]

## -----------------------------------------------------------------------------
#  try_catch(matrix(1:3, nrow = 2), .w = ~ print("warning"))
#  # [1] "warning"

## -----------------------------------------------------------------------------
#  try_catch(log("a"),
#            .e = function(e){
#              print(paste0("There is an error: ", e))
#              print("Ok, let's save this")
#              time <- Sys.time()
#              a <- paste("+ At",time, ", \nError:",e)
#              # write(a, "log.txt", append = TRUE) # commented to prevent log.txt creation
#              print(paste("log saved on log.txt at", time))
#              print("let's move on now")
#            })
#  
#  # [1] "There is an error: Error in log(\"a\"): non-numeric argument to mathematical function\n"
#  # [1] "Ok, let's save this"
#  # [1] "log saved on log.txt at 2018-01-30 16:59:13"
#  # [1] "let's move on now"

## -----------------------------------------------------------------------------
#  try_catch(log("a"),
#            .e = function(e){
#              paste0("There is an error: ", e)
#            },
#            .f = ~ print("I'm not sure you can do that pal !"))
#  # [1] "I'm not sure you can do that pal !"
#  # [1] "There is an error: Error in log(\"a\"): non-numeric argument to mathematical function\n"
#  
#  try_catch(log("a"),
#            .e = ~ paste0("There is an error: ", .x),
#            .f = function() print("I'm not sure you can do that pal !"))
#  # [1] "I'm not sure you can do that pal !"
#  # [1] "There is an error: Error in log(\"a\"): non-numeric argument to mathematical function\n"

## -----------------------------------------------------------------------------
#  res_log <- try_catch_df(log("a"))
#  res_log
#  res_log$value
#  
#  res_matrix <- try_catch_df(matrix(1:3, nrow = 2))
#  res_matrix
#  res_matrix$value
#  
#  res_success <- try_catch_df(log(1))
#  res_success
#  res_success$value

## -----------------------------------------------------------------------------
#  map_try_catch(l = list(1, 3, "a"), fun = log, .e = ~ .x)
#  
#  map_try_catch_df(list(1,3,"a"), log)

