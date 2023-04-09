## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(shinybusy)

## ---- eval=FALSE--------------------------------------------------------------
#  # Add in UI
#  add_busy_spinner(spin = "fading-circle")

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics(path = "figures/add_busy_spinner.png")

## ---- eval=FALSE--------------------------------------------------------------
#  add_busy_bar(color = "red", height = "8px")

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics(path = "figures/add_busy_bar.png")

## ---- eval=FALSE--------------------------------------------------------------
#  # Add in UI
#  add_busy_gif(
#    src = "https://jeroen.github.io/images/banana.gif",
#    height = 70, width = 70
#  )

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics(path = "figures/add_busy_gif.png")

## ---- eval=FALSE--------------------------------------------------------------
#  # in UI
#  use_busy_spinner(spin = "fading-circle")
#  
#  # in server
#  show_spinner() # show the spinner
#  hide_spinner() # hide the spinner

## ---- eval=FALSE--------------------------------------------------------------
#  # in UI
#  use_busy_bar(color = "#01DF01", height = "15px")
#  
#  # in server
#  update_busy_bar(0) # update with the desire value [0-100], 100 hide the bar

## ---- eval=FALSE--------------------------------------------------------------
#  # in UI
#  use_busy_gif(
#    src = "https://jeroen.github.io/images/banana.gif",
#    height = 70, width = 70
#  )
#  
#  # in server
#  play_gif() # play animation
#  stop_gif() # stop animation

## ---- eval=FALSE--------------------------------------------------------------
#  # in server
#  show_modal_spinner() # show the modal window
#  remove_modal_spinner() # remove it when done

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics(path = "figures/modal_spinner.png")

## ---- eval=FALSE--------------------------------------------------------------
#  # in server
#  show_modal_progress_line() # show the modal window
#  update_modal_progress(0.2) # update progress bar value
#  remove_modal_progress() # remove it when done

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics(path = "figures/modal_progress.png")

## ----notifications, eval=FALSE------------------------------------------------
#  # success notification
#  notify_success("Well done!")
#  
#  # report failure
#  report_failure(
#    "Oups...",
#    "Something went wrong"
#  )

