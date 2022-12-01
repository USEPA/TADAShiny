
# hz updated the following line to check and install the golem and used packages 1/4/2022
if (!require("pacman")) install.packages("pacman")
pacman::p_load("golem", "thinkr", "shiny", "shinycssloaders", "dplyr", "xlsx", 
               "readr", "magrittr", "bootstrap", "stringr", 
               "NADA", "readxl", "DT", "plotly", "ggplot2", "tidyr", "ggpubr", 
               "cowplot", "textshape",  "boot")

# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app()
