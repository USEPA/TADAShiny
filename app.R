# app.R
options(golem.app.prod = TRUE)
options(warn = 2)

# replace mypkg with your package name
TADAShiny::run_app()
