# Launch the ShinyApp (Do not remove this file)
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,
                  helpers = FALSE,
                  attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
options(warn=2)
TADAShiny::run_app() # add parameters here (if any)

# hit republish 
