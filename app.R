# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Suppress Shiny support autoloading (extra safety; not strictly necessary if inst/app has no R/)
options(shiny.autoload.r = FALSE)

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

# Golem production mode
options(golem.app.prod = TRUE)

# Explicitly load your package (helps dependency detection)
if (!requireNamespace("TADAShiny", quietly = TRUE)) {
  stop("Package 'TADAShiny' must be installable on the server.")
}
library(TADAShiny)

# Start the golem app
TADAShiny::run_app() # add parameters here (if any)
