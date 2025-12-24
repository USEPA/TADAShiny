# Suppress Shiny support autoloading (extra safety; not strictly necessary if inst/app has no R/)
options(shiny.autoload.r = FALSE)

# Golem production mode
options(golem.app.prod = TRUE)

# Explicitly load your package (helps dependency detection)
if (!requireNamespace("TADAShiny", quietly = TRUE)) {
  stop("Package 'TADAShiny' must be installable on the server.")
}
library(TADAShiny)

# Start the golem app
TADAShiny::run_app()
