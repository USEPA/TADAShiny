# Launch the ShinyApp (Do not remove this file)
# To deploy, run the script in 04_deploy.R

# Suppress Shiny support autoloading (extra safety; not strictly necessary if inst/app has no R/)
options(shiny.autoload.r = FALSE)

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)

# Golem production mode
options(golem.app.prod = TRUE)

# Start the golem app
run_app()
