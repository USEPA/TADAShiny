.onLoad <- function(libname, pkgname) {
  # Get the current working directory
  project_dir <- getwd()
  
  # Set the TMPDIR environment variable to the project directory
  Sys.setenv(TMPDIR = project_dir)
  
  # Print a message for debugging purposes (optional)
  message("EPATADA: TMPDIR set to: ", Sys.getenv("TMPDIR"))
}
