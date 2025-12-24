#'
#' NOTE: If you manually change your package name in the DESCRIPTION,
#' don't forget to change it here too, and in the config file.
#' For a safer name change mechanism, use the `golem::set_golem_name()` function.
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package. The default, none, returns the root of the app.
#'
#' @noRd
#'
# Return a path inside the installed package if available; otherwise
# fall back to local paths in the app bundle (inst/..., then . / ...).
app_sys <- function(..., package = "TADAShiny") {
  parts <- list(...)

  # Case: no arguments -> return app root (installed pkg root if available)
  if (length(parts) == 0) {
    p <- system.file(package = package)
    if (nzchar(p)) {
      return(p)
    }
    return(normalizePath(".", winslash = "/", mustWork = FALSE))
  }

  # Try installed package file first
  p <- system.file(..., package = package)
  if (nzchar(p)) {
    return(p)
  }

  # Fallbacks for plain Shiny deployment from source
  candidates <- c(
    file.path("inst", ...),
    file.path(".", ...)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing[[1]])
  }

  # If nothing exists, return the first candidate so callers can decide or fail clearly
  candidates[[1]]
}


#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#'
#' @noRd
# Read App Config with safe file resolution
get_golem_config <- function(
  value,
  config = Sys.getenv(
    "GOLEM_CONFIG_ACTIVE",
    Sys.getenv("R_CONFIG_ACTIVE", "default")
  ),
  use_parent = TRUE
) {
  # Resolve file via app_sys (installed or local)
  f <- app_sys("golem-config.yml")

  # If app_sys returns a non-empty path but the file doesn't exist, try local fallbacks
  if (!nzchar(f) || !file.exists(f)) {
    if (file.exists("golem-config.yml")) {
      f <- "golem-config.yml"
    } else if (file.exists(file.path("inst", "golem-config.yml"))) {
      f <- file.path("inst", "golem-config.yml")
    } else {
      stop(
        "Config file 'golem-config.yml' not found. ",
        "Checked: system.file(), ./golem-config.yml, inst/golem-config.yml. ",
        "Set GOLEM_CONFIG_ACTIVE/R_CONFIG_ACTIVE appropriately or include the file in the bundle."
      )
    }
  }

  config::get(
    value = value,
    config = config,
    file = f,
    use_parent = use_parent
  )
}
