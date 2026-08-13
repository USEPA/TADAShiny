#' Resolve application system paths
#'
#'Path segments are passed in order, e.g. `app_sys("www", "favicon.ico")`.
#'
#' @param ... Path segments (character), specifying the subdirectory/file path
#'   within the package. These are treated as sequential segments, not alternatives.
#' @param package Package name to search. Defaults to `"TADAShiny"`.
#'
#' @return A length-1 character string. Returns `""` if no matching file is found.
#'
#' @keywords internal
#' @noRd
app_sys <- function(..., package = "TADAShiny") {
  system.file(..., package = "TADAShiny")
}

#' Read app configuration (`golem-config.yml`)
#'
#' Retrieves a value from `golem-config.yml` using robust file resolution:
#' first via app_sys() (installed package), then local fallbacks
#' (`./golem-config.yml`, `inst/golem-config.yml`). If the requested key is
#' not found and `default` is provided, returns `default`; otherwise errors.
#'
#' @param value Name of the value to retrieve from the config file.
#' @param config Active configuration name. Defaults to `GOLEM_CONFIG_ACTIVE`,
#'   then `R_CONFIG_ACTIVE`, and finally `"default"` if unset.
#' @param use_parent Logical; whether to scan parent directories
#'   for the config file (passed to config::get()).
#' @param default Optional default value to return if `value` is not found
#'   in the active configuration. If `NULL` (the default), missing keys error.
#'
#' @return The value from the config, or `default` if provided and the key is missing.
#'
#' @seealso config::get()
#' @keywords internal
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv("R_CONFIG_ACTIVE", "default")
    ),
    use_parent = TRUE,
    default = NULL
) {
  # Resolve file via app_sys (installed or local) — guaranteed scalar
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
        "Checked: installed package, ./golem-config.yml, inst/golem-config.yml. ",
        "Set GOLEM_CONFIG_ACTIVE or R_CONFIG_ACTIVE appropriately or include the file in the bundle."
      )
    }
  }
  
  if (length(f) != 1L) {
    stop(
      "Internal error: resolved config path must be a single string, got length ",
      length(f),
      "."
    )
  }
  
  tryCatch(
    config::get(
      value = value,
      config = config,
      file = f,
      use_parent = use_parent
    ),
    error = function(e) {
      if (!is.null(default)) {
        return(default)
      }
      stop(e)
    }
  )
}
