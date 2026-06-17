#' Resolve application system paths
#'
#' Return a path inside the installed package if available; otherwise
#' fall back to local paths in the app bundle (inst/... first, then ./...).
#' The function always returns a single character string. If no existing
#' candidate is found, it returns the first candidate path (which may not exist),
#' so callers can decide how to proceed or fail clearly.
#'
#' @param ... Path segments (character), specifying subdirectory and file
#'   within your package. All elements are treated as sequential segments,
#'   not as alternatives. For example, `app_sys("app", "www", "favicon.ico")`.
#' @param package Package name where to look for installed files. Defaults
#'   to "TADAShiny"; update if the package is renamed or set explicitly.
#'
#' @return A length-1 character string with a normalized path. It may point
#'   to a non-existent file if no candidate exists locally or in the package.
#'
#' @keywords internal
#' @noRd
app_sys <- function(..., package = "TADAShiny") {
  segs <- unlist(list(...), recursive = TRUE, use.names = FALSE)
  segs <- segs[!is.na(segs) & nzchar(segs)]
  
  # Try installed package location first (mustWork = FALSE prevents errors)
  if (length(segs) == 0L) {
    p <- system.file(package = package, mustWork = FALSE)
  } else {
    p <- do.call(
      system.file,
      c(as.list(segs), list(package = package, mustWork = FALSE))
    )
  }
  
  if (nzchar(p)) {
    return(p)
  }
  
  # Fallbacks
  if (length(segs) == 0L) {
    # Root fallback: local project root
    return(normalizePath(".", winslash = "/", mustWork = FALSE))
  }
  
  # Join segments into a relative path for local fallbacks
  rel <- do.call(file.path, as.list(segs))
  candidates <- c(file.path("inst", rel), rel)
  
  for (cand in candidates) {
    if (file.exists(cand)) {
      return(normalizePath(cand, winslash = "/", mustWork = FALSE))
    }
  }
  
  # If nothing exists, return the first candidate as a single string
  normalizePath(candidates[[1]], winslash = "/", mustWork = FALSE)
}


#' Read app configuration (golem-config.yml)
#'
#' Retrieve a value from golem-config.yml using robust file resolution:
#' first via app_sys() (installed package), then local fallbacks
#' (./golem-config.yml, inst/golem-config.yml). If the requested key is
#' not found and `default` is provided, return `default`; otherwise error.
#'
#' @param value Value to retrieve from the config file.
#' @param config Active configuration name. Defaults to GOLEM_CONFIG_ACTIVE,
#'   then R_CONFIG_ACTIVE, and finally "default" if unset.
#' @param use_parent Logical; whether to scan parent directories
#'   for the config file (passed to config::get).
#' @param default Optional default value to return if `value` is not found
#'   in the active configuration. If NULL (the default), missing keys error.
#'
#' @return The value from the config, or `default` if provided and key is missing.
#'
#' @seealso [config::get()]
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
  
  # Try to get the value; if missing and default is provided, return default
  tryCatch(
    config::get(value = value, config = config, file = f, use_parent = use_parent),
    error = function(e) {
      if (!is.null(default)) {
        return(default)
      }
      stop(e)
    }
  )
}
