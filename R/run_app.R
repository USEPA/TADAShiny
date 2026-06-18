#' Run the Shiny Application
#'
#' @description Launch the Shiny application.
#'
#' @inheritParams shiny::shinyApp onStart options enableBookmarking uiPattern
#' @param ... Named options forwarded to `golem_opts` via [golem::with_golem_options()],
#'   retrievable with [golem::get_golem_options()].
#'
#' @return A shiny.appobj returned by [shiny::shinyApp()].
#' @export
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...
) {
  # Read MB_LIMIT and set shiny.maxRequestSize immediately when valid (> 0)
  limit_raw <- get_golem_config("MB_LIMIT", default = 500)
  limit_mb <- suppressWarnings(as.numeric(limit_raw))
  if (!is.na(limit_mb) && is.finite(limit_mb) && limit_mb > 0) {
    base::options(shiny.maxRequestSize = limit_mb * 1024^2)
  }
  
  # Optional: apply TIMEOUT_SECONDS immediately when valid (> 0)
  timeout_raw <- get_golem_config("TIMEOUT_SECONDS", default = 3600)
  timeout_sec <- suppressWarnings(as.numeric(timeout_raw))
  if (!is.na(timeout_sec) && is.finite(timeout_sec) && timeout_sec > 0) {
    base::options(shiny.timeout = timeout_sec)
  }
  
  # Pass onStart through unchanged and forward options/enableBookmarking/uiPattern
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
