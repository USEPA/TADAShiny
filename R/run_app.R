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
  limit_mb <- as.numeric(golem::get_golem_config("MB_LIMIT", default = 500))
  timeout_sec <- as.numeric(golem::get_golem_config(
    "TIMEOUT_SECONDS",
    default = 3600
  ))

  # Wrap onStart so we always apply the options, then call any user-provided onStart
  app_onStart <- function() {
    old <- options(
      shiny.maxRequestSize = limit_mb * 1024^2,
      shiny.timeout = timeout_sec
    )
    # Restore previous options when the app stops
    try(shiny::onStop(function() options(old)), silent = TRUE)
    if (!is.null(onStart)) onStart()
  }

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = app_onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
