#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# THE BUSINESS STARTS ON line 223 or thereabouts.
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tags$html(class = "no-js", lang="en"),
      # Site Header
      epa_header,
      shinyjs::useShinyjs(),
      navbarPage(
        title = tagList(span("TADAShiny", style = "padding: 10px; font-weight: bold; font-size: 35px")),
        # id = "navbar",
        tabPanel("Load",
                 mod_upload_data_ui("upload_data_1"),
                 mod_query_data_ui("query_data_1"),
                 br(),
                 mod_summary_ui("summary_1"),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Overview",
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Flag",
                 mod_data_flagging_ui("data_flagging_1"),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Filter",
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Harmonize",
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Censored Data",
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Explore",
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Download",
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
      )
      ,
      # EPA Site Footer
      epa_footer
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ehTADAShiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
