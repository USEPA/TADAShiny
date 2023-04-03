#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
#'

# THE BUSINESS STARTS ON line 223 or thereabouts.
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shiny::fluidPage(
      tags$html(class = "no-js", lang="en"),
      epa_header,
      shiny::includeHTML(app_sys("app/www/header.html")),
      shinyjs::useShinyjs(),
      htmltools::br(),
      shiny::headerPanel(title = "TADAShiny"),
      htmltools::br(),
      shiny::tabsetPanel( # create a navbar page with tabs at the top
        id = "tabbar",
        shiny::tabPanel("Load", # each tabPanel represents a tab page at the top of the navbar
                 htmltools::br(),
                 mod_query_data_ui("query_data_1")), #,
        shiny::tabPanel("Overview",
                 htmltools::br(),
                 mod_overview_ui("overview_1"),
                 htmltools::hr(),
                 mod_summary_ui("summary_1")),
        shiny::tabPanel("Flag",
                        htmltools::br(),
                 mod_data_flagging_ui("data_flagging_1")),
        shiny::tabPanel("Filter", id="filter"),
        shiny::tabPanel("Harmonize"),
        shiny::tabPanel("Censored Data"),
        shiny::tabPanel("Explore"),
        shiny::tabPanel("Download")
      ),
      htmltools::hr(),
      mod_TADA_summary_ui("TADA_summary_1"),
      shiny::includeHTML(app_sys("app/www/footer.html"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#'

golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "TADAShiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
