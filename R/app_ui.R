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
    fluidPage(
      tags$html(class = "no-js", lang="en"),
      epa_header,
      # shinyjs::useShinyjs(),
      title = "TADAShiny",
      tabsetPanel( # create a navbar page with tabs at the top
        id = "tabbar", selected = "Overview",
        tabPanel("Load", # each tabPanel represents a tab page at the top of the navbar
                 br(),
                 mod_upload_data_ui("upload_data_1"), # these are ui calls to the different module uis
                 mod_query_data_ui("query_data_1"),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Overview",
                 br(),
                 mod_overview_ui("overview_1"),
                 hr(),
                 mod_summary_ui("summary_1"),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Flag",
                 br(),
                 mod_data_flagging_ui("data_flagging_1")),
        tabPanel("Filter", id="filter",
                 br(),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Harmonize",
                 br(),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Censored Data",
                 br(),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Explore",
                 br(),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
        tabPanel("Download",
                 br(),
                 hr(),
                 mod_TADA_summary_ui("TADA_summary_1")),
      ),
      epa_footer
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
