#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
#'

# THE BUSINESS STARTS ON line 223 or thereabouts.
# css below addresses https://github.com/USEPA/TADAShiny/issues/198?
css <- "
.nav li a.disabled {
  background-color: #F5F5F5 !important;
  color: #333 !important;
  pointer-events: none; /* added 2025-Nov-17 Disable mouse events */
  cursor: not-allowed !important;
  border-color: #F5F5F5 !important;
}
/* This displays the custom cursor on the disabled anchor.  It gets overwritten when the nav li is activated. */
.nav li {
  cursor: not-allowed !important;
}
.row {
    margin-right: 0px;
    margin-left: 0px;
}

/* Put shiny notifications front and center */
.shiny-notification {
  position: fixed;
  top: calc(50%);
  left: calc(50%);
  transform: translate(-50%, -50%); /* Centers the element precisely */
  border-style: solid;
  border-width: medium;
}
"

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    # This function automatically incorporates the epa styles.css file included
    # in the www folder. Downloaded from https://www.epa.gov/web-policies-and-procedures/web-standards-look-and-feel-template
    # styles.css hosted locally in this app includes a fix (for compatibility with leaflet and plotly)
    golem_add_external_resources(),
    # Your application UI logic
    shiny::fluidPage(
      tags$html(class = "no-js", lang = "en"),
      # standardized Go to Top button appears on lower-right corner when window is scrolled down 100 pixels
      gotop::use_gotop( # add it inside the ui
        src = "fas fa-chevron-circle-up", # css class from Font Awesome
        opacity = 0.8, # transparency
        width = 60, # size
        appear = 100 # number of pixels before appearance
      ), # ),
      # adds development banner
      # HTML("<div id='eq-disclaimer-banner' class='padding-1 text-center text-white bg-secondary-dark'><strong>EPA development environment:</strong> The
      # content on this page is not production ready. This site is being used
      # for <strong>development</strong> and/or <strong>testing</strong> purposes
      # only.</div>"),

      # adds epa header html from here: https://www.epa.gov/themes/epa_theme/pattern-lab/patterns/pages-standalone-template/pages-standalone-template.rendered.html
      shiny::includeHTML(app_sys("app/www/header.html")),
      shinyjs::useShinyjs(),
      shinyjs::inlineCSS(css),
      htmltools::br(),
      shiny::headerPanel(title = "Tools for Automated Data Analysis (TADA) Module 1: Water Quality Portal Data Discovery and Cleaning"),
      htmltools::br(),
      shiny::tabsetPanel( # create a navbar page with tabs at the top
        id = "tabbar",
        shiny::tabPanel("1. Load",
          value = "Load", # each tabPanel represents a tab page at the top of the navbar
          htmltools::br(),
          mod_query_data_ui("query_data_1")
        ), # ,
        shiny::tabPanel("2. Overview",
          value = "Overview",
          htmltools::br(),
          mod_overview_ui("overview_1")
        ),
        shiny::tabPanel("3. Flag",
          value = "Flag",
          htmltools::br(),
          mod_data_flagging_ui("data_flagging_1")
        ),
        shiny::tabPanel("4. Filter",
          value = "Filter",
          htmltools::br(),
          mod_filtering_ui("filtering_1")
        ),
        shiny::tabPanel("5. Censored Data",
          value = "Censored",
          htmltools::br(),
          mod_censored_data_ui("censored_data_1")
        ),
        shiny::tabPanel("6. Harmonize and Calculate",
          value = "Harmonize",
          htmltools::br(),
          mod_harmonize_np_ui("harmonize_np_1")
        ),
        shiny::tabPanel("7. Explore",
          value = "Figures",
          htmltools::br(),
          mod_figures_ui("figures_1")
        ),
        shiny::tabPanel("8. Review",
          value = "Review",
          htmltools::br(),
          mod_review_data_ui("review_data_1")
        )
      ),
      htmltools::hr(),
      # adds 'TADA Working Summary and download buttons above the app footer
      mod_TADA_summary_ui("TADA_summary_1"),
      # adds epa footer html
      shiny::includeHTML(app_sys("app/www/footer.html"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
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
