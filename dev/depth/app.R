# app.R - EPATADA calls moved into server (no sourcing of depthProfileTest.R)
library(shiny)
library(EPATADA)
library(dplyr)
library(ggplot2)
library(plotly)

fixed_port <- 6150

# Requested defaults
default_start_date <- "2018-07-07"
default_end_date <- "2018-07-09"
default_siteid <- "REDLAKE_WQX-LRC"
default_characteristics <- c(
  'TEMPERATURE, WATER_NA_NA_DEG C',
  'DEPTH, SECCHI DISK DEPTH_NA_NA_M',
  'DISSOLVED OXYGEN (DO)_NA_NA_MG/L'
)

rv <- reactiveValues(
  df0 = NULL,   # raw retrieval
  df1 = NULL,   # flagged/categorized
  df2 = NULL,   # id combos
  loaded = FALSE,
  last_plot = NULL,
  last_plot_class = NULL,
  last_plot_names = NULL
)

ui <- fluidPage(
  titlePanel("TADA Depth Profile Viewer (EPATADA calls in server)"),

  sidebarLayout(
    sidebarPanel(
      # Date & site inputs are used by TADA_DataRetrieval when "Load data" pressed
      dateInput("start_date", "Start date", value = default_start_date),
      dateInput("end_date",   "End date",   value = default_end_date),
      textInput("siteid", "Site ID (siteid)", value = default_siteid),

      tags$hr(),
      numericInput("surfacevalue", "surfacevalue (used by TADA_FlagDepthCategory)", value = 2, min = 0),
      numericInput("bottomvalue",  "bottomvalue (used by TADA_FlagDepthCategory)", value = 2, min = 0),
      selectInput("unit", "Unit (for plotting)", choices = c("m","ft","in"), selected = "m"),

      actionButton("load_data", "Load data (run TADA_DataRetrieval + processing)", icon = icon("download")),
      tags$hr(),

      helpText("After loading, pick an activity date and up to 3 characteristics, then click Update plot."),

      selectInput("activity_date", "Activity date", choices = NULL),
      selectizeInput("characters", "Up to 3 characteristics", choices = NULL, multiple = TRUE, options = list(maxItems = 3)),
      checkboxInput("depthcat", "Show depth category (depthcat = TRUE)", value = TRUE),
      actionButton("update", "Update plot", icon = icon("chart-area"))
    ),

    mainPanel(
      uiOutput("load_status"),
      plotlyOutput("depthPlotly", height = "650px"),
      verbatimTextOutput("debug_text")
    )
  )
)

server <- function(input, output, session) {

  output$load_status <- renderUI({
    if (!rv$loaded) {
      tagList(tags$b("Data not loaded"), p("Set dates and site, then click Load data."))
    } else {
      tagList(tags$b("Data loaded"),
              p(sprintf("df0 rows: %s | df1 rows: %s | df2 rows: %s",
                        ifelse(is.null(rv$df0), "NULL", nrow(rv$df0)),
                        ifelse(is.null(rv$df1), "NULL", nrow(rv$df1)),
                        ifelse(is.null(rv$df2), "NULL", nrow(rv$df2))))
      )
    }
  })

  # Load and process data when user clicks load_data
  observeEvent(input$load_data, {
    # basic validation
    if (is.na(as.Date(input$start_date)) || is.na(as.Date(input$end_date))) {
      showModal(modalDialog(title = "Invalid dates", "Please provide valid start and end dates.", easyClose = TRUE))
      return()
    }
    if (as.Date(input$start_date) > as.Date(input$end_date)) {
      showModal(modalDialog(title = "Invalid date range", "Start date must be <= end date.", easyClose = TRUE))
      return()
    }
    if (nzchar(input$siteid) == FALSE) {
      showModal(modalDialog(title = "No site ID", "Please enter a site ID.", easyClose = TRUE))
      return()
    }

    withProgress(message = "Retrieving and processing data", value = 0, {
      incProgress(0.1, detail = "Retrieving data (TADA_DataRetrieval)")
      # TADA_DataRetrieval: use the siteid and dates from UI
      df0 <- tryCatch({
        EPATADA::TADA_DataRetrieval(siteid = input$siteid,
                                   startDate = as.character(input$start_date),
                                   endDate = as.character(input$end_date),
                                  ask=FALSE)
      }, error = function(e) {
        showModal(modalDialog(title = "Data retrieval error", paste0("TADA_DataRetrieval failed: ", e$message), easyClose = TRUE))
        return(NULL)
      })
      if (is.null(df0)) return()
      rv$df0 <- df0

      incProgress(0.45, detail = "Flagging depth categories (TADA_FlagDepthCategory)")
      # run TADA_FlagDepthCategory with user-supplied surface/bottom values
      df1 <- tryCatch({
        EPATADA::TADA_FlagDepthCategory(df0,
                                        bycategory = "no",
                                        bottomvalue = input$bottomvalue,
                                        surfacevalue = input$surfacevalue,
                                        dailyagg = "none",
                                        clean = FALSE)
      }, error = function(e) {
        showModal(modalDialog(title = "Depth categorization error", paste0("TADA_FlagDepthCategory failed: ", e$message), easyClose = TRUE))
        return(NULL)
      })
      if (is.null(df1)) return()
      rv$df1 <- df1

      incProgress(0.75, detail = "Computing ID combos (TADA_IDDepthProfiles)")
      # compute list of available locations/dates/characteristics
      df2 <- tryCatch({
        EPATADA::TADA_IDDepthProfiles(df1, nresults = TRUE, nvalue = 2, aggregates = FALSE)
      }, error = function(e) {
        showModal(modalDialog(title = "IDDepthProfiles error", paste0("TADA_IDDepthProfiles failed: ", e$message), easyClose = TRUE))
        return(NULL)
      })
      if (is.null(df2)) return()
      rv$df2 <- df2

      incProgress(0.95, detail = "Updating UI choices")

      # Populate choices:
      # activity_date candidates
      activity_choices <- NULL
      char_choices <- NULL

      if (!is.null(rv$df2)) {
        # try common column names
        if ("ActivityStartDate" %in% names(rv$df2)) activity_choices <- sort(unique(as.character(rv$df2$ActivityStartDate)))
        if ("activity_date" %in% names(rv$df2)) activity_choices <- sort(unique(as.character(rv$df2$activity_date)))
        # characteristics
        if ("TADA.ComparableDataIdentifier" %in% names(rv$df2)) char_choices <- sort(unique(as.character(rv$df2$TADA.ComparableDataIdentifier)))
        if (is.null(char_choices) && "ComparableDataIdentifier" %in% names(rv$df2)) char_choices <- sort(unique(as.character(rv$df2$ComparableDataIdentifier)))
      }
      # fallback to df1 if df2 missing
      if (is.null(activity_choices) && !is.null(rv$df1)) {
        possible_date_cols <- intersect(c("ActivityStartDate","activity_date","ActivityStartDateTime","date"), names(rv$df1))
        if (length(possible_date_cols)>0) activity_choices <- sort(unique(as.character(rv$df1[[possible_date_cols[1]]])))
      }
      if (is.null(char_choices) && !is.null(rv$df1)) {
        possible_char_cols <- intersect(c("TADA.ComparableDataIdentifier","ComparableDataIdentifier","characteristic_name","characteristic"), names(rv$df1))
        if (length(possible_char_cols)>0) char_choices <- sort(unique(as.character(rv$df1[[possible_char_cols[1]]])))
      }

      # Ensure we have a character vector
      if (is.null(activity_choices)) activity_choices <- character(0)
      if (is.null(char_choices)) char_choices <- character(0)

      # Select default characteristics from your requested list when available
      sel_chars <- intersect(default_characteristics, char_choices)
      if (length(sel_chars) == 0 && length(char_choices)>0) sel_chars <- head(char_choices, 3)

      updateSelectInput(session, "activity_date", choices = activity_choices, selected = ifelse(length(activity_choices)>0, activity_choices[1], NA))
      updateSelectizeInput(session, "characters", choices = char_choices, selected = sel_chars)

      rv$loaded <- TRUE
      incProgress(1, detail = "Done")
    })
  })

  # Build the plot when Update pressed
  depth_plot_obj <- eventReactive(input$update, {
    if (!rv$loaded || is.null(rv$df1)) {
      showModal(modalDialog(title = "Data not loaded", "Please click Load data first.", easyClose = TRUE))
      return(NULL)
    }

    chars <- input$characters
    if (is.null(chars) || length(chars) == 0) {
      chars <- intersect(default_characteristics, unique(rv$df1$TADA.ComparableDataIdentifier %||% character(0)))
      if (length(chars)==0) {
        showModal(modalDialog(title = "No characteristic", "Please select at least one characteristic.", easyClose = TRUE))
        return(NULL)
      }
    }

    p <- tryCatch({
      EPATADA::TADA_DepthProfilePlot(rv$df1,
                                    groups = chars,
                                    location = input$siteid, # or input$location if you prefer
                                    activity_date = input$activity_date,
                                    depthcat = input$depthcat,
                                    surfacevalue = input$surfacevalue,
                                    bottomvalue = input$bottomvalue,
                                    unit = input$unit)
    }, error = function(e) {
      showModal(modalDialog(title = "Plot error", paste0("TADA_DepthProfilePlot failed: ", e$message), easyClose = TRUE))
      return(NULL)
    })

    # store for debug
    rv$last_plot <- p
    rv$last_plot_class <- if (!is.null(p)) paste(class(p), collapse = ", ") else ""
    rv$last_plot_names <- if (is.list(p)) paste(names(p), collapse = ", ") else ""
    p
  })

  # Render plotly/htmlwidget or ggplot fallback
  output$depthPlotly <- renderPlotly({
    p <- depth_plot_obj()
    req(p)

    if (inherits(p, "plotly") || inherits(p, "htmlwidget")) {
      return(p)
    }
    if (inherits(p, "ggplot")) {
      return(ggplotly(p))
    }
    if (is.list(p)) {
      if (!is.null(p$plot) && inherits(p$plot, "ggplot")) return(ggplotly(p$plot))
      if (!is.null(p$ggplot) && inherits(p$ggplot, "ggplot")) return(ggplotly(p$ggplot))

      # numeric fallback: first two numeric members
      numeric_members <- Filter(function(x) is.numeric(x) || is.integer(x), p)
      if (length(numeric_members) >= 2) {
        df <- data.frame(x = numeric_members[[1]], y = numeric_members[[2]])
        return(plot_ly(df, x = ~x, y = ~y, type = 'scatter', mode = 'markers'))
      }
    }

    showModal(modalDialog(title = "Plot render error",
                          paste0("Unrecognized plot object structure (class: ", rv$last_plot_class, "). See debug panel."),
                          easyClose = TRUE))
    stop("Unrecognized plot object structure.")
  })

  output$debug_text <- renderText({
    paste0(
      "loaded: ", rv$loaded, "\n",
      "df0 rows: ", ifelse(is.null(rv$df0), "NULL", nrow(rv$df0)), "\n",
      "df1 rows: ", ifelse(is.null(rv$df1), "NULL", nrow(rv$df1)), "\n",
      "df2 rows: ", ifelse(is.null(rv$df2), "NULL", nrow(rv$df2)), "\n",
      "last plot class: ", ifelse(is.null(rv$last_plot_class), "NULL", rv$last_plot_class), "\n",
      "last plot names: ", ifelse(is.null(rv$last_plot_names), "NULL", rv$last_plot_names)
    )
  })
}

app <- shinyApp(ui = ui, server = server)

if (interactive()) {
  options(shiny.launch.browser = TRUE)
  shiny::runApp(app, launch.browser = TRUE, port = fixed_port)
}