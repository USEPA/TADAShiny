# app.R - EPATADA calls moved into server (no sourcing of depthProfileTest.R)
library(shiny)
library(EPATADA)
library(dplyr)
library(ggplot2)
library(plotly)

fixed_port <- 6150

# Requested defaults
default_start_date <- "2018-07-07"
default_end_date <- "2019-07-09"
default_siteid <- "REDLAKE_WQX-LRC"
default_organizations <-  c("REDLAKE_WQX",
                   "SFNOES_WQX",
                   "PUEBLO_POJOAQUE",
                   "FONDULAC_WQX",
                   "PUEBLOOFTESUQUE", "CNENVSER")
default_organization <-  c("REDLAKE_WQX", "SFNOES_WQX")
# default_characteristics <- c(
#   'TEMPERATURE, WATER_NA_NA_DEG C',
#   'DEPTH, SECCHI DISK DEPTH_NA_NA_M',
#   'DISSOLVED OXYGEN (DO)_NA_NA_MG/L'
# )
default_characteristics <- c(0)

tada_rv <- reactiveValues(
  input_raw_df = NULL,   # raw retrieval
  depth_categorized_df = NULL,   # flagged/categorized
  site_date_char_groups_df = NULL,   # id combos
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
      shiny::fluidRow(
        column(
          5,
          shiny::dateInput("start_date", "Start date", value = default_start_date)
        ),
        column(
          5,
          shiny::dateInput("end_date",   "End date",   value = default_end_date)
        )
      ),
      textInput("siteid", "Site ID (siteid)", value = default_siteid),
      selectizeInput("organizations", 
                     "Up to 6 organizations", 
                     choices = default_organizations, 
                     multiple = TRUE, 
                     selected = default_organization,
                     options = list(maxItems = 3, plugins = list("remove_button"))
      ),


      actionButton("load_data", "Load data (run TADA_DataRetrieval + processing)", icon = icon("download")),
      tags$hr(),

      helpText("After loading, pick an activity date and up to 3 characteristics, then click Update plot."),

      shiny::fluidRow(
        column(
          5,
          shiny::selectInput("found_site_id", "Downloaded Site ID", choices = NULL)
        ),
        column(
          5,
          shiny::selectInput("activity_date", "Activity date", choices = NULL)
        )
      ),
      selectizeInput("characteristics", 
                     "Up to 3 characteristics", 
                     choices = NULL, 
                     multiple = TRUE, 
                     options = list(maxItems = 3, plugins = list("remove_button"))
      ),
      
      tags$hr(),
      checkboxInput("depthcat", "Show depth category lines in plot", value = TRUE),
      shiny::fluidRow(
        column(
          5,
          shiny::numericInput("surfacevalue", "Surface (depth below surface)", value = 2, min = 0)
        ),
        column(
          5,
          shiny::numericInput("bottomvalue",  "Bottom (height above bottom) (m)", value = 2, min = 0)
        ),
        # column(
        #   2,
        #   shiny::selectInput("unit", "Unit (for plotting)", choices = c("m","ft","in"), selected = "m")
        # )
      ),      
      # numericInput("surfacevalue", "Surface depth (m) (used in plot)", value = 2, min = 0),
      # numericInput("bottomvalue",  "Bottom depth (m) (used in plot)", value = 2, min = 0),
      # ,      
      actionButton("update", "Update plot", icon = icon("chart-area"))
    ),

    mainPanel(
      # uiOutput("load_status"),
      plotly::plotlyOutput("depthPlotly", height = "650px"),
      verbatimTextOutput("debug_text")
    )
  )
)

server <- function(input, output, session) {

  # Create a reactiveValues object to hold shared data between modules
  tadat <- shiny::reactiveValues()
  
  # output$load_status <- renderUI({
  #   if (!tada_rv$loaded) {
  #     tagList(tags$b("Data not loaded"), p("Set dates and site, then click Load data."))
  #   } else {
  #     tagList(tags$b("Data loaded"),
  #             p(sprintf("input_raw_df rows: %s | depth_categorized_df rows: %s | site_date_char_groups_df rows: %s",
  #                       ifelse(is.null(tada_rv$input_raw_df), "NULL", nrow(tada_rv$input_raw_df)),
  #                       ifelse(is.null(tada_rv$depth_categorized_df), "NULL", nrow(tada_rv$depth_categorized_df)),
  #                       ifelse(is.null(tada_rv$site_date_char_groups_df), "NULL", nrow(tada_rv$site_date_char_groups_df))))
  #     )
  #   }
  # })

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
      input_raw_df <- tryCatch({
        EPATADA::TADA_DataRetrieval(# siteid = input$siteid,
                                    organization = input$organizations,
                                   startDate = as.character(input$start_date),
                                   endDate = as.character(input$end_date),
                                  ask=FALSE)
      }, error = function(e) {
        showModal(modalDialog(title = "Data retrieval error", 
                              paste0("TADA_DataRetrieval failed: ", e$message), 
                              easyClose = TRUE))
        return(NULL)
      })
      if ((is.null(input_raw_df)) || nrow(input_raw_df) == 0) return()
      tada_rv$input_raw_df <- input_raw_df

      incProgress(0.45, detail = "Flagging depth categories (TADA_FlagDepthCategory)")
      
      
      #' TADA_FlagDepthCategory flags depth categories based on user-specified surface and bottom values.
      #'
      #' This function creates a new column, TADA.DepthCategory.Flag with values: "No
      #' depth info", "Surface", "Bottom", and
      #' "Middle" when multiple depths are available.
      #' Categories are: less than 2m (or user specified value) depth = "Surface",
      #' from bottom up to 2m (or user specified value) from bottom = "Bottom", and
      #' all depths in between the Surface and Bottom are assigned to the "Middle"
      #' category.
      
      # run TADA_FlagDepthCategory with user-supplied surface/bottom values
      depth_categorized_df <- tryCatch({
        EPATADA::TADA_FlagDepthCategory(input_raw_df,
                                        bycategory = "no",
                                        bottomvalue = input$bottomvalue,
                                        surfacevalue = input$surfacevalue,
                                        dailyagg = "none",
                                        clean = FALSE)
      }, error = function(e) {
        showModal(modalDialog(title = "Depth categorization error", paste0("TADA_FlagDepthCategory failed: ", e$message), easyClose = TRUE))
        return(NULL)
      })
      
      ############ start additional data cleaning ##################
      # remove any row that does not have a value in the field TADA.ConsolidatedDepth
      depth_categorized_df <- depth_categorized_df[!is.na(depth_categorized_df$TADA.ConsolidatedDepth), , drop = FALSE]
      
      # remove any rows if there are not at least 3 rows with the save value in field TADA.ComparableDataIdentifier
      # to ensure we have enough data for plotting
      depth_categorized_df <- depth_categorized_df %>%
        group_by(TADA.ComparableDataIdentifier) %>%
        filter(n() >= 3) %>%
        ungroup()
      ############### end additional data cleaning ##################
      
      if (is.null(depth_categorized_df)) return()
      tada_rv$depth_categorized_df <- depth_categorized_df

      incProgress(0.75, detail = "Computing ID combos (TADA_IDDepthProfiles)")
      # compute list of available locations/dates/characteristics
      site_date_char_groups_df <- tryCatch({
        EPATADA::TADA_IDDepthProfiles(depth_categorized_df, nresults = TRUE, nvalue = 2, aggregates = FALSE)
      }, error = function(e) {
        showModal(modalDialog(title = "IDDepthProfiles error", paste0("TADA_IDDepthProfiles failed: ", e$message), easyClose = TRUE))
        return(NULL)
      })
      if (is.null(site_date_char_groups_df)) return()
      tada_rv$site_date_char_groups_df <- site_date_char_groups_df

      incProgress(0.95, detail = "Updating UI choices")

      # Populate choices:
      # activity_date candidates
      activity_choices <- NULL
      found_site_id_choices <- NULL
      char_choices <- NULL

      browser()
      if (!is.null(tada_rv$site_date_char_groups_df)) {
        # try common column names
        if ("ActivityStartDate" %in% names(tada_rv$site_date_char_groups_df)) activity_choices <- sort(unique(as.character(tada_rv$site_date_char_groups_df$ActivityStartDate)))

        if ("TADA.MonitoringLocationIdentifier" %in% names(tada_rv$site_date_char_groups_df)) { 
          found_site_id_choices <- sort(unique(as.character(tada_rv$site_date_char_groups_df$TADA.MonitoringLocationIdentifier)))
        }        
        # characteristics
        if ("TADA.ComparableDataIdentifier" %in% names(tada_rv$site_date_char_groups_df)) 
          char_choices <- sort(unique(as.character(tada_rv$site_date_char_groups_df$TADA.ComparableDataIdentifier)))
        if (is.null(char_choices) && "ComparableDataIdentifier" %in% names(tada_rv$site_date_char_groups_df)) { 
          char_choices <- sort(unique(as.character(tada_rv$site_date_char_groups_df$ComparableDataIdentifier)))
        }
      }
      # fallback to depth_categorized_df if site_date_char_groups_df missing
      if (is.null(activity_choices) && !is.null(tada_rv$depth_categorized_df)) {
        possible_date_cols <- intersect(c("ActivityStartDate","activity_date","ActivityStartDateTime","date"), names(tada_rv$depth_categorized_df))
        if (length(possible_date_cols)>0) activity_choices <- sort(unique(as.character(tada_rv$depth_categorized_df[[possible_date_cols[1]]])))
      }
      if (is.null(char_choices) && !is.null(tada_rv$depth_categorized_df)) {
        possible_char_cols <- intersect(c("TADA.ComparableDataIdentifier","ComparableDataIdentifier","characteristic_name","characteristic"), names(tada_rv$depth_categorized_df))
        if (length(possible_char_cols)>0) char_choices <- sort(unique(as.character(tada_rv$depth_categorized_df[[possible_char_cols[1]]])))
      }

      # Ensure we have a character vector
      if (is.null(found_site_id_choices)) found_site_id_choices <- character(0)
      if (is.null(activity_choices)) activity_choices <- character(0)
      if (is.null(char_choices)) char_choices <- character(0)

      # Select default characteristics from your requested list when available
      selected_characteristics <- intersect(default_characteristics, char_choices)
      if (length(selected_characteristics) == 0 && length(char_choices) > 0) {
        selected_characteristics <- head(char_choices, 3)
      }

      updateSelectInput(session, "found_site_id", 
                        choices = found_site_id_choices, 
                        selected = ifelse(length(found_site_id_choices)>0, found_site_id_choices[1], NA))
            
      updateSelectInput(session, "activity_date", 
                        choices = activity_choices, 
                        selected = ifelse(length(activity_choices)>0, activity_choices[1], NA))
      
      updateSelectizeInput(session, "characteristics", 
                           choices = char_choices, 
                           selected = selected_characteristics)

      tada_rv$loaded <- TRUE
      incProgress(1, detail = "Done")
    })
  })

  # When activity_date changes, update the characteristics choices to only those available on that date
  observeEvent(input$activity_date, {
    # only act after data has been loaded
    req(tada_rv$loaded)
    req(input$activity_date)

    # prefer site_date_char_groups_df, fallback to depth_categorized_df
    df <- tada_rv$site_date_char_groups_df
    if (is.null(df)) df <- tada_rv$depth_categorized_df
    if (is.null(df)) return()

    # detect date and characteristic column names (use first matching)
    date_col <- intersect(c("ActivityStartDate","activity_date","ActivityStartDateTime","date"), names(df))[1]
    char_col <- intersect(c("TADA.ComparableDataIdentifier","ComparableDataIdentifier","characteristic_name","characteristic"), names(df))[1]
    if (is.na(date_col) || is.na(char_col)) return()

    # filter rows matching the selected activity_date (compare as character to avoid class issues)
    df_sel <- df[as.character(df[[date_col]]) == as.character(input$activity_date), , drop = FALSE]

    # build sorted unique choices
    choices <- if (nrow(df_sel) > 0) sort(unique(as.character(df_sel[[char_col]]))) else character(0)

    # keep any currently selected characteristics that are still valid
    current_sel <- input$characteristics %||% character(0)
    new_selected <- intersect(current_sel, choices)

    # if none of the previous selections remain, pick up to 3 defaults from choices
    if (length(new_selected) == 0 && length(choices) > 0) {
      new_selected <- head(choices, 3)
    }

    updateSelectizeInput(session, "characteristics",
                         choices = choices,
                         selected = new_selected)
  }, ignoreNULL = TRUE)
  
  # helper to return an informative, safe plotly object when there's no plot
  safe_message_plot <- function(msg) {
    plotly::plot_ly() %>%
      plotly::layout(
        title = msg,
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(
          list(
            text = msg,
            x = 0.5, xref = "paper", xanchor = "center",
            y = 0.5, yref = "paper", yanchor = "middle",
            showarrow = FALSE, font = list(size = 14)
          )
        )
      )
  }
  
  # Build the plot when Update pressed (robust to insufficient data)
  depth_plot_obj <- eventReactive(input$update, {
    if (!tada_rv$loaded || is.null(tada_rv$depth_categorized_df)) {
      showModal(modalDialog(title = "Data not loaded", "Please click Load data first.", easyClose = TRUE))
      return(NULL)
    }

    # validate selected characteristics
    characteristics <- input$characteristics
    if (is.null(characteristics) || length(characteristics) == 0) {
      characteristics <- intersect(default_characteristics,
                                  unique(tada_rv$depth_categorized_df$TADA.ComparableDataIdentifier %||% character(0)))
      if (length(characteristics) == 0) {
        return(safe_message_plot("No characteristic selected or available."))
      }
    }

    # determine date & characteristic column names (reuse same heuristics)
    df <- tada_rv$depth_categorized_df
    date_col <- intersect(c("ActivityStartDate", "activity_date", "ActivityStartDateTime", "date"), names(df))[1]
    char_col <- intersect(c("TADA.ComparableDataIdentifier", "ComparableDataIdentifier", "characteristic_name", "characteristic"), names(df))[1]

    # if we can't find the expected columns, fall back to attempting to call the plotting function
    if (is.na(date_col) || is.na(char_col)) {
      # try calling the plot function but guard it
      p_try <- tryCatch({
        EPATADA::TADA_DepthProfilePlot(tada_rv$depth_categorized_df,
                                      groups = characteristics,
                                      location = input$siteid,
                                      activity_date = input$activity_date,
                                      depthcat = input$depthcat,
                                      surfacevalue = input$surfacevalue,
                                      bottomvalue = input$bottomvalue,
                                      # unit = input$unit
                                      )
      }, error = function(e) {
        return(safe_message_plot(paste0("Plot error: ", e$message)))
      })
      # p_try will either be a plotly/ggplot or the safe_message_plot object
      tada_rv$last_plot <- p_try
      return(p_try)
    }

    # filter to rows for the requested date and selected characteristics
    sel_rows <- df[as.character(df[[date_col]]) == as.character(input$activity_date) &
                   as.character(df[[char_col]]) %in% as.character(characteristics), , drop = FALSE]
    # browser()
    # define a minimal threshold for "enough" data: adjust as needed (here, at least 2 rows)
    if (nrow(sel_rows) < 2) {
      msg <- sprintf("Not enough data for %s on %s", paste(characteristics, collapse = ", "), as.character(input$activity_date))
      return(safe_message_plot(msg))
    }

    # we have enough data: call the EPATADA plotting function safely
    p <- tryCatch({
      EPATADA::TADA_DepthProfilePlot(tada_rv$depth_categorized_df,
                                    groups = characteristics,
                                    location = input$siteid,
                                    activity_date = input$activity_date,
                                    depthcat = input$depthcat,
                                    surfacevalue = input$surfacevalue,
                                    bottomvalue = input$bottomvalue,
                                    # unit = input$unit
                                    )
    }, error = function(e) {
      return(safe_message_plot(paste0("Plot error: ", e$message)))
    })

    # normalize possible returned structures:
    # - if it's a ggplot, return as-is
    # - if plotly/htmlwidget, return as-is
    # - if it's a list with a ggplot element, extract it
    # - if it's a list with numeric vectors, unname them and build a simple plotly scatter
    if (is.null(p)) {
      return(safe_message_plot("Plot function returned NULL."))
    }

    # store for debug
    tada_rv$last_plot <- p
    tada_rv$last_plot_class <- paste(class(p), collapse = ", ")
    tada_rv$last_plot_names <- if (is.list(p)) paste(names(p), collapse = ", ") else ""

    # If it's already a plotly/htmlwidget or ggplot, return (renderPlotly will handle ggplot)
    if (inherits(p, "plotly") || inherits(p, "htmlwidget") || inherits(p, "ggplot")) {
      return(p)
    }

    # If list with ggplot element
    if (is.list(p)) {
      if (!is.null(p$plot) && inherits(p$plot, "ggplot")) return(p$plot)
      if (!is.null(p$ggplot) && inherits(p$ggplot, "ggplot")) return(p$ggplot)

      # If there is a data.frame in the list, prefer first data.frame
      df_member <- Filter(is.data.frame, p)
      if (length(df_member) > 0) {
        # use first data.frame and attempt a sensible plot
        dfp <- df_member[[1]]
        numeric_cols <- names(Filter(function(x) is.numeric(x) || is.integer(x), dfp))
        if (length(numeric_cols) >= 2) {
          xcol <- numeric_cols[1]; ycol <- numeric_cols[2]
          # unname columns if they are named vectors
          dfp[[xcol]] <- unname(as.numeric(dfp[[xcol]]))
          dfp[[ycol]] <- unname(as.numeric(dfp[[ycol]]))
          return(plot_ly(dfp, x = as.formula(paste0("~`", xcol, "`")), y = as.formula(paste0("~`", ycol, "`")), type = 'scatter', mode = 'markers'))
        }
      }

      # numeric vector fallback (unnaming to avoid asJSON error)
      numeric_members <- Filter(function(x) is.numeric(x) || is.integer(x), p)
      if (length(numeric_members) >= 2) {
        x_vec <- unname(as.numeric(numeric_members[[1]]))
        y_vec <- unname(as.numeric(numeric_members[[2]]))
        if (length(x_vec) >= 2 && length(y_vec) >= 2) {
          df_simple <- data.frame(x = x_vec, y = y_vec, stringsAsFactors = FALSE)
          return(plot_ly(df_simple, x = ~x, y = ~y, type = 'scatter', mode = 'markers'))
        }
      }
    }

    # if we reach here, we don't know how to render p
    safe_message_plot(sprintf("Unrecognized plot object (class: %s).", tada_rv$last_plot_class))
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

    stop("Unrecognized plot object structure.")
  })

  output$debug_text <- renderText({
    paste0(
      "loaded: ", tada_rv$loaded, "\n",
      "input_raw_df rows: ", ifelse(is.null(tada_rv$input_raw_df), "NULL", nrow(tada_rv$input_raw_df)), "\n",
      "depth_categorized_df rows: ", ifelse(is.null(tada_rv$depth_categorized_df), "NULL", 
                                            nrow(tada_rv$depth_categorized_df)), "\n",
      "site_date_char_groups_df rows: ", ifelse(is.null(tada_rv$site_date_char_groups_df), "NULL", 
                                                nrow(tada_rv$site_date_char_groups_df))
    )
  })
}

app <- shinyApp(ui = ui, server = server)

if (interactive()) {
  options(shiny.launch.browser = TRUE)
  shiny::runApp(app, launch.browser = TRUE, port = fixed_port)
}