#' depth UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#'
#'
depth_profile <- shiny::reactiveValues(
  depth_categorized_df = NULL,
  site_date_char_groups_df = NULL,
  site_date_pairs = NULL,
  available_characteristics_df = NULL,
  loaded = FALSE,
  no_data = FALSE
)

mod_depth_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    htmltools::h3("1. Determine if there is sufficient Depth Profile data"),
    htmltools::HTML("First, hit the 'Review Depth Data' button to check if there is sufficient depth profile data
                    to do a Depth (water column depth) analysis. If there is sufficient data available,
                    the map will populate with sites that have depth profile data.
                    Click on a site to see the available visit dates for that site,
                    then select a date to see the characteristics available for plotting."),
    htmltools::div(style = "margin-bottom:10px"),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(ns("review_depth_profile_data"),
        "Review Depth Data",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )),
    # tags$hr(),

    shiny::conditionalPanel(
      condition = paste0("output['", ns("no_depth_profile_data"), "'] == true"),
      # Show only the loading message when data is not loaded

      shiny::div(style = "margin-top:20px; color: #666;font-size: large;font-weight: bolder;",
        "After retrieval and data cleaning, there are no usable water quality records with data collected in a Depth Profile."
      )
    ),
    # Show the rest of the UI only after data is loaded
    shiny::conditionalPanel(
      condition = paste0("output['", ns("depth_profile_loaded"), "'] == true"), # output.depth_profile_loaded == true",
      # Instruction text
      tags$hr(style="margin-top: 10px;"),
      htmltools::h3("2. Select sites, dates, and characteristics to plot."),
      shiny::HTML(paste0("A total of <strong>", shiny::textOutput(ns("number_of_records"), container = tags$span),
        "</strong> results at <strong>",
        shiny::textOutput(ns("number_of_sites"), container = tags$span),
        "</strong> sites are available for plotting Depth Profiles.
        The following data processing steps were performed performed to prepare the data for plotting:
        <ol>
        <li> Selected only sites with Depth profiles (i.e., samples with depth values collected at multiple depths on the same date at the same site),
        <li> Prepared counts of available characteristics for each site/date combination, and
        <li> harmonized result and depth units to TADA defaults, and
        <li> replaced retired characteristic names with current names.
        </ol>")),
      shiny::div(style = "margin-bottom:8px; color: #666;font-size: large;font-weight: bolder;",
        shiny::HTML(paste0("The map shows the ",
          shiny::textOutput(ns("number_of_sites2"), container = tags$span),
          " sites that have data available for plotting Depth profiles.
            After loading: choose a Site ID, then choose a Visit Date.
            Then pick up to 3 characteristics and click 'Update plot'."))
      ),
      # map
      shiny::fluidRow(
        column(12,
          shinycssloaders::withSpinner(leaflet::leafletOutput(
            ns("depth_profile_sites_map"),
            height = "500px")
          )
        )
      ),
      # Site and date selects (same row)
      shiny::fluidRow(class = "control-row", style="width: 50%; padding-top: 10px;",
        column(8,
          shiny::div(style = "display:flex; flex-direction:column; ",
            tags$label("Site ID", `for` = "depth_profile_site_id"),
            shiny::selectInput(ns("depth_profile_site_id"), NULL, choices = NULL, width = "100%")
          )
        ),
        column(4,
          shiny::div(style = "display:flex; flex-direction:column;",
            tags$label("Visit date", `for` = "activity_date"),
            shiny::selectInput(ns("activity_date"), NULL, choices = NULL, width = "100%")
          )
        )
      ),

      shiny::br(),

      # Available characteristics table
      shiny::fluidRow(class = "control-row",
        column(12, DT::DTOutput(ns("available_characteristics")))
      ),

      tags$hr(),

      # Options row: depthcat, surfacevalue, bottomvalue
      shiny::fluidRow(class = "control-row", style="width: 30%;",
        column(4,
          shiny::div(style = "display:flex; flex-direction:column;",
            tags$label("Depth category"),
            shiny::checkboxInput(ns("depthcat"), NULL, value = TRUE)
          )
        ),
        column(4,
          shiny::div(style = "display:flex; flex-direction:column;",
            tags$label("Surface (depth below surface)"),
            shiny::numericInput(ns("surfacevalue"), NULL, value = 2, min = 0, width = "100%")
          )
        ),
        column(4,
          shiny::div(style = "display:flex; flex-direction:column;",
            tags$label("Bottom (height above bottom) (m)"),
            shiny::numericInput(ns("bottomvalue"), NULL, value = 2, min = 0, width = "100%")
          )
        )
      ),

      # Update button full-width
      shiny::fluidRow(class = "control-row",
        column(12, shiny::actionButton(ns("update"),
          "Update plot",
          icon = shiny::icon("chart-area"), class = "btn btn-primary", style = "width:10%; padding-top: 10px;"))
      ),
      # Middle: the plot (map) — use viewport height so it fills much of the window
      shiny::fluidRow(
        column(width = 12,
          # 70vh => 70% of viewport height; adjust to taste
          plotly::plotlyOutput(ns("depthPlotly"), height = "70vh")
        )
      ),
      # Middle: the data table of the data shown in the plot
      shiny::fluidRow(
        column(width = 12,
          shiny::div(style = "width:70%; margin-left: auto; margin-right: auto; overflow-x: auto;",
            column(12, DT::DTOutput(ns("depth_plot_data_table"))
            )
        ))
      ),


      # Bottom: debug information occupying full width
      shiny::fluidRow(
        column(width = 12,
          tags$hr(),
          shiny::verbatimTextOutput(ns("debug_text"))
        )
      )
    )
  )
}


# --- Depth module helper functions (module-level so they are testable) ---

# Helper to split semicolon-separated characteristic lists (robust)
split_characteristics <- function(vec) {
  vec <- stats::na.omit(as.character(vec))
  if (length(vec) == 0) return(character(0))
  parts <- unlist(strsplit(vec, ";", fixed = TRUE))
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  sort(unique(parts))
}

# Normalize tokens: trim and remove trailing " (digits)" only
normalize_token <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  sub(" \\(\\d+\\)$", "", x)
}

# Normalize tokens: remove _NONE_NONE_ sequence if needed
normalize_NA_token <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  sub("_NONE_NONE_", " ", x)
}

# Extract trailing numeric count from token strings like "TOKEN (5)". Returns NA if none.
extract_trailing_count <- function(token) {
  token <- as.character(token)
  m <- regexec(".*?\\((\\d+)\\)\\s*$", token)
  r <- regmatches(token, m)
  if (length(r) == 0 || length(r[[1]]) < 2) return(NA_integer_)
  val <- as.integer(r[[1]][2])
  if (is.na(val)) NA_integer_ else val
}


#' depth Server Functions
#'
#' @noRd
mod_depth_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preferred defaults (priority order)
    preferred_characteristics <- c(
      "TEMPERATURE, WATER_NONE_NONE_DEG C",
      "DISSOLVED OXYGEN (DO)_NONE_NONE_MG/L",
      "CONDUCTIVITY_NONE_NONE_US/CM",
      "PH_NONE_NONE_NONE"
    )


    # Safe message for plot area (plotly)
    safe_message_plot <- function(msg) {
      plotly::plot_ly() %>%
        plotly::layout(
          title = msg,
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(text = msg,
            x = 0.5, xref = "paper", xanchor = "center",
            y = 0.5, yref = "paper", yanchor = "middle",
            showarrow = FALSE, font = list(size = 14)))
        )
    }

    # this a reactive list created to hold all the reactive objects specific to this module.
    mapdat <- shiny::reactiveValues()

    # expose the loaded flag to conditionalPanel in UI to show a UI elements that says there is no depth profile data
    output$no_depth_profile_data <- shiny::reactive({ isTRUE(depth_profile$no_data) })
    # ensure the output is not suspended when hidden, so UI sees the change
    shiny::outputOptions(output, "no_depth_profile_data", suspendWhenHidden = FALSE)

    # ibid
    output$depth_profile_loaded <- shiny::reactive({ isTRUE(depth_profile$loaded) })
    # ibid
    shiny::outputOptions(output, "depth_profile_loaded", suspendWhenHidden = FALSE)

    react <- shiny::reactiveValues() # create a reactive values object to hold vectors, dataframes, etc. that you want to use throughout the app.
    # NOTE: react is used across multiple UI/observer blocks to share computed datasets and choices.

    # This grabs the column names of tadat$raw when user first clicks on tadat$tab, so grouping col dropdown doesn't have the 'group' column created in this tab.
    shiny::observeEvent(tadat$tab, {
      if (tadat$tab == "Figures") {
        react$names <- names(tadat$raw)
        # TIP: If you later preprocess tadat$raw (e.g., rename columns), update this hook to reflect new names.
      }
    })

    output$depth_review_table_title <- shiny::renderText({
      if (is.null(tadat$raw) || dim(tadat$raw)[1] < 1) {
        return(NULL)
      } else {
        "Depth Data Summary"
      }
    })

    # 'Review Depth Data' button click
    shiny::observeEvent(input$review_depth_profile_data, {
      # if TADA.Remove column exists, filter out records where TADA.Remove == TRUE;
      # if it doesn't exist, keep all records (don't filter)
      # Ensure required data is present
      shiny::req(tadat$raw)

      # a modal that pops up showing it's working on loading the data
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = tagList(
          tags$div(
            tags$p("Checking for Depth Profile data", tags$br(), input$example_data),
            style = "text-align:center; padding: 12px;",
            tags$p(id = "js_time_display", "00:00:00")
          ),
          # Hidden input to hold elapsed seconds for server (JS updates it)
          tags$input(id = "js_elapsed_seconds", type = "hidden", value = "0")
        ),
        session = shiny::getDefaultReactiveDomain()
      )

      # Keep rows only when TADA.Remove is explicitly FALSE (no NA expected)
      input_raw_df <- if ("TADA.Remove" %in% names(tadat$raw)) {
        df <- tadat$raw

        # Enforce the assumption
        if (anyNA(df[["TADA.Remove"]])) {
          stop("TADA.Remove contains NA; none expected.")
        }

        # Optional: ensure logical type
        if (!is.logical(df[["TADA.Remove"]])) {
          stop("TADA.Remove must be logical (TRUE/FALSE).")
        }

        df[df[["TADA.Remove"]] == FALSE, , drop = FALSE]
      } else {
        tadat$raw
      }

      # shiny::incProgress(0.40, detail = "Flagging depth categories (TADA_FlagDepthCategory)")
      depth_categorized_df <- tryCatch({
        EPATADA::TADA_FlagDepthCategory(input_raw_df,
          bycategory = "no",
          bottomvalue = 2, # input$bottomvalue,
          surfacevalue = 2, # input$surfacevalue,
          dailyagg = "none",
          clean = FALSE)
      }, error = function(e) {
        shiny::showModal(shiny::modalDialog(title = "Depth categorization error",
          paste0("TADA_FlagDepthCategory failed: ", e$message), easyClose = TRUE))
        depth_profile$no_data <- TRUE
        return(NULL)
      })

      # filter out records with NA depth category and groups with < 3 records (per TADA_IDDepthProfiles requirements)
      depth_categorized_df <- depth_categorized_df[!is.na(depth_categorized_df$TADA.ConsolidatedDepth), , drop = FALSE]
      depth_categorized_df <- depth_categorized_df %>%
        dplyr::group_by(TADA.ComparableDataIdentifier) %>%
        dplyr::filter(dplyr::n() >= 3) %>%
        dplyr::ungroup()

      if (is.null(depth_categorized_df) || nrow(depth_categorized_df) == 0) {
        shiny::showModal(shiny::modalDialog(title = "No usable records",
          "No usable depth records after cleaning.",
          easyClose = TRUE))
        depth_profile$no_data <- TRUE
        return()
      }

      site_date_char_groups_df <- tryCatch({
        EPATADA::TADA_IDDepthProfiles(depth_categorized_df,
          nresults = TRUE,
          nvalue = 2,
          aggregates = FALSE)
      }, error = function(e) {
        shiny::showModal(shiny::modalDialog(title = "IDDepthProfiles error",
          paste0("TADA_IDDepthProfiles failed: ", e$message), easyClose = TRUE))
        depth_profile$no_data <- TRUE
        return(NULL)
      })
      if (is.null(site_date_char_groups_df) || nrow(site_date_char_groups_df) == 0) {
        shiny::showModal(shiny::modalDialog(title = "No ID combos",
          "TADA_IDDepthProfiles returned no rows.", easyClose = TRUE))
        depth_profile$no_data <- TRUE
        return()
      }
      depth_profile$site_date_char_groups_df <- site_date_char_groups_df

      # update the depth_categorized_df by removing records that don't
      # have matching TADA.MonitoringLocationIdentifier and ActivityStartDate in site_date_char_groups_df,
      # because those records won't be used in the plot and this way we avoid confusion
      # of showing sites/dates in the map that can't be plotted.

      # NOTE: this is driving me crazy.  Something isn't right and I can't understand it.
      depth_categorized_filtered_df <- depth_categorized_df %>%
        dplyr::inner_join(
          site_date_char_groups_df %>%
            dplyr::select(OrganizationIdentifier, TADA.MonitoringLocationIdentifier, ActivityStartDate) %>%
            dplyr::distinct(),
          by = c("OrganizationIdentifier", "TADA.MonitoringLocationIdentifier", "ActivityStartDate")
        )


      # this is when the map get populated
      # depth_profile$depth_categorized_df <- depth_categorized_df

      # trying this first
      depth_profile$depth_categorized_df <- depth_categorized_filtered_df

      # shiny::incProgress(0.95, detail = "Extracting unique (site, date) pairs")

      # find columns robustly
      loc_col  <- "TADA.MonitoringLocationIdentifier"
      date_col <- "ActivityStartDate"

      pairs_df <- data.frame(
        MonitoringLocationIdentifier =
          if (!is.na(loc_col)) as.character(site_date_char_groups_df[[loc_col]]) else NA_character_,
        ActivityStartDate =
          if (!is.na(date_col)) as.character(site_date_char_groups_df[[date_col]]) else NA_character_,
        stringsAsFactors = FALSE
      )
      pairs_df <- unique(pairs_df)
      suppressWarnings({ dt_parsed <- as.Date(pairs_df$ActivityStartDate) })
      if (!all(is.na(dt_parsed))) {
        pairs_df <- pairs_df[order(dt_parsed, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
      }

      depth_profile$site_date_pairs <- pairs_df
      depth_profile$available_characteristics_df <- data.frame(Characteristic = character(0), stringsAsFactors = FALSE)

      depth_profile$loaded <- TRUE
      depth_profile$no_data <- FALSE

      # Populate the site and date selectInputs
      found_site_id_choices <- if (!is.null(pairs_df) && nrow(pairs_df) > 0) sort(unique(as.character(pairs_df$MonitoringLocationIdentifier))) else character(0)
      all_dates <- if (!is.null(pairs_df) && nrow(pairs_df) > 0) sort(unique(as.character(pairs_df$ActivityStartDate))) else character(0)

      shiny::updateSelectInput(session, "depth_profile_site_id",
        choices = found_site_id_choices,
        selected = if (length(found_site_id_choices) > 0) found_site_id_choices[1] else NA)
      shiny::updateSelectInput(session, "activity_date",
        choices = all_dates,
        selected = if (length(all_dates) > 0) all_dates[1] else NA)

      # shiny::incProgress(1, detail = "Done")
      shinybusy::remove_modal_spinner()

    }
    ) # end shiny::observeEvent(input$review_depth_profile_data, { ... })


    # the leaflet map - shows all sites in the loaded data, with popups of site ID and number of records (if CompID available)
    output$depth_profile_sites_map <- leaflet::renderLeaflet({
      shiny::req(mapdat$text)
      EPATADA::TADA_OverviewMap(depth_profile$depth_categorized_df)
    })

    # create dataset for map and histogram using raw data
    shiny::observe( {
      shiny::req(tadat$raw)
      shiny::req(depth_profile$depth_categorized_df)
      # create gray text tile info
      mapdat$text <- depth_profile$depth_categorized_df %>%
        dplyr::select(
          ResultIdentifier,
          MonitoringLocationIdentifier,
          OrganizationFormalName,
          ActivityStartDate
        )

      # create summary info and binning for map

      # get top 10 characteristics by result number in the dataset and place the rest in a group called "all others"
      chars <- tadat$raw %>%
        # dplyr::filter(TADA.Remove == FALSE) %>%
        dplyr::group_by(TADA.CharacteristicName) %>%
        dplyr::summarise("Result_Count" = length(unique(ResultIdentifier)))

      topslice <-
        chars %>% dplyr::slice_max(order_by = Result_Count, n = 10)

      bottomslice <- chars %>%
        dplyr::ungroup() %>%
        dplyr::filter(!TADA.CharacteristicName %in% topslice$TADA.CharacteristicName) %>%
        dplyr::select("Result_Count") %>%
        dplyr::summarise("Result_Count" = sum(Result_Count)) %>%
        dplyr::mutate("TADA.CharacteristicName" = "ALL OTHERS")

      chars <-
        plyr::rbind.fill(topslice, bottomslice) %>% dplyr::filter(Result_Count > 0)

      chars <-
        chars %>% dplyr::mutate(TADA.Chars = substr(TADA.CharacteristicName, 1, 22))

      chars$TADA.Chars <-
        ifelse(
          nchar(chars$TADA.CharacteristicName) > 22,
          base::paste0(chars$TADA.Chars, "..."),
          chars$TADA.Chars
        )

      chars <-
        chars %>% dplyr::mutate(TADA.Chars = forcats::fct_reorder(TADA.Chars, Result_Count, .desc = TRUE))

      mapdat$chars <- chars
    })

    # Proxy to select rows programmatically
    available_chars_proxy <- DT::dataTableProxy("available_characteristics")

    # Client-side callback to enforce max 3 selections (no Select extension)
    available_chars_callback <- DT::JS(
      paste0("table.on('click', 'tr', function() {",
        "  var clicked = this;",
        "  setTimeout(function(){",
        "    var sel_count = table.$('tr.selected').length;",
        "    if(sel_count > 3){",
        "      $(clicked).removeClass('selected');",
        "      var sel_nodes = table.$('tr.selected');",
        "      var sel_idx = [];",
        "      sel_nodes.each(function(){ sel_idx.push(table.row(this).index() + 1); });",
        "      if(window.Shiny){",
        "        Shiny.setInputValue('", ns("available_characteristics_rows_selected"), "', sel_idx, {priority: 'event'});",
        "        Shiny.setInputValue('", ns("available_chars_overlimit"), "', Math.random(), {priority: 'event'});",
        "      }",
        "    }",
        "  }, 1);",
        "});")
    )

    # Available characteristics table (renderDT + datatable) - display excludes CompID
    output$available_characteristics <- DT::renderDT({
      shiny::req(depth_profile$loaded)
      df_full <- depth_profile$available_characteristics_df
      if (is.null(df_full) || nrow(df_full) == 0) {
        DT::datatable(data.frame(Message = "No available characteristics. Select a Site and Date above."),
          options = list(dom = "t"))
      } else {
        # Prepare display df: include Characteristic, N, Unit only (if Unit exists)
        disp_cols <- c("Characteristic", "N")
        if ("Unit" %in% names(df_full)) disp_cols <- c(disp_cols, "Unit")
        display_df <- df_full[, intersect(disp_cols, names(df_full)), drop = FALSE]

        # # Move this row to the bottom to prevent a EPATADA error
        # target_value <- 'DEPTH, SECCHI DISK DEPTH M'
        #
        # # Split the data frame
        # row_to_move <- display_df[display_df$Characteristic == target_value, ]  # Row to move
        # remaining_rows <- display_df[display_df$Characteristic != target_value, ]  # Other rows
        #
        # # Recombine with the target row at the bottom
        # display_df <- rbind(remaining_rows, row_to_move)

        DT::datatable(
          display_df,
          rownames = FALSE,
          selection = list(mode = "multiple"),
          options = list(pageLength = 6, scrollY = "100%", dom = "t"),
          callback = available_chars_callback
        )
      }
    }, server = FALSE)

    # When user changes selected site, update the activity_date choices
    shiny::observeEvent(input$depth_profile_site_id, {
      shiny::req(depth_profile$loaded)
      site_in <- input$depth_profile_site_id
      if (is.null(site_in) || !nzchar(site_in)) {
        shiny::updateSelectInput(session, "activity_date", choices = character(0), selected = NA)
        return()
      }
      pairs_df <- depth_profile$site_date_pairs
      if (is.null(pairs_df) || nrow(pairs_df) == 0) {
        shiny::updateSelectInput(session, "activity_date", choices = character(0), selected = NA)
        return()
      }
      site_dates <- sort(unique(as.character(pairs_df$ActivityStartDate[pairs_df$MonitoringLocationIdentifier == site_in])))
      shiny::updateSelectInput(session, "activity_date", choices = site_dates, selected = if (length(site_dates) > 0) site_dates[1] else NA)
    })

    # When user selects an activity_date (after site chosen), compute available characteristics
    shiny::observeEvent(input$activity_date, {
      shiny::req(depth_profile$loaded)
      sel_site <- input$depth_profile_site_id
      sel_date <- input$activity_date
      if (is.null(sel_site) || !nzchar(sel_site) || is.null(sel_date) || !nzchar(sel_date)) {
        depth_profile$available_characteristics_df <- data.frame(Characteristic = character(0), stringsAsFactors = FALSE)
        return()
      }

      df <- depth_profile$site_date_char_groups_df
      if (is.null(df)) df <- depth_profile$depth_categorized_df
      if (is.null(df)) {
        depth_profile$available_characteristics_df <- data.frame(Characteristic = character(0), stringsAsFactors = FALSE)
        return()
      }

      loc_col  <- "TADA.MonitoringLocationIdentifier"
      date_col <- "ActivityStartDate"
      char_col <- "TADA.CharacteristicsForDepthProfile"

      if (is.na(loc_col) || is.na(date_col) || is.na(char_col)) {
        depth_profile$available_characteristics_df <- data.frame(Characteristic = character(0), stringsAsFactors = FALSE)
        return()
      }

      df_sel <- df[as.character(df[[loc_col]]) == sel_site & as.character(df[[date_col]]) == sel_date, , drop = FALSE]
      if (nrow(df_sel) == 0) {
        depth_profile$available_characteristics_df <- data.frame(Characteristic = character(0), stringsAsFactors = FALSE)
        return()
      }

      # Extract tokens from char_col and compute N per-token
      raw_vals <- as.character(df_sel[[char_col]])
      char_choices <- split_characteristics(raw_vals)
      if (length(char_choices) == 0) char_choices <- sort(unique(stats::na.omit(raw_vals)))
      if (length(char_choices) == 0) {
        depth_profile$available_characteristics_df <- data.frame(Characteristic = character(0), stringsAsFactors = FALSE)
        return()
      }

      tokens_display <- vapply(char_choices, normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)
      tokens_display <- vapply(tokens_display, normalize_NA_token, FUN.VALUE = character(1), USE.NAMES = FALSE)

      counts <- integer(length(char_choices))
      unit_col_candidates <- names(df_sel)[grepl("unit", names(df_sel), ignore.case = TRUE)]
      unit_col <- if (length(unit_col_candidates) > 0) unit_col_candidates[1] else NA_character_
      units <- rep(NA_character_, length(char_choices))

      for (i in seq_along(char_choices)) {
        ch <- char_choices[i]
        explicit_total <- 0L
        explicit_found <- FALSE
        row_match_flags <- rep(FALSE, nrow(df_sel))

        for (j in seq_len(nrow(df_sel))) {
          pv <- as.character(df_sel[[char_col]][j])
          if (!is.na(pv) && nzchar(pv)) {
            toks <- split_characteristics(pv)
            if (length(toks) > 0) {
              for (tok in toks) {
                if (normalize_token(tok) == normalize_token(ch)) {
                  cnt <- extract_trailing_count(tok)
                  if (!is.na(cnt)) {
                    explicit_total <- explicit_total + cnt
                    explicit_found <- TRUE
                  } else {
                    row_match_flags[j] <- TRUE
                  }
                  break
                }
              }
            }
          }
        }

        if (explicit_found && explicit_total > 0L) {
          counts[i] <- explicit_total
        } else {
          counts[i] <- sum(row_match_flags, na.rm = TRUE)
        }

        if (!is.na(unit_col) && any((!is.na(row_match_flags) & row_match_flags) & !is.na(df_sel[[unit_col]]))) {
          uu <- as.character(df_sel[[unit_col]][row_match_flags])
          uu <- uu[!is.na(uu)]
          if (length(uu) > 0) units[i] <- names(sort(table(uu), decreasing = TRUE))[1]
        }
      }

      df_chars <- data.frame(
        Characteristic = tokens_display,
        N = counts,
        stringsAsFactors = FALSE
      )
      df_chars$CompID <- vapply(char_choices, normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)
      if (!all(is.na(units))) df_chars$Unit <- units

      target_value <- "DEPTH, SECCHI DISK DEPTH M"

      # Split the data frame
      row_to_move <- df_chars[df_chars$Characteristic == target_value, ]  # Row to move
      remaining_rows <- df_chars[df_chars$Characteristic != target_value, ]  # Other rows

      # Recombine with the target row at the bottom
      df_chars <- rbind(remaining_rows, row_to_move)

      depth_profile$available_characteristics_df <- df_chars

      # Pre-select defaults by priority (match normalized preferred_characteristics)
      df_chars_local <- depth_profile$available_characteristics_df
      sel_rows <- integer(0)

      for (pc in preferred_characteristics) {
        pc_clean <- normalize_token(pc)
        idx <- which(df_chars_local$CompID == pc_clean | df_chars_local$Characteristic == pc_clean)
        if (length(idx) > 0) sel_rows <- c(sel_rows, idx[1])
        if (length(sel_rows) >= 3) break()
      }
      if (length(sel_rows) < 3) {
        more_idx <- setdiff(seq_len(nrow(df_chars_local)), sel_rows)
        if (length(more_idx) > 0) sel_rows <- c(sel_rows, utils::head(more_idx, 3 - length(sel_rows)))
      }
      sel_rows <- unique(sel_rows)
      if (length(sel_rows) > 0) {
        # TODO DT::dataTableProxy("filterStep2", session = session)
        try({ DT::selectRows(available_chars_proxy, sel_rows) }, silent = TRUE)
      }

    }, ignoreNULL = TRUE)

    # Notify when client-side over-limit attempted
    shiny::observeEvent(input$available_chars_overlimit, {
      shiny::showNotification("You may select up to 3 characteristics.", type = "warning", duration = 3)
    })

    # Safety trim if >3 selected
    shiny::observeEvent(input$available_characteristics_rows_selected, {
      sel <- input$available_characteristics_rows_selected
      if (is.null(sel) || length(sel) <= 3) return()
      new_sel <- sel[seq_len(3)]
      DT::selectRows(available_chars_proxy, new_sel)
      shiny::showNotification("Selection limited to 3 characteristics (trimmed).", type = "warning", duration = 3)
    }, ignoreNULL = FALSE)

    # Build plot on Update
    depth_plot_obj <- shiny::eventReactive(input$update, {
      if (!depth_profile$loaded || is.null(depth_profile$depth_categorized_df)) {
        shiny::showModal(shiny::modalDialog(title = "Data not loaded", "Please click Load data first.", easyClose = TRUE))
        return(NULL)
      }

      sel_site <- if (!is.null(input$depth_profile_site_id) && nzchar(as.character(input$depth_profile_site_id))) input$depth_profile_site_id else input$siteid
      sel_date <- input$activity_date

      if (is.null(sel_site) || !nzchar(as.character(sel_site)) || is.null(sel_date) || !nzchar(as.character(sel_date))) {
        return(safe_message_plot("Please select Site and Date first."))
      }

      # gather selected characteristics comp IDs
      chars_idx <- input$available_characteristics_rows_selected
      characteristics <- character(0)
      if (!is.null(chars_idx) && length(chars_idx) > 0 &&
        !is.null(depth_profile$available_characteristics_df) && nrow(depth_profile$available_characteristics_df) > 0) {
        if ("CompID" %in% names(depth_profile$available_characteristics_df)) {
          characteristics <- as.character(depth_profile$available_characteristics_df$CompID[chars_idx])
        } else {
          characteristics <- as.character(depth_profile$available_characteristics_df$Characteristic[chars_idx])
        }
      }

      # fallback defaults
      if (length(characteristics) == 0) {
        if (!is.null(depth_profile$available_characteristics_df) && nrow(depth_profile$available_characteristics_df) > 0) {
          if ("CompID" %in% names(depth_profile$available_characteristics_df)) {
            characteristics <- utils::head(as.character(depth_profile$available_characteristics_df$CompID), 3)
          } else {
            characteristics <- utils::head(as.character(depth_profile$available_characteristics_df$Characteristic), 3)
          }
        } else {
          characteristics <- utils::head(unique(depth_profile$depth_categorized_df$TADA.ComparableDataIdentifier %||% character(0)), 3)
          if (length(characteristics) == 0) return(safe_message_plot("No characteristic selected"))
        }
      }

      # determine matching columns
      df_all <- depth_profile$depth_categorized_df
      loc_col  <- "TADA.MonitoringLocationIdentifier"
      date_col <- "ActivityStartDate"
      char_col <- "TADA.ComparableDataIdentifier"

      # filter by site/date
      df_sel <- df_all
      if (!is.na(loc_col) && !is.null(sel_site) && nzchar(as.character(sel_site))) {
        df_sel <- df_sel[as.character(df_sel[[loc_col]]) == as.character(sel_site), , drop = FALSE]
      }
      if (!is.na(date_col) && !is.null(sel_date) && nzchar(as.character(sel_date))) {
        df_sel <- df_sel[as.character(df_sel[[date_col]]) == as.character(sel_date), , drop = FALSE]
      }

      if (nrow(df_sel) == 0) {
        return(safe_message_plot(sprintf("No records for site %s on %s", sel_site %||% "<none>", sel_date %||% "<none>")))
      }

      # normalize selected tokens
      characteristics_norm <- vapply(characteristics, normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)

      # prepare df_sel normalized copy for char_col
      df_sel_norm <- df_sel
      if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
        df_sel_norm[[char_col]] <- vapply(as.character(df_sel_norm[[char_col]]), normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)
      }

      keep_idx <- rep(FALSE, nrow(df_sel_norm))

      # match normalized char_col
      if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
        keep_idx <- keep_idx | (as.character(df_sel_norm[[char_col]]) %in% characteristics_norm)
      }

      # match normalized tokens from char_col (split per-row)
      if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
        prof_vals <- as.character(df_sel_norm[[char_col]])
        for (i in seq_along(prof_vals)) {
          rvchars <- split_characteristics(prof_vals[i])
          if (length(rvchars) > 0) rvchars <- vapply(rvchars, normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)
          if (length(intersect(rvchars, characteristics_norm)) > 0) keep_idx[i] <- TRUE
        }
      }

      # fallback regex match if nothing matched (escape specials)
      if (!any(keep_idx)) {
        esc <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", characteristics_norm)
        pattern <- paste0("^(", paste0(esc, collapse = "|"), ")$")
        if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
          keep_idx <- keep_idx | grepl(pattern, as.character(df_sel_norm[[char_col]]), ignore.case = TRUE)
        }
        if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
          keep_idx <- keep_idx | grepl(pattern, as.character(df_sel_norm[[char_col]]), ignore.case = TRUE)
        }
      }

      df_plot_prep <- df_sel[keep_idx, , drop = FALSE]
      if (nrow(df_plot_prep) == 0) {
        return(safe_message_plot("No records match the selected characteristics for that site/date."))
      }

      val_col <- intersect(c("TADA.ResultMeasureValue", "ResultMeasureValue", "ResultMeasure"), names(df_plot_prep))[1]
      depth_col <- intersect(c("TADA.ConsolidatedDepth", "ConsolidatedDepth", "Depth"), names(df_plot_prep))[1]

      good_vals <- FALSE
      if (!is.na(val_col) && !is.na(depth_col)) {
        num_val <- suppressWarnings(as.numeric(as.character(df_plot_prep[[val_col]])))
        num_depth <- suppressWarnings(as.numeric(as.character(df_plot_prep[[depth_col]])))
        if (any(!is.na(num_val)) && any(!is.na(num_depth))) good_vals <- TRUE
      }

      if (!good_vals) {
        return(safe_message_plot("Selected combination has no numeric measure or depth values to plot."))
      }

      # All checks passed: call EPATADA plotting function
      p <- tryCatch({
        EPATADA::TADA_DepthProfilePlot(df_sel,
          groups = characteristics,
          location = sel_site,
          activity_date = sel_date,
          depthcat = input$depthcat,
          surfacevalue = input$surfacevalue,
          bottomvalue = input$bottomvalue)
      }, error = function(e) {
        safe_message_plot(paste0("Plot error: ", e$message))
      })

      p
    })

    # Render plotly (or convert ggplot to plotly)
    output$depthPlotly <- plotly::renderPlotly({
      p <- depth_plot_obj()
      shiny::req(p)
      if (inherits(p, "plotly") || inherits(p, "htmlwidget")) return(p)
      if (inherits(p, "ggplot")) return(plotly::ggplotly(p))
      safe_message_plot("Unable to render plot object.")
    })

    # Create a reactive data.frame that contains the same underlying data the plot
    # uses, but in "wide" form: one column per selected characteristic and rows = depths.
    depth_plot_data <- shiny::reactive({
      # Require that plot has been prepared (the eventReactive builds the plot on Update)
      # but we don't rely on its object - instead rebuild the filtered df similarly
      shiny::req(depth_profile$loaded)
      shiny::req(input$activity_date)
      # site must be selected
      sel_site <- if (!is.null(input$depth_profile_site_id) && nzchar(as.character(input$depth_profile_site_id))) input$depth_profile_site_id else input$siteid
      sel_date <- input$activity_date
      if (is.null(sel_site) || !nzchar(as.character(sel_site)) || is.null(sel_date) || !nzchar(as.character(sel_date))) {
        return(NULL)
      }

      # gather selected characteristics comp IDs (same source as depth_plot_obj)
      chars_idx <- input$available_characteristics_rows_selected
      characteristics <- character(0)
      if (!is.null(chars_idx) && length(chars_idx) > 0 &&
        !is.null(depth_profile$available_characteristics_df) && nrow(depth_profile$available_characteristics_df) > 0) {
        if ("CompID" %in% names(depth_profile$available_characteristics_df)) {
          characteristics <- as.character(depth_profile$available_characteristics_df$CompID[chars_idx])
        } else {
          characteristics <- as.character(depth_profile$available_characteristics_df$Characteristic[chars_idx])
        }
      }

      # fallback defaults (same as plot)
      if (length(characteristics) == 0) {
        if (!is.null(depth_profile$available_characteristics_df) && nrow(depth_profile$available_characteristics_df) > 0) {
          if ("CompID" %in% names(depth_profile$available_characteristics_df)) {
            characteristics <- utils::head(as.character(depth_profile$available_characteristics_df$CompID), 3)
          } else {
            characteristics <- utils::head(as.character(depth_profile$available_characteristics_df$Characteristic), 3)
          }
        } else {
          characteristics <- utils::head(unique(depth_profile$depth_categorized_df$TADA.ComparableDataIdentifier %||% character(0)), 3)
          if (length(characteristics) == 0) return(NULL)
        }
      }

      # Filter the depth_categorized_df similarly to depth_plot_obj
      df_all <- depth_profile$depth_categorized_df
      if (is.null(df_all) || nrow(df_all) == 0) return(NULL)

      loc_col  <- "TADA.MonitoringLocationIdentifier"
      date_col <- "ActivityStartDate"
      char_col <- "TADA.ComparableDataIdentifier"

      df_sel <- df_all
      if (!is.na(loc_col) && !is.null(sel_site) && nzchar(as.character(sel_site))) {
        df_sel <- df_sel[as.character(df_sel[[loc_col]]) == as.character(sel_site), , drop = FALSE]
      }
      if (!is.na(date_col) && !is.null(sel_date) && nzchar(as.character(sel_date))) {
        df_sel <- df_sel[as.character(df_sel[[date_col]]) == as.character(sel_date), , drop = FALSE]
      }

      if (nrow(df_sel) == 0) return(NULL)

      # normalize selected tokens
      characteristics_norm <- vapply(characteristics, normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)

      # prepare df_sel normalized copy for char_col
      df_sel_norm <- df_sel
      if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
        df_sel_norm[[char_col]] <- vapply(as.character(df_sel_norm[[char_col]]), normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)
      }

      # build keep index again (same matching strategy)
      keep_idx <- rep(FALSE, nrow(df_sel_norm))

      if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
        keep_idx <- keep_idx | (as.character(df_sel_norm[[char_col]]) %in% characteristics_norm)
      }

      if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
        prof_vals <- as.character(df_sel_norm[[char_col]])
        for (i in seq_along(prof_vals)) {
          rvchars <- split_characteristics(prof_vals[i])
          if (length(rvchars) > 0) rvchars <- vapply(rvchars, normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE)
          if (length(intersect(rvchars, characteristics_norm)) > 0) keep_idx[i] <- TRUE
        }
      }

      # fallback regex if needed (same as plot)
      if (!any(keep_idx)) {
        esc <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", characteristics_norm)
        pattern <- paste0("^(", paste0(esc, collapse = "|"), ")$")
        if (!is.na(char_col) && char_col %in% names(df_sel_norm)) {
          keep_idx <- keep_idx | grepl(pattern, as.character(df_sel_norm[[char_col]]), ignore.case = TRUE)
        }
      }

      df_plot_prep <- df_sel[keep_idx, , drop = FALSE]
      if (nrow(df_plot_prep) == 0) return(NULL)

      # identify value and depth columns as used in the plotting code
      val_col <- intersect(c("TADA.ResultMeasureValue", "ResultMeasureValue", "ResultMeasure"), names(df_plot_prep))[1]
      depth_col <- intersect(c("TADA.ConsolidatedDepth", "ConsolidatedDepth", "Depth"), names(df_plot_prep))[1]

      if (is.null(val_col) || is.null(depth_col)) return(NULL)

      # Prepare a tidy long table containing characteristic, depth, value
      # We will try to get a characteristic label for each row: prefer normalized char_col, otherwise try TADA.CharacteristicName
      char_label_col <- NULL
      if (!is.na(char_col) && char_col %in% names(df_plot_prep)) char_label_col <- char_col
      alt_char_name_col <- intersect(c("TADA.CharacteristicName", "CharacteristicName", "Characteristic"), names(df_plot_prep))[1]

      # Ensure depth and value numeric
      df_plot_prep <- df_plot_prep %>%
        dplyr::mutate(
          .depth_num = suppressWarnings(as.numeric(as.character(.data[[depth_col]]))),
          .value_num = suppressWarnings(as.numeric(as.character(.data[[val_col]])))
        )

      # Create a character label to pivot on
      if (!is.null(char_label_col)) {
        df_plot_prep <- df_plot_prep %>%
          dplyr::mutate(.char_label = vapply(as.character(.data[[char_label_col]]), normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE))
      } else if (!is.null(alt_char_name_col)) {
        df_plot_prep <- df_plot_prep %>%
          dplyr::mutate(.char_label = vapply(as.character(.data[[alt_char_name_col]]), normalize_token, FUN.VALUE = character(1), USE.NAMES = FALSE))
      } else {
        # fallback: use ComparableDataIdentifier if present or ResultIdentifier
        fallback_char <- intersect(c("TADA.ComparableDataIdentifier", "TADA.ResultIdentifier", "ResultIdentifier"), names(df_plot_prep))[1]
        df_plot_prep <- df_plot_prep %>%
          dplyr::mutate(.char_label = if (!is.null(fallback_char)) as.character(.data[[fallback_char]]) else "value")
      }

      # Filter out rows missing depth and value
      df_plot_prep <- df_plot_prep %>% dplyr::filter(!is.na(.depth_num))

      if (nrow(df_plot_prep) == 0) return(NULL)

      # If multiple observations for same depth & characteristic exist, choose summary (mean)
      tidy_long <- df_plot_prep %>%
        dplyr::group_by(.char_label, .depth_num) %>%
        dplyr::summarise(value = mean(.value_num, na.rm = TRUE), .groups = "drop")

      # Pivot to wide: depth as first column, then one column per .char_label
      tidy_wide <- tidy_long %>%
        tidyr::pivot_wider(names_from = .char_label, values_from = value)

      # Ensure depth column is first and sorted (ascending or descending to match plot orientation)
      if (!" .depth_num" %in% names(tidy_wide)) {
        # pivot_wider will keep .depth_num as a column; rename if needed
      }
      # after pivot_wider and before returning tidy_wide, do:

      # 1) Rename depth column to "Depth (m)" and sort by depth (ascending)
      tidy_wide <- tidy_wide %>%
        dplyr::rename(`Depth (m)` = .data$.depth_num) %>%
        dplyr::arrange(`Depth (m)`)

      # 2) Sanitize the other column names (characteristic columns) using your helper
      #    normalize_NA_token (optionally also normalize_token if desired).
      all_cols <- names(tidy_wide)

      # Keep the depth column name unchanged
      depth_col_name <- "Depth (m)"

      # Identify non-depth columns to sanitize
      other_cols <- setdiff(all_cols, depth_col_name)

      if (length(other_cols) > 0) {
        # Apply normalization: first normalize_token (trim/strip trailing counts), then normalize_NA_token
        sanitized <- vapply(other_cols,
          FUN.VALUE = character(1),
          FUN = function(x) {
            nm <- normalize_token(x)
            nm <- normalize_NA_token(nm)
            nm
          })

        # Ensure uniqueness (append suffixes if necessary)
        if (any(duplicated(sanitized))) {
          # make unique while preserving order (like make.names(unique=TRUE) but keep nicer labels)
          sanitized <- make.unique(sanitized, sep = "_")
        }

        # Build final names vector and assign
        new_names <- all_cols
        new_names[match(other_cols, all_cols)] <- sanitized
        names(tidy_wide) <- new_names
      }

      # Return the wide df (depth first)
      tidy_wide
    })

    # Render the depth plot data table (one column per selected characteristic, rows = depths)
    output$depth_plot_data_table <- DT::renderDT({
      df <- depth_plot_data()
      if (is.null(df) || nrow(df) == 0) {
        DT::datatable(data.frame(Message = "No data to show. Click Update plot or select a Site/Date and characteristics."),
          options = list(dom = "t"))
      } else {
        # Format numeric columns reasonably
        # Use server = TRUE if you expect many rows; small tables fine server = FALSE
        DT::datatable(
          df,
          rownames = FALSE,
          options = list(pageLength = 30, scrollX = TRUE, dom = "t")
        )
      }
    }, server = FALSE)





    output$number_of_records <- shiny::renderText({
      if (!depth_profile$loaded) return("Data not loaded.")
      if (is.null(depth_profile$depth_categorized_df)) return("No data available.")
      scales::comma(nrow(depth_profile$depth_categorized_df))
    })

    output$number_of_sites <- shiny::renderText({
      if (!depth_profile$loaded) return("Data not loaded.")
      if (is.null(depth_profile$depth_categorized_df)) return("No data available.")
      num_sites <- length(unique(as.character(depth_profile$depth_categorized_df$TADA.MonitoringLocationIdentifier)))
      scales::comma(num_sites)
    })

    output$number_of_sites2 <- shiny::renderText({
      if (!depth_profile$loaded) return("Data not loaded.")
      if (is.null(depth_profile$depth_categorized_df)) return("No data available.")
      num_sites <- length(unique(as.character(depth_profile$depth_categorized_df$TADA.MonitoringLocationIdentifier)))
      scales::comma(num_sites)
    })


    output$debug_text <- shiny::renderText({
      paste0(
        "loaded: ", depth_profile$loaded, "\n",
        "selected site: ", ifelse(is.null(input$depth_profile_site_id),
          "<none>",
          input$depth_profile_site_id), "\n",
        "selected date: ", ifelse(is.null(input$activity_date),
          "<none>",
          input$activity_date), "\n",
        "available_characteristics rows: ", ifelse(is.null(depth_profile$available_characteristics_df),
          "NULL",
          nrow(depth_profile$available_characteristics_df))
      )
    })


  })
}

## To be copied in the UI
# mod_depth_ui("depth_1")

## To be copied in the server
# mod_depth_server("depth_1")
