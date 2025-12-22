# Load the input data
data_path1 <- app_sys("extdata/filter_descriptions.RData")
load(data_path1)

mod_filtering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::HTML("<h3>Select field to filter on:</h3>"),
    htmltools::HTML(
      "Fields are listed in the table below, along with the number of unique values present in that field. These counts do not include unique values from results flagged for removal. Click on a field name and a new table will appear below showing the counts associated with each unique value in the selected field."
    ),
    shiny::radioButtons(
      ns("field_sel"),
      label = "Fields to select from: ",
      choices = c("key", "most", "all"),
      selected = "key",
      inline = TRUE
    ),
    DT::DTOutput(ns("filterStep1")),
    htmltools::br(),
    shiny::htmlOutput(ns("promptStep2")),
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(4, shiny::plotOutput(ns("filter_pie_chart"), height = "500px")),
      shiny::column(8, DT::DTOutput(ns("filterStep2")))
    ),
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::actionButton(ns("addOnlys"), "Include Only Selected Values", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      shiny::column(
        3,
        shiny::actionButton(ns("addExcludes"), "Exclude Selected Values", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
    ),
    htmltools::br(),
    htmltools::br(),
    htmltools::h3("Selected filters"),
    htmltools::HTML(
      "Your exclude/include filters are documented below. You can easily reset one or more filter decisions by clicking on rows of interest and then hitting the 'Remove Selected Filters' button. Alternatively, you can reset/remove all filters on this page by clicking the 'Reset All Filters' button."
    ),
    DT::DTOutput(ns("selectedFilters")),
    htmltools::br(),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::actionButton(ns("removeFilters"), "Reset Selected Filters", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      shiny::column(
        3,
        shiny::actionButton(ns("resetFilters"), "Reset All Filters", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
    )
  )
}

mod_filtering_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tables <- shiny::reactiveValues()
    values <- shiny::reactiveValues()
    values$locked <- character()
    values$selected_field <- NULL
    shinyjs::hide("addOnlys")
    shinyjs::hide("addExcludes")
    
    # Value counting with NA and literal "NA" unified to "[NA]" label
    getValues <- function(.data, field) {
      if (is.null(.data) || is.null(field) || !(field %in% names(.data))) {
        return(data.frame(
          Value_label = character(),
          Count       = integer(),
          IsNA        = logical(),
          stringsAsFactors = FALSE
        ))
      }
      x <- .data[[field]]
      x_chr <- as.character(x)
      # Map both true NA and literal "NA" to "[NA]"
      lbl <- ifelse(is.na(x) | x_chr == "NA", "[NA]", x_chr)
      tab <- as.data.frame(table(lbl), stringsAsFactors = FALSE)
      names(tab) <- c("Value_label", "Count")
      tab$IsNA <- tab$Value_label == "[NA]"
      tab
    }
    
    # Initialize when Filter tab activates
    shiny::observeEvent(tadat$tab, {
      if (identical(tadat$tab, "Filter")) {
        # Use only rows not flagged for removal
        tables$dat <- dplyr::filter(tadat$raw, TADA.Remove == FALSE)
        
        tables$filter_fields <-
          EPATADA::TADA_FieldCounts(tables$dat, display = "key") %>%
          dplyr::left_join(filter_dat, by = "Fields") %>%
          dplyr::mutate(Description = ifelse(is.na(Description),
                                             "No description available",
                                             Description))
        
        tables$filter_fields[
          tables$filter_fields$Fields == "TADA.Media.Flag",
          "Description"
        ] <- "TADA-standardized media fields"
      }
    })
    
    # Step 1: field list
    output$filterStep1 <- DT::renderDT({
      shiny::req(tables$filter_fields)
      tables$filter_fields
    },
    escape = FALSE,
    selection = "single",
    rownames = FALSE,
    options = list(
      dom = "t",
      pageLength = nrow(tables$filter_fields),
      paging = FALSE
    ))
    
    # Pie chart for selected field
    output$filter_pie_chart <- shiny::renderPlot({
      shiny::req(values$selected_field)
      shiny::req(tadat$raw)
      pie_data <- tadat$raw
      pie_data <- pie_data[pie_data$TADA.Remove == FALSE, , drop = FALSE]
      shiny::req(values$selected_field %in% names(pie_data))
      # Exclude current removals
      if (!is.null(tadat$removals) && is.data.frame(tadat$removals) && ncol(tadat$removals) > 0) {
        keep <- rowSums(tadat$removals) == 0
        pie_data <- pie_data[keep, , drop = FALSE]
      }
      EPATADA::TADA_FieldValuesPie(pie_data, field = values$selected_field)
    })
    
    # Step 1 selection => Step 2 values
    shiny::observeEvent(input$filterStep1_rows_selected, {
      sel <- input$filterStep1_rows_selected
      shiny::req(!is.null(sel))
      field_name <- tables$filter_fields[sel, "Fields"]
      if (is.null(field_name) || is.na(field_name) || !nzchar(field_name) || !(field_name %in% names(tables$dat))) {
        values$selected_field <- NULL
        tables$filter_values <- NULL
        shinyjs::hide("addOnlys")
        shinyjs::hide("addExcludes")
        output$promptStep2 <- shiny::renderUI(htmltools::HTML("<p>No valid field selected.</p>"))
        return(NULL)
      }
      values$selected_field <- field_name
      applyLocks()
      tables$filter_values <- getValues(tables$dat, values$selected_field)
      output$promptStep2 <- shiny::renderUI(htmltools::HTML(
        paste0(
          "<h3>Filter by '", values$selected_field, "'</h3>",
          "<p>Select one or more values below, including the special <b>[NA]</b> value if present. ",
          "Then choose whether to exclude those values, or keep only those values.</p>"
        )
      ))
      shinyjs::show("addOnlys")
      shinyjs::show("addExcludes")
    })
    
    # Step 2: values list (show label and counts; "[NA]" is clickable)
    output$filterStep2 <- DT::renderDT({
      shiny::req(tables$filter_values)
      data.frame(
        Value = tables$filter_values$Value_label,
        Count = tables$filter_values$Count,
        stringsAsFactors = FALSE
      )
    },
    escape = FALSE,
    selection = "multiple",
    rownames = FALSE,
    options = list(
      dom = "t",
      pageLength = nrow(tables$filter_values)
    ))
    
    # Initialize selected filters (track IsNA)
    tadat$selected_filters <- data.frame(
      Fields = character(),
      Value  = character(),  # displayed label (includes "[NA]" for NA and "NA")
      Filter = character(),
      Count  = integer(),
      IsNA   = logical(),
      stringsAsFactors = FALSE
    )
    
    # Selected filters table (hide IsNA in display)
    output$selectedFilters <- DT::renderDT({
      shiny::req(tadat$selected_filters)
      tadat$selected_filters[, c("Fields", "Value", "Filter", "Count"), drop = FALSE]
    },
    escape = FALSE,
    selection = "multiple",
    rownames = FALSE,
    options = list(
      dom = "t",
      paging = FALSE,
      language = list(zeroRecords = "No filters selected")
    ))
    
    # Add selections from Step 2
    selectFilters <- function(Filter) {
      if (is.null(values$selected_field) || !(values$selected_field %in% names(tables$dat))) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid selection",
          "Please select a valid field before choosing values."
        ))
        return(invisible(NULL))
      }
      values$locked[values$selected_field] <- Filter
      rows <- input$filterStep2_rows_selected
      if (is.null(rows) || length(rows) == 0) return(invisible(NULL))
      
      Fields <- rep(values$selected_field, length(rows))
      Value  <- tables$filter_values$Value_label[rows]
      IsNA   <- tables$filter_values$IsNA[rows]
      Count  <- rep(0L, length(rows))
      
      new_rows <- data.frame(Fields, Value, Filter, Count, IsNA, stringsAsFactors = FALSE)
      tadat$selected_filters <- dplyr::distinct(
        rbind(tadat$selected_filters, new_rows),
        Fields, Value, .keep_all = TRUE
      )
    }
    
    # Include Only
    shiny::observeEvent(input$addOnlys, {
      if (is.null(input$filterStep2_rows_selected)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Select Field Values",
            "You must select (by clicking on) the field value(s) you'd like to include in your dataset before clicking Include Only Selected Values."
          )
        )
      } else {
        selectFilters("Keep only")
      }
    })
    
    # Exclude
    shiny::observeEvent(input$addExcludes, {
      if (is.null(input$filterStep2_rows_selected)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Select Field Values",
            "You must select (by clicking on) the field value(s) you'd like to exclude from your dataset before clicking Exclude Selected Values."
          )
        )
      } else {
        selectFilters("Exclude")
      }
    })
    
    # Radio for fields display
    shiny::observeEvent(input$field_sel, {
      tadat$field_sel <- input$field_sel
    })
    
    shiny::observeEvent(tadat$field_sel, {
      shiny::updateRadioButtons(session, "field_sel", selected = tadat$field_sel)
      if (!is.null(tables$dat)) {
        tables$filter_fields <-
          EPATADA::TADA_FieldCounts(tables$dat, display = tadat$field_sel) %>%
          dplyr::left_join(filter_dat, by = "Fields") %>%
          dplyr::mutate(Description = ifelse(is.na(Description),
                                             "No description available",
                                             Description))
        tables$filter_fields[
          tables$filter_fields$Fields == "TADA.Media.Flag",
          "Description"
        ] <- "TADA-standardized media fields"
      }
    })
    
    # Reset all filters
    shiny::observeEvent(input$resetFilters, {
      tadat$selected_filters <- data.frame(
        Fields = character(),
        Value  = character(),
        Filter = character(),
        Count  = integer(),
        IsNA   = logical(),
        stringsAsFactors = FALSE
      )
    })
    
    # Remove selected filters
    shiny::observeEvent(input$removeFilters, {
      if (is.null(input$selectedFilters_rows_selected)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Select Filter",
            "You must select (by clicking on) the filter(s) you'd like to remove from the applied filters table."
          )
        )
      } else {
        tadat$selected_filters <- tadat$selected_filters[-input$selectedFilters_rows_selected, ]
      }
    })
    
    # Maintain locks
    shiny::observeEvent(tadat$selected_filters, {
      still_present <- intersect(names(values$locked), unique(tadat$selected_filters$Fields))
      values$locked <- values$locked[still_present]
    })
    
    applyLocks <- function() {
      if (!is.null(values$selected_field) && values$selected_field %in% names(tables$dat)) {
        active_lock <- values$locked[values$selected_field]
        if (is.na(active_lock)) {
          shinyjs::enable("addOnlys"); shinyjs::enable("addExcludes")
        } else if (active_lock == "Keep only") {
          shinyjs::enable("addOnlys"); shinyjs::disable("addExcludes")
        } else {
          shinyjs::disable("addOnlys"); shinyjs::enable("addExcludes")
        }
      } else {
        shinyjs::disable("addOnlys"); shinyjs::disable("addExcludes")
      }
    }
    
    shiny::observeEvent(values$locked, {
      applyLocks()
    })
    
    # Apply filters, update counts, and removal reasons
    shiny::observeEvent(tadat$selected_filters, {
      # Update lock state per field
      field_filters <- dplyr::distinct(tadat$selected_filters, Fields, Filter)
      values$locked <- field_filters$Filter
      names(values$locked) <- field_filters$Fields
      prefix <- "Filter: "
      
      # Initialize/remake removals frame
      if (is.null(tadat$removals) && !is.null(tadat$raw)) {
        tadat$removals <- data.frame(matrix(nrow = nrow(tadat$raw), ncol = 0))
      } else if (!is.null(tadat$removals)) {
        tadat$removals <- dplyr::select(tadat$removals, -(dplyr::starts_with(prefix)))
      }
      
      if (!is.null(tadat$raw)) {
        shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
        
        if (nrow(tadat$selected_filters) > 0) {
          shinyjs::enable("resetFilters")
          shinyjs::enable("removeFilters")
          
          # Apply per-field filters
          for (active_field in unique(tadat$selected_filters$Fields)) {
            if (!(active_field %in% names(tadat$raw))) next
            filter_type <- values$locked[active_field]
            field_filters <- tadat$selected_filters[tadat$selected_filters$Fields == active_field, , drop = FALSE]
            
            results <- rep(FALSE, nrow(tadat$raw))
            for (row_idx in seq_len(nrow(field_filters))) {
              is_na_sel <- isTRUE(field_filters[row_idx, "IsNA"])
              if (is_na_sel) {
                # Select rows that are either true NA or literal "NA"
                sel <- is.na(tadat$raw[[active_field]]) |
                  (as.character(tadat$raw[[active_field]]) == "NA")
              } else {
                sel <- (as.character(tadat$raw[[active_field]]) == field_filters[row_idx, "Value"])
              }
              sel[is.na(sel)] <- FALSE
              results <- results | sel
            }
            # Keep only: remove rows not selected
            if (identical(filter_type, "Keep only")) {
              results <- !results
            }
            all_vals <- paste(field_filters$Value, collapse = " or ")
            label <- paste0(prefix, filter_type, " ", active_field, " is ", all_vals)
            tadat$removals[[label]] <- as.logical(results)
          }
        }
        
        # Update counts in selected_filters
        if (!is.null(tables$dat) && nrow(tadat$selected_filters) > 0) {
          new_selected_filters <- tadat$selected_filters
          for (i in seq_len(nrow(new_selected_filters))) {
            row <- new_selected_filters[i, ]
            vals <- getValues(tables$dat, row$Fields)
            if (isTRUE(row$IsNA)) {
              new_selected_filters[i, "Count"] <- sum(vals$Count[vals$IsNA], na.rm = TRUE)
            } else {
              new_selected_filters[i, "Count"] <- sum(vals$Count[vals$Value_label == row$Value], na.rm = TRUE)
            }
          }
          tadat$selected_filters <- new_selected_filters
        }
        
        # Update TADA.RemovalReason
        removals <- tadat$removals
        if (is.data.frame(removals) && nrow(removals) == nrow(tadat$raw)) {
          sel <- which(removals == TRUE, arr.ind = TRUE)
          if (length(sel) > 0) {
            removals[sel] <- names(removals)[sel[, "col"]]
            removals[removals == FALSE] <- ""
            tadat$raw$TADA.RemovalReason <- apply(
              removals, 1,
              function(row) paste(row[nzchar(row)], collapse = ", ")
            )
          } else {
            tadat$raw$TADA.RemovalReason <- NA
          }
        }
      }
    })
  })
}
