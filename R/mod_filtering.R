mod_filtering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::HTML("<h3>Select field to filter on:</h3>"),
    htmltools::HTML(
      "Key columns are listed in the table below, along with the number of unique values present in that field. These counts do not include unique values from results flagged for removal. Click on a field name and a new table will appear below showing the counts associated with each unique value in the selected field."
    ),
    DT::dataTableOutput(ns("filterStep1")),
    htmltools::br(),
    shiny::htmlOutput(ns("promptStep2")),
    DT::DTOutput(ns("filterStep2")),
    htmltools::br(),
    shiny::fluidRow(
      column(
        3,
        shiny::actionButton(ns("addOnlys"), "Include Only Selected Values",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ),
      column(
        3,
        shiny::actionButton(ns("addExcludes"),
          "Exclude Selected Values",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
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
      column(
        3,
        shiny::actionButton(ns("removeFilters"),
          "Reset Selected Filters",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
      ),
      column(
        3,
        shiny::actionButton(ns("resetFilters"),
          "Reset All Filters",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
        )
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

    # make sure dataset being used to create filters is only REMOVE = FALSE
    shiny::observe({
      shiny::req(tadat$tab)
      if (tadat$tab == "Filter") {
        # only show unique values from data that have not been flagged for removal
        tables$dat <- subset(tadat$raw, tadat$raw$TADA.Remove == FALSE)
        tables$filter_fields <- TADA::TADA_FieldCounts(tables$dat, display = "key")
      }
    })

    # First data table with key columns
    output$filterStep1 <- DT::renderDT(
      tables$filter_fields,
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = dim(tables$filter_fields)[1],
        paging = FALSE
      )
    )

    # When key column selected, get unique values for that column
    shiny::observeEvent(input$filterStep1_rows_selected, {
      # Get the name of the selected field
      values$selected_field <- tables$filter_fields[input$filterStep1_rows_selected, ]$Field
      applyLocks()
      tables$filter_values <- data.frame(getValues(tables$dat, values$selected_field))
      output$promptStep2 <- shiny::renderUI(HTML(
        paste0(
          "<h3>Filter by '",
          values$selected_field,
          "'</h3>
               <p>In this table, you may either exclude selected values, or ONLY include selected values and exclude all other non-selected values. Use the buttons at the bottom of this table to make your decisions. Note that once you select a filtering type (Exclude or Include), the other filtering type button is disabled for that field. <b>Note:</b> If any results are NA, they will be represented by a blank (empty) row in this table.</p>"
        )
      ))
      shinyjs::show("addOnlys")
      shinyjs::show("addExcludes")
    })

    # show unique values for selected column
    output$filterStep2 <- DT::renderDT(
      tables$filter_values,
      escape = FALSE,
      selection = "multiple",
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = dim(tables$filter_values)[1]
      )
    )

    # empty selected table on open
    tadat$selected_filters <-
      data.frame(matrix(
        ncol = 4,
        nrow = 0,
        dimnames = list(NULL, c("Field", "Value", "Filter", "Count"))
      ))

    # selected table at bottom
<<<<<<< HEAD
    output$selectedFilters <- DT::renderDT(
      tables$selected,
=======
    output$selectedFilters =  DT::renderDT(
      tadat$selected_filters,
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
      escape = FALSE,
      selection = "multiple",
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        language = list(zeroRecords = "No filters selected")
      )
    )

    # what happens when you click "Include Only Selected Values"
    shiny::observeEvent(input$addOnlys, {
      if (is.null(input$filterStep2_rows_selected)) {
        # make sure something is selected
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

    # what happens when you click "Exclude Selected Values"
    shiny::observeEvent(input$addExcludes, {
      if (is.null(input$filterStep2_rows_selected)) {
        # make sure something is selected
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

    # reset all filters in bottom table
    shiny::observeEvent(input$resetFilters, {
<<<<<<< HEAD
      tables$selected <- tables$selected[0, ]
=======
      tadat$selected_filters = tadat$selected_filters[0, ]
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
    })

    # reset selected filters in bottom table
    shiny::observeEvent(input$removeFilters, {
      if (is.null(input$selectedFilters_rows_selected)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Select Filter",
            "You must select (by clicking on) the filter(s) you'd like to remove from the applied filters table."
          )
        )
<<<<<<< HEAD
      } else {
        tables$selected <- tables$selected[-input$selectedFilters_rows_selected, ]
=======
      } else{
        tadat$selected_filters = tadat$selected_filters[-input$selectedFilters_rows_selected, ]
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
      }
    })

    # Called whenever a "Include" or "Exclude" button is clicked
    selectFilters <- function(Filter) {
<<<<<<< HEAD
      # Locks the value of the selected field to "Include" or "Exclude"
      values$locked[values$selected_field] <- Filter
=======
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
      # Initializes a table for the newly selected values
      rows <- input$filterStep2_rows_selected
      Field <- values$selected_field
      Value <- tables$filter_values[rows, "Value"]
      Count <- tables$filter_values[rows, "Count"]
      new_rows <- data.frame(Field, Value, Filter, Count)
      # Adds the newly selected field/vals to the Selected table
<<<<<<< HEAD
      tables$selected <- rbind(tables$selected, new_rows)
      tables$selected <-
        tables$selected %>% dplyr::distinct(Field, Value, .keep_all = TRUE)
=======
      tadat$selected_filters = rbind(tadat$selected_filters, new_rows)
      tadat$selected_filters =
        tadat$selected_filters %>% dplyr::distinct(Field, Value, .keep_all = TRUE)
      
      # Locks the value of the selected field to "Include" or "Exclude"
      #values$locked[values$selected_field] = Filter
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
    }

    #####
    # These functions are used to lock fields to "Include or Exclude"
    # This is necessary because including ONLY certain values from a field
    # will inherently exclude all others, so there can't be mixing
<<<<<<< HEAD
    shiny::observeEvent(tables$selected, {
      still_present <- intersect(names(values$locked), unique(tables$selected$Field))
      values$locked <- values$locked[still_present]
    })

=======
    
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
    applyLocks <- function() {
      if (!is.null(values$selected_field)) {
        active_lock <- values$locked[values$selected_field]
        if (is.na(active_lock)) {
          shinyjs::enable("addOnlys")
          shinyjs::enable("addExcludes")
        } else if (active_lock == "Keep only") {
          shinyjs::enable("addOnlys")
          shinyjs::disable("addExcludes")
        } else {
          shinyjs::disable("addOnlys")
          shinyjs::enable("addExcludes")
        }
      } else {
        shinyjs::disable("addOnlys")
        shinyjs::disable("addExcludes")
      }
    }

    shiny::observeEvent(values$locked, {
      applyLocks()
    })
    #####

    # This gets run whenever a change in selected filters happens
<<<<<<< HEAD
    shiny::observeEvent(tables$selected, {
      prefix <- "Filter: "
=======
    shiny::observeEvent(tadat$selected_filters, {
      
      # Apply field locks 
      field_filters = dplyr::distinct(tadat$selected_filters, Field, Filter)
      values$locked = field_filters$Filter
      names(values$locked) <- field_filters$Field
      
      prefix = "Filter: "
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
      # Remove all the filter columns from the removals table (start fresh)
      if (!is.null(tadat$removals)) {
        tadat$removals <- dplyr::select(tadat$removals, -(dplyr::starts_with(prefix)))
      }

      # Only proceed if filters have been selected
      if (nrow(tadat$selected_filters) > 0) {
        # Since filters have been added, enable the ability to reset them
        shinyjs::enable("resetFilters")
        shinyjs::enable("removeFilters")

        # Loop through the filters field-by-field
        for (active_field in unique(tadat$selected_filters$Field)) {
          filter_type <- values$locked[active_field]
<<<<<<< HEAD
          field_filters <- tables$selected[tables$selected$Field == active_field, ]
=======
          field_filters = tadat$selected_filters[tadat$selected_filters$Field == active_field, ]
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
          results <- rep(FALSE, nrow(tadat$raw))
          for (row in 1:nrow(field_filters)) {
            sel <- (tadat$raw[[active_field]] == field_filters[row, "Value"])
            sel[is.na(sel)] <- FALSE
            results <- results | sel
          }
          # Get the intersection of all the places where True
          if (filter_type == "Keep only") {
            results <- !results
          }
<<<<<<< HEAD
          all_vals <- paste(field_filters$Value, collapse = " or ")
          label <- paste0(prefix, filter_type, " ", active_field, " is ", all_vals)
          tadat$removals[label] <- as.logical(results)
=======
          all_vals = paste(field_filters$Value, collapse = " or ")
          label = paste0(prefix, filter_type, " ", active_field, " is ", all_vals)
          tadat$removals[label] = as.logical(results)
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
        }
      }
    })

    getValues <- function(.data, field) {
<<<<<<< HEAD
      counts <- table(.data[[field]], useNA = "ifany")
=======
      counts = table(.data[[field]])
>>>>>>> 75ec81b89342ca80ab74a83c1b5c3be20b399873
      if (length(rownames(counts) > 0)) {
        value_table <- data.frame(Value = names(counts), Count = as.vector(counts))
      } else {
        value_table <- data.frame(Value = character(), Count = integer())
      }
      return(value_table)
    }
  })
}
