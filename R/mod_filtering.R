# Load the input data (keep your package's app_sys)
data_path1 <- app_sys("extdata/filter_descriptions.RData")
load(data_path1)

mod_filtering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
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
        shinyjs::hidden(
          shiny::actionButton(ns("addOnlys"), "Include Only Selected Values", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
      ),
      shiny::column(
        3,
        shinyjs::hidden(
          shiny::actionButton(ns("addExcludes"), "Exclude Selected Values", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
    values$selected_field <- NULL
    
    # Unified UI label for missing values
    na_label <- "NA - Not Available"
    
    # Robust labelizer: NA/NULL -> "NA - Not Available"; handles list-columns
    labelize <- function(v) {
      if (is.list(v)) {
        chr <- vapply(v, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
      } else {
        chr <- tryCatch(as.character(v), error = function(e) rep(NA_character_, length(v)))
      }
      chr[is.na(chr) | toupper(chr) %in% c("NA", "NULL")] <- na_label
      chr
    }
    
    # Active dataset: excludes TADA.Remove and rows flagged in tadat$removals
    active_data <- shiny::reactive({
      d <- tadat$raw
      shiny::req(d)
      d <- d[d$TADA.Remove == FALSE, , drop = FALSE]
      if (is.data.frame(tadat$removals) && ncol(tadat$removals) > 0) {
        rem <- tadat$removals
        if (!all(vapply(rem, is.logical, logical(1)))) {
          rem <- as.data.frame(lapply(rem, function(col) as.logical(col)))
        }
        keep <- rowSums(rem, na.rm = TRUE) == 0
        d <- d[keep, , drop = FALSE]
      }
      d
    })
    
    # Value counting from active data; safe for empty data
    getValues <- function(.data, field) {
      if (is.null(.data) || is.null(field) || !(field %in% names(.data)) || nrow(.data) == 0) {
        return(data.frame(Value_label = character(), Count = integer(), stringsAsFactors = FALSE))
      }
      lab <- labelize(.data[[field]])
      if (length(lab) == 0) {
        return(data.frame(Value_label = character(), Count = integer(), stringsAsFactors = FALSE))
      }
      counts <- table(lab)
      data.frame(
        Value_label = names(counts),
        Count = as.integer(counts),
        stringsAsFactors = FALSE
      )
    }
    
    # Initialize when Filter tab activates (use active data)
    shiny::observeEvent(tadat$tab, {
      if (identical(tadat$tab, "Filter")) {
        d <- active_data()
        tables$filter_fields <-
          EPATADA::TADA_FieldCounts(d, display = "key") %>%
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
      pageLength = max(1L, nrow(tables$filter_fields)),
      paging = FALSE
    ))
    
    # Pie chart source: labelize selected field
    pie_source <- shiny::reactive({
      d <- active_data()
      fld <- values$selected_field
      shiny::req(!is.null(fld), fld %in% names(d))
      d2 <- d
      d2[[fld]] <- labelize(d[[fld]])
      d2
    })
    
    output$filter_pie_chart <- shiny::renderPlot({
      fld <- values$selected_field
      shiny::req(!is.null(fld))
      d <- active_data()
      # Avoid calling EPATADA pie on empty/missing data
      if (is.null(d) || nrow(d) == 0 || !(fld %in% names(d))) {
        plot.new()
        title("No data to display")
        return(invisible())
      }
      dummy_dep <- if (!is.null(tadat$removals)) ncol(tadat$removals) else 0
      d2 <- pie_source()
      if (nrow(d2) == 0) {
        plot.new()
        title("No data to display")
        return(invisible())
      }
      EPATADA::TADA_FieldValuesPie(d2, field = fld)
    })
    
    # Step 1 selection => Step 2 prompt/setup
    shiny::observeEvent(input$filterStep1_rows_selected, {
      sel <- input$filterStep1_rows_selected
      shiny::req(!is.null(sel))
      field_name <- as.character(tables$filter_fields$Fields[sel])
      if (is.null(field_name) || is.na(field_name) || !nzchar(field_name)) {
        values$selected_field <- NULL
        shinyjs::hide("addOnlys")
        shinyjs::hide("addExcludes")
        output$promptStep2 <- shiny::renderUI(htmltools::HTML("<p>No valid field selected.</p>"))
        return(NULL)
      }
      d <- active_data()
      if (!(field_name %in% names(d))) {
        values$selected_field <- NULL
        shinyjs::hide("addOnlys")
        shinyjs::hide("addExcludes")
        output$promptStep2 <- shiny::renderUI(htmltools::HTML("<p>Selected field is not present in the current dataset.</p>"))
        return(NULL)
      }
      values$selected_field <- field_name
      output$promptStep2 <- shiny::renderUI(htmltools::HTML(
        paste0(
          "<h3>Filter by '", values$selected_field, "'</h3>",
          "<p>Select one or more values below, including <b>", na_label, "</b> if present. ",
          "Then choose whether to exclude those values, or keep only those values.</p>"
        )
      ))
      shinyjs::show("addOnlys")
      shinyjs::show("addExcludes")
    })
    
    # Step 2 values from the current active data (reactive)
    filter_values <- shiny::reactive({
      fld <- values$selected_field
      d <- active_data()
      if (!is.null(fld) && !is.null(d) && fld %in% names(d)) {
        getValues(d, fld)
      } else {
        data.frame(Value_label = character(), Count = integer(), stringsAsFactors = FALSE)
      }
    })
    
    output$filterStep2 <- DT::renderDT({
      vals <- filter_values()
      data.frame(
        Value = vals$Value_label,
        Count = vals$Count,
        stringsAsFactors = FALSE
      )
    },
    escape = FALSE,
    selection = "multiple",
    rownames = FALSE,
    options = list(
      dom = "t",
      pageLength = max(1L, nrow(filter_values()))
    ))
    
    # Initialize selected filters (labels only)
    tadat$selected_filters <- data.frame(
      Fields = character(),
      Value  = character(),  # label (includes "NA - Not Available")
      Filter = character(),  # always "Exclude" entries in this design
      Count  = integer(),
      stringsAsFactors = FALSE
    )
    
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
    # - Include Only: add complement values as Exclude (replace any prior filters for that field)
    # - Exclude: add selected values as Exclude (accumulate, deduplicate)
    add_filters_include_only <- function() {
      d <- active_data()
      fld <- values$selected_field
      if (is.null(fld) || !(fld %in% names(d))) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid selection",
          "Please select a valid field before choosing values."
        ))
        return(invisible(NULL))
      }
      rows <- input$filterStep2_rows_selected
      if (is.null(rows) || length(rows) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Select Field Values",
          "You must select the field value(s) to include."
        ))
        return(invisible(NULL))
      }
      
      vals <- filter_values()
      all_labels <- vals$Value_label
      selected_labels <- unique(vals$Value_label[rows])
      complement_labels <- setdiff(all_labels, selected_labels)
      
      # Replace any existing filters for this field with complement excludes
      tadat$selected_filters <- tadat$selected_filters[!(tadat$selected_filters$Fields == fld), , drop = FALSE]
      
      if (length(complement_labels) > 0) {
        new_rows <- data.frame(
          Fields = rep(fld, length(complement_labels)),
          Value  = complement_labels,
          Filter = rep("Exclude", length(complement_labels)),
          Count  = integer(length(complement_labels)),
          stringsAsFactors = FALSE
        )
        tadat$selected_filters <- dplyr::distinct(
          rbind(tadat$selected_filters, new_rows),
          Fields, Value, .keep_all = TRUE
        )
      }
    }
    
    add_filters_exclude <- function() {
      d <- active_data()
      fld <- values$selected_field
      if (is.null(fld) || !(fld %in% names(d))) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid selection",
          "Please select a valid field before choosing values."
        ))
        return(invisible(NULL))
      }
      rows <- input$filterStep2_rows_selected
      if (is.null(rows) || length(rows) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Select Field Values",
          "You must select the field value(s) to exclude."
        ))
        return(invisible(NULL))
      }
      
      vals <- filter_values()
      selected_labels <- unique(vals$Value_label[rows])
      
      new_rows <- data.frame(
        Fields = rep(fld, length(selected_labels)),
        Value  = selected_labels,
        Filter = rep("Exclude", length(selected_labels)),
        Count  = integer(length(selected_labels)),
        stringsAsFactors = FALSE
      )
      tadat$selected_filters <- dplyr::distinct(
        rbind(tadat$selected_filters, new_rows),
        Fields, Value, .keep_all = TRUE
      )
    }
    
    shiny::observeEvent(input$addOnlys, {
      add_filters_include_only()
    })
    
    shiny::observeEvent(input$addExcludes, {
      add_filters_exclude()
    })
    
    # Radio for fields display
    shiny::observeEvent(input$field_sel, {
      tadat$field_sel <- input$field_sel
    })
    
    shiny::observeEvent(tadat$field_sel, {
      shiny::req(tadat$field_sel)
      shiny::updateRadioButtons(session, "field_sel", selected = tadat$field_sel)
      d <- active_data()
      tables$filter_fields <-
        EPATADA::TADA_FieldCounts(d, display = tadat$field_sel) %>%
        dplyr::left_join(filter_dat, by = "Fields") %>%
        dplyr::mutate(Description = ifelse(is.na(Description),
                                           "No description available",
                                           Description))
      tables$filter_fields[
        tables$filter_fields$Fields == "TADA.Media.Flag",
        "Description"
      ] <- "TADA-standardized media fields"
    })
    
    # Reset all filters
    shiny::observeEvent(input$resetFilters, {
      tadat$selected_filters <- data.frame(
        Fields = character(),
        Value  = character(),
        Filter = character(),
        Count  = integer(),
        stringsAsFactors = FALSE
      )
    })
    
    # Remove selected filters
    shiny::observeEvent(input$removeFilters, {
      if (is.null(input$selectedFilters_rows_selected)) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Select Filter",
            "You must select the filter(s) you'd like to remove."
          )
        )
      } else {
        tadat$selected_filters <- tadat$selected_filters[-input$selectedFilters_rows_selected, , drop = FALSE]
      }
    })
    
    # Apply filters, update counts, and removal reasons
    shiny::observeEvent(tadat$selected_filters, {
      prefix <- "Filter: "
      
      # Initialize/remake removals frame
      if (is.null(tadat$removals) && !is.null(tadat$raw)) {
        tadat$removals <- data.frame(matrix(nrow = nrow(tadat$raw), ncol = 0))
      } else if (!is.null(tadat$removals)) {
        tadat$removals <- dplyr::select(tadat$removals, -dplyr::starts_with(prefix))
      }
      
      if (!is.null(tadat$raw)) {
        shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
        
        if (nrow(tadat$selected_filters) > 0) {
          shinyjs::enable("resetFilters")
          shinyjs::enable("removeFilters")
          
          # Apply per-field excludes to tadat$raw
          for (fld in unique(tadat$selected_filters$Fields)) {
            field_filters <- tadat$selected_filters[tadat$selected_filters$Fields == fld, , drop = FALSE]
            data_labels <- labelize(tadat$raw[[fld]])
            sel_labels <- field_filters$Value
            matches <- data_labels %in% sel_labels
            
            # Exclude = remove the matched rows
            to_remove <- matches
            
            all_vals <- paste(unique(sel_labels), collapse = " or ")
            label <- paste0(prefix, "Exclude ", fld, " is ", all_vals)
            
            tadat$removals[[label]] <- as.logical(to_remove)
          }
        }
        
        # Update counts in selected_filters using active_data()
        d <- active_data()
        if (!is.null(d) && nrow(tadat$selected_filters) > 0) {
          new_selected_filters <- tadat$selected_filters
          for (i in seq_len(nrow(new_selected_filters))) {
            row <- new_selected_filters[i, ]
            vals <- getValues(d, as.character(row$Fields))
            new_selected_filters[i, "Count"] <- sum(vals$Count[vals$Value_label == row$Value], na.rm = TRUE)
          }
          tadat$selected_filters <- new_selected_filters
        }
        
        # Update TADA.RemovalReason
        removals_df <- tadat$removals
        if (is.data.frame(removals_df) && nrow(removals_df) == nrow(tadat$raw)) {
          rem_mat <- as.matrix(removals_df)
          sel <- which(rem_mat, arr.ind = TRUE)
          if (!is.null(sel) && nrow(sel) > 0) {
            txt_mat <- matrix("", nrow = nrow(rem_mat), ncol = ncol(rem_mat))
            txt_mat[sel] <- colnames(rem_mat)[sel[, "col"]]
            reasons <- apply(txt_mat, 1, function(row) {
              txt <- paste(row[nzchar(row)], collapse = ", ")
              if (identical(txt, "")) NA_character_ else txt
            })
            tadat$raw$TADA.RemovalReason <- reasons
          } else {
            tadat$raw$TADA.RemovalReason <- NA_character_
          }
        }
      }
    })
  })
}
