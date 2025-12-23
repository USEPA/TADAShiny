# Load the input data (keep your package's app_sys)
data_path1 <- app_sys("extdata/filter_descriptions.RData")
load(data_path1)

mod_filtering_ui <- function(id) {
  ns <- shiny::NS(id)
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
    shiny::uiOutput(ns("promptStep2")),
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
      paste0(
        "Your exclude/include filters are documented below. You can easily reset one or more filter decisions by clicking on rows of interest and then hitting the 'Remove Selected Filters' button. ",
        "Alternatively, you can reset/remove all filters on this page by clicking the 'Reset All Filters' button.<br><br>",
        "<em>Note:</em> 'Include Only Selected Values' is implemented by excluding all other values. ",
        "As a result, your selections will appear as 'Exclude' entries for the complementary values in the 'Selected filters' table."
      )
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

    # Ensure filter_dat exists
    if (!exists("filter_dat", inherits = TRUE)) {
      filter_dat <- data.frame(Fields = character(), Description = character(), stringsAsFactors = FALSE)
    }

    # Prefix for module-generated removals
    prefix <- "Filter (module): "

    # Unified UI label for missing values
    na_label <- "NA - Not Available"

    # Robust labelizer
    labelize <- function(v) {
      if (is.list(v)) {
        chr <- vapply(v, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
      } else {
        chr <- tryCatch(as.character(v), error = function(e) rep(NA_character_, length(v)))
      }
      chr <- trimws(chr)
      chr_upper <- toupper(chr)
      is_missing <- is.na(chr) | chr == "" | chr_upper %in% c("NA", "NULL", "NAN")
      chr[is_missing] <- na_label
      chr
    }

    # Keep mask honoring TADA.Remove and all removals; optionally ignore this field's own removal columns
    keep_mask_for <- function(fld = NULL) {
      d <- tadat$raw
      if (is.null(d)) {
        return(logical(0))
      }

      # TADA.Remove mask
      keep_tada <- rep(TRUE, nrow(d))
      if ("TADA.Remove" %in% names(d)) {
        rmv <- suppressWarnings(as.logical(d$TADA.Remove))
        rmv[is.na(rmv)] <- FALSE
        keep_tada <- !rmv
      }

      rem <- tadat$removals
      if (!is.data.frame(rem) || ncol(rem) == 0 || nrow(rem) != nrow(d)) {
        keep_rem <- rep(TRUE, nrow(d))
      } else {
        rem <- as.data.frame(lapply(rem, function(col) if (is.logical(col)) col else as.logical(col)))
        if (!is.null(fld)) {
          prefixes <- c(
            paste0("Filter (module): Exclude ", fld, " is "),
            paste0("Filter: Exclude ", fld, " is ")
          )
          drop_cols <- vapply(colnames(rem), function(nm) any(startsWith(nm, prefixes)), logical(1))
          rem <- rem[, !drop_cols, drop = FALSE]
        }
        keep_rem <- if (ncol(rem) == 0) rep(TRUE, nrow(d)) else rowSums(rem, na.rm = TRUE) == 0
      }

      keep_tada & keep_rem
    }

    # Active dataset
    active_data <- shiny::reactive({
      d <- tadat$raw
      shiny::req(d)
      keep <- keep_mask_for(NULL)
      d[keep, , drop = FALSE]
    })

    # Value counting with labelize
    getValues <- function(.data, field) {
      if (is.null(.data) || is.null(field) || !(field %in% names(.data)) || nrow(.data) == 0) {
        return(data.frame(Value_label = character(), Count = integer(), stringsAsFactors = FALSE))
      }
      lab <- labelize(.data[[field]])
      if (length(lab) == 0) {
        return(data.frame(Value_label = character(), Count = integer(), stringsAsFactors = FALSE))
      }
      counts <- table(lab)
      data.frame(Value_label = names(counts), Count = as.integer(counts), stringsAsFactors = FALSE)
    }

    # Step 1: field list
    shiny::observeEvent(list(active_data(), input$field_sel), {
      d <- active_data()
      shiny::req(d)
      display_mode <- if (!is.null(input$field_sel)) input$field_sel else "key"
      tables$filter_fields <-
        EPATADA::TADA_FieldCounts(d, display = display_mode) %>%
        dplyr::left_join(filter_dat, by = "Fields") %>%
        dplyr::mutate(Description = ifelse(is.na(Description), "No description available", Description))
      tables$filter_fields[
        tables$filter_fields$Fields == "TADA.Media.Flag",
        "Description"
      ] <- "TADA-standardized media fields"

      if (!is.null(values$selected_field) && !(values$selected_field %in% names(d))) {
        values$selected_field <- NULL
        shinyjs::hide("addOnlys")
        shinyjs::hide("addExcludes")
        output$promptStep2 <- shiny::renderUI(htmltools::HTML("<p>No valid field selected.</p>"))
      }

      DT::selectRows(DT::dataTableProxy("filterStep1", session = session), NULL)
    })

    output$filterStep1 <- DT::renderDT(
      {
        shiny::req(tables$filter_fields)
        tables$filter_fields
      },
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = max(1L, nrow(tables$filter_fields)),
        paging = FALSE,
        ordering = FALSE
      )
    )

    # Pie chart source
    pie_source <- shiny::reactive({
      d <- tadat$raw
      shiny::req(d)
      fld <- values$selected_field
      shiny::req(!is.null(fld), fld %in% names(d))
      keep <- keep_mask_for(fld)
      d2 <- d[keep, , drop = FALSE]
      d2[[fld]] <- labelize(d2[[fld]])
      d2
    })

    output$filter_pie_chart <- shiny::renderPlot({
      fld <- values$selected_field
      shiny::req(!is.null(fld))
      d <- tadat$raw
      if (is.null(d) || nrow(d) == 0 || !(fld %in% names(d))) {
        graphics::plot.new()
        graphics::title("No data to display")
        return(invisible())
      }
      d2 <- pie_source()
      if (nrow(d2) == 0) {
        graphics::plot.new()
        graphics::title("No data to display")
        return(invisible())
      }
      EPATADA::TADA_FieldValuesPie(d2, field = fld)
    })

    # Step 1 selection => Step 2 setup
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
          "Then choose whether to exclude those values, or keep only those values.</p>",
          "<p><em>Note:</em> 'Include Only Selected Values' is implemented by excluding all other values; ",
          "your selections will therefore appear as 'Exclude' filters for the complementary values in the table below.</p>"
        )
      ))
      shinyjs::show("addOnlys")
      shinyjs::show("addExcludes")
    })

    # Step 2 values
    filter_values <- shiny::reactive({
      fld <- values$selected_field
      d <- active_data()
      if (!is.null(fld) && !is.null(d) && fld %in% names(d)) {
        getValues(d, fld)
      } else {
        data.frame(Value_label = character(), Count = integer(), stringsAsFactors = FALSE)
      }
    })

    output$filterStep2 <- DT::renderDT(
      {
        vals <- filter_values()
        data.frame(Value = vals$Value_label, Count = vals$Count, stringsAsFactors = FALSE)
      },
      escape = FALSE,
      selection = "multiple",
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE,
        paging = TRUE,
        pageLength = 20
      ),
      server = TRUE
    )

    # Clear selection only when the selected field changes
    shiny::observeEvent(values$selected_field, {
      DT::selectRows(DT::dataTableProxy("filterStep2", session = session), NULL)
    })

    # Init selected filters if absent
    shiny::observeEvent(TRUE,
      {
        if (is.null(shiny::isolate(tadat$selected_filters))) {
          tadat$selected_filters <- data.frame(
            Fields = character(),
            Value = character(),
            Filter = character(),
            Count = integer(),
            stringsAsFactors = FALSE
          )
        }
      },
      once = TRUE
    )

    # Selected filters counts from raw + selected_filters
    compute_selected_filter_counts <- function(sf) {
      if (is.null(sf) || nrow(sf) == 0) {
        return(integer(0))
      }
      raw <- tadat$raw
      out <- integer(nrow(sf))
      if (is.null(raw) || nrow(raw) == 0) {
        return(out)
      }

      for (fld in unique(sf$Fields)) {
        idx <- which(sf$Fields == fld)
        if (!(fld %in% names(raw))) {
          out[idx] <- 0L
          next
        }

        data_labels <- labelize(raw[[fld]])
        field_values <- sf$Value[idx]
        removed_mask <- data_labels %in% field_values
        removed_tbl <- table(data_labels[removed_mask])
        removed_map <- stats::setNames(as.integer(removed_tbl), names(removed_tbl))
        counts <- as.integer(removed_map[field_values])
        counts[is.na(counts)] <- 0L
        out[idx] <- counts
      }
      out
    }

    output$selectedFilters <- DT::renderDT(
      {
        shiny::req(tadat$selected_filters)
        sf <- tadat$selected_filters
        if (nrow(sf) > 0) {
          sf$Count <- compute_selected_filter_counts(sf)
        }
        sf[, c("Fields", "Value", "Filter", "Count"), drop = FALSE]
      },
      escape = FALSE,
      selection = "multiple",
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        language = list(zeroRecords = "No filters selected")
      )
    )

    # Add selections from Step 2: Include Only (stored as complement 'Exclude')
    add_filters_include_only <- function() {
      fld <- values$selected_field
      if (is.null(fld)) {
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

      # Selected labels (already labelized via filter_values -> getValues -> labelize)
      vals <- filter_values()
      selected_labels <- unique(vals$Value_label[rows])

      # Universe: ALL labels in raw for this field (labelized), not just those kept by other filters
      base <- tadat$raw
      if (is.null(base) || !(fld %in% names(base))) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid selection",
          "Selected field is not present in the current dataset."
        ))
        return(invisible(NULL))
      }
      all_labels <- unique(labelize(base[[fld]]))

      # Complement = everything except the selected labels
      complement_labels <- setdiff(all_labels, selected_labels)

      # Replace any existing filters for this field, then store complement as 'Exclude' rows
      tadat$selected_filters <- tadat$selected_filters[!(tadat$selected_filters$Fields == fld), , drop = FALSE]

      if (length(complement_labels) > 0) {
        new_rows <- data.frame(
          Fields = rep(fld, length(complement_labels)),
          Value = complement_labels, # labelized
          Filter = rep("Exclude", length(complement_labels)),
          Count = integer(length(complement_labels)),
          stringsAsFactors = FALSE
        )
        tadat$selected_filters <- dplyr::distinct(
          rbind(tadat$selected_filters, new_rows),
          Fields, Value,
          .keep_all = TRUE
        )
      }

      shiny::showNotification(sprintf("Applied 'Include Only' to %d value(s) for %s", length(selected_labels), fld),
        type = "message", duration = 3
      )
    }

    # Exclude: union with existing
    add_filters_exclude <- function() {
      d <- active_data()
      fld <- values$selected_field
      if (is.null(fld) || !(fld %in% names(d))) {
        shiny::showModal(shiny::modalDialog(title = "Invalid selection", "Please select a valid field before choosing values."))
        return(invisible(NULL))
      }

      rows <- input$filterStep2_rows_selected
      if (is.null(rows) || length(rows) == 0) {
        shiny::showModal(shiny::modalDialog(title = "Select Field Values", "You must select the field value(s) to exclude."))
        return(invisible(NULL))
      }

      vals <- filter_values()
      selected_labels <- unique(vals$Value_label[rows])

      existing_field <- tadat$selected_filters[tadat$selected_filters$Fields == fld, , drop = FALSE]
      existing_vals <- if (nrow(existing_field) > 0) unique(existing_field$Value) else character(0)

      updated_excluded_vals <- sort(unique(c(existing_vals, selected_labels)))

      tadat$selected_filters <- tadat$selected_filters[tadat$selected_filters$Fields != fld, , drop = FALSE]

      if (length(updated_excluded_vals) > 0) {
        new_rows <- data.frame(
          Fields = rep(fld, length(updated_excluded_vals)),
          Value = updated_excluded_vals,
          Filter = rep("Exclude", length(updated_excluded_vals)),
          Count = integer(length(updated_excluded_vals)),
          stringsAsFactors = FALSE
        )
        tadat$selected_filters <- dplyr::distinct(
          rbind(tadat$selected_filters, new_rows),
          Fields, Value,
          .keep_all = TRUE
        )
      }

      shiny::showNotification(sprintf("Excluded %d value(s) for %s", length(selected_labels), fld), type = "message", duration = 3)
    }

    # Wire buttons
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
      if (!identical(input$field_sel, tadat$field_sel)) {
        shiny::updateRadioButtons(session, "field_sel", selected = tadat$field_sel)
      }
    })

    # Reset all filters
    shiny::observeEvent(input$resetFilters, {
      tadat$selected_filters <- data.frame(
        Fields = character(),
        Value = character(),
        Filter = character(),
        Count = integer(),
        stringsAsFactors = FALSE
      )
      shinyjs::hide("addOnlys")
      shinyjs::hide("addExcludes")
    })

    # Remove selected filters
    shiny::observeEvent(input$removeFilters, {
      if (is.null(input$selectedFilters_rows_selected) || length(input$selectedFilters_rows_selected) == 0) {
        shiny::showModal(shiny::modalDialog(title = "Select Filter", "You must select the filter(s) you'd like to remove."))
      } else {
        tadat$selected_filters <- tadat$selected_filters[-input$selectedFilters_rows_selected, , drop = FALSE]
      }
    })

    # Show/hide buttons based on valid field
    shiny::observeEvent(
      list(values$selected_field, tadat$selected_filters, tadat$raw),
      {
        fld <- values$selected_field
        raw <- tadat$raw
        valid_field <- !is.null(fld) && !is.null(raw) && (fld %in% names(raw))
        if (valid_field) {
          shinyjs::show("addOnlys")
          shinyjs::show("addExcludes")
        } else {
          shinyjs::hide("addOnlys")
          shinyjs::hide("addExcludes")
        }
      },
      ignoreInit = FALSE
    )

    # Apply filters and update removal reasons (per-field updates; guard heavy work)
    shiny::observeEvent(tadat$selected_filters, {
      try(
        {
          if (is.null(tadat$raw)) {
            return()
          }

          # Ensure removals exists and has correct nrow
          if (is.null(tadat$removals) || !is.data.frame(tadat$removals) || nrow(tadat$removals) != nrow(tadat$raw)) {
            tadat$removals <- data.frame(matrix(nrow = nrow(tadat$raw), ncol = 0))
          }

          # Apply per-field excludes: remove prior module columns for that field, then add/update new one
          if (nrow(tadat$selected_filters) > 0) {
            shinyjs::enable("resetFilters")
            shinyjs::enable("removeFilters")

            for (fld in unique(tadat$selected_filters$Fields)) {
              if (!(fld %in% names(tadat$raw))) next

              # Drop this field's prior module columns
              prior_prefix <- paste0(prefix, "Exclude ", fld, " is ")
              drop_idx <- which(startsWith(colnames(tadat$removals), prior_prefix))
              if (length(drop_idx) > 0) {
                tadat$removals <- tadat$removals[, -drop_idx, drop = FALSE]
              }

              field_filters <- tadat$selected_filters[tadat$selected_filters$Fields == fld, , drop = FALSE]
              data_labels <- labelize(tadat$raw[[fld]])
              sel_labels <- unique(field_filters$Value)

              to_remove <- data_labels %in% sel_labels
              all_vals <- paste(sel_labels, collapse = " or ")
              label <- paste0(prefix, "Exclude ", fld, " is ", all_vals)

              tadat$removals[[label]] <- as.logical(to_remove)
            }
          } else {
            shinyjs::disable("resetFilters")
            shinyjs::disable("removeFilters")
          }

          # Update TADA.RemovalReason (fast guard paths)
          removals_df <- tadat$removals
          if (is.data.frame(removals_df) &&
            nrow(removals_df) == nrow(tadat$raw) &&
            ncol(removals_df) > 0) {
            # Coerce to logical to avoid surprises
            rem_log <- as.data.frame(lapply(removals_df, function(col) if (is.logical(col)) col else as.logical(col)))
            cn <- colnames(rem_log)
            mat <- as.matrix(rem_log)

            any_true <- rowSums(mat, na.rm = TRUE) > 0
            reasons <- rep(NA_character_, nrow(mat))
            if (any(any_true)) {
              idx_list <- apply(mat[any_true, , drop = FALSE], 1L, function(row) which(row))
              if (is.integer(idx_list)) idx_list <- list(idx_list)
              reasons[any_true] <- vapply(idx_list, function(idx) paste(cn[idx], collapse = ", "), character(1))
            }
            tadat$raw$TADA.RemovalReason <- reasons
          } else if (is.data.frame(removals_df)) {
            tadat$raw$TADA.RemovalReason <- NA_character_
          }
        },
        silent = TRUE
      )
    })

    # Enable/disable reset/remove buttons based on filter presence
    shiny::observe({
      if (nrow(tadat$selected_filters) > 0) {
        shinyjs::enable("resetFilters")
        shinyjs::enable("removeFilters")
      } else {
        shinyjs::disable("resetFilters")
        shinyjs::disable("removeFilters")
      }
    })
  })
}
