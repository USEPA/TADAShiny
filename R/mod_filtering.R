mod_filtering_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::actionButton(ns("describe_removals"), "Describe Removals"),
    shiny::htmlOutput(ns("promptStep1")),
    DT::dataTableOutput(ns("filterStep1")),
    htmltools::br(),
    shiny::htmlOutput(ns("promptStep2")),
    DT::DTOutput(ns("filterStep2")),
    shiny::actionButton(ns("addOnlys"),
                        "Include Only These Values",
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    shiny::actionButton(ns("addExcludes"),
                        "Exclude These Values",
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    htmltools::br(),
    htmltools::br(),
    shiny::htmlOutput(ns("promptStep3")),
    DT::DTOutput(ns("selectedFilters")),
    shiny::actionButton(ns("removeFilters"),
                        "Remove These Filters",
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    shiny::actionButton(ns("resetFilters"),
                        "Reset All Filters",
                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
  )
}



mod_filtering_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tables = shiny::reactiveValues()
    values = shiny::reactiveValues()
    values$locked <- character()
    values$selected_field <- NULL
    shinyjs::hide("addOnlys")
    shinyjs::hide("addExcludes")
    
    output$filterStep2 = DT::renderDataTable(NULL)
    tables$selected <-
      data.frame(matrix(
        ncol = 4,
        nrow = 0,
        dimnames = list(NULL, c("Field", "Value", "Filter", "Count"))
      ))
    
    
    output$promptStep3 = shiny::renderUI(HTML("<h3>Selected Filters</h3>"))
    output$selectedFilters =  DT::renderDT(
      tables$selected,
      escape = FALSE,
      selection = 'multiple',
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        language = list(zeroRecords = "No filters selected")
      )
    )
    
    
    output$promptStep1 = shiny::renderUI(HTML("<h4>Select field to filter on:</h4>"))
    
    shiny::observeEvent(tadat$ovgo, {
      tables$filter_fields = getFieldNames(tadat$raw)
      output$filterStep1 = DT::renderDT(
        tables$filter_fields,
        escape = FALSE,
        selection = 'single',
        rownames = FALSE,
        options = list(dom = 't',
                       paging = FALSE)
      )
    })
    
    shiny::observeEvent(input$filterStep1_rows_selected, {
      # Get the name of the selected field
      values$selected_field = tables$filter_fields[input$filterStep1_rows_selected,]$Field
      applyLocks()
      tables$filter_values = data.frame(getValues(tadat$raw, values$selected_field))
      output$promptStep2 = shiny::renderUI(HTML(
        paste0("<h4>Filter by '", values$selected_field, "'</h4>")
      ))
      output$filterStep2 = DT::renderDT(
        tables$filter_values,
        escape = FALSE,
        selection = 'multiple',
        rownames = FALSE,
        options = list(dom = 't', paging = TRUE)
      )
      shinyjs::show("addOnlys")
      shinyjs::show("addExcludes")
    })
    
    shiny::observeEvent(input$addOnlys, {
      selectFilters("Only")
    })
    
    shiny::observeEvent(input$addExcludes, {
      selectFilters("Exclude")
    })
    
    shiny::observeEvent(input$resetFilters, {
      tables$selected = tables$selected[0, ]
    })
    
    shiny::observeEvent(input$removeFilters, {
      current_field = values$selected_field
      tables$selected = tables$selected[-input$selectedFilters_rows_selected, ]
    })
    
    selectFilters <- function(Filter) {
      values$locked[values$selected_field] = Filter
      rows = input$filterStep2_rows_selected
      Field = values$selected_field
      Value = tables$filter_values[rows, "Value"]
      Count = tables$filter_values[rows, "Count"]
      new_rows = data.frame(Field, Value, Filter, Count)
      tables$selected = rbind(tables$selected, new_rows)
      tables$selected =
        tables$selected %>% dplyr::distinct(Field, Value, .keep_all = TRUE)
      
    }
    
    shiny::observeEvent(tables$selected, {
      still_present = intersect(names(values$locked), unique(tables$selected$Field))
      values$locked = values$locked[still_present]
    })
    
    applyLocks <- function() {
      if (!is.null(values$selected_field)) {
        active_lock <- values$locked[values$selected_field]
        if (is.na(active_lock)) {
          shinyjs::enable("addOnlys")
          shinyjs::enable("addExcludes")
        } else if (active_lock == "Only") {
          shinyjs::enable("addOnlys")
          shinyjs::disable("addExcludes")
        } else {
          shinyjs::disable("addOnlys")
          shinyjs::enable("addExcludes")
        }
      } else{
        shinyjs::disable("addOnlys")
        shinyjs::disable("addExcludes")
      }
    }
    
    shiny::observeEvent(values$locked, {
      applyLocks()
    })
    
    getFieldNames <- function(.data) {
      valid_fields = c(
        "ActivityTypeCode",
        "TADA.ActivityMediaName",
        "ActivityMediaSubdivisionName",
        "ActivityCommentText",
        "MonitoringLocationTypeName",
        "StateName",
        "TribalLandName",
        "OrganizationFormalName",
        "TADA.CharacteristicName",
        "HydrologicCondition",
        "HydrologicEvent",
        "BiologicalIntentName",
        "MeasureQualifierCode",
        "ActivityGroup",
        "AssemblageSampledName",
        "ProjectName",
        "CharacteristicNameUserSupplied",
        "DetectionQuantitationLimitTypeName",
        "SampleTissueAnatomyName",
        "LaboratoryName"
      )
      fields <- intersect(colnames(.data), valid_fields)
      counts <-
        apply(.data[fields], 2, function(x)
          length(unique(x[!is.na(x)])))
      field_table = data.frame(Field = names(counts), Count = as.vector(counts))
      field_table = field_table[field_table$Count > 0,]
      return(field_table)
    }
    
    # This gets run whenever a change in selected filters happens
    shiny::observeEvent(tables$selected, {
      prefix = "Filter: "
      # Remove all the filter columns from the removals table (start fresh)
      if (!is.null(tadat$removals)) {
        tadat$removals = dplyr::select(tadat$removals,-starts_with(prefix))
      }
      
      # Only proceed if filters have been selected
      if (nrow(tables$selected) > 0) {
        # Since filters have been added, enable the ability to reset them
        shinyjs::enable("resetFilters")
        shinyjs::enable("removeFilters")
        
        # Loop through the filters field-by-field
        for (active_field in unique(tables$selected$Field)) {
          filter_type <- values$locked[active_field]
          field_filters = tables$selected[tables$selected$Field == active_field, ]
          results <- rep(FALSE, nrow(tadat$raw))
          for (row in 1:nrow(field_filters)) {
            sel = (tadat$raw[[active_field]] == field_filters[row, "Value"])
            sel[is.na(sel)] <- FALSE
            results = results | sel
          }
          # Get the intersection of all the places where True
          if (filter_type == "Only") {
            results = !results
          }
          all_vals = paste(field_filters$Value, collapse = " or ")
          label = paste0(prefix, filter_type, " ", active_field, " is ", all_vals)
          tadat$removals[label] = as.logical(results)
          
        }
      }
    })
    
    shiny::observeEvent(input$describe_removals, {
      removals <- tadat$removals
      sel <- which(removals == TRUE, arr.ind = TRUE)
      removals[sel] <- names(removals)[sel[, "col"]]
      removals[removals == FALSE] = ""
      tadat$raw$RemovalReason = apply(removals, 1,
                                      function(row)
                                        paste(row[nzchar(row)], collapse = ", "))
      print(unique(tadat$raw$RemovalReason))
    })
    
    
    getValues <- function(.data, field) {
      counts = table(.data[[field]])
      if (length(rownames(counts) > 0)) {
        value_table = data.frame(Value = names(counts), Count = as.vector(counts))
      } else {
        value_table = data.frame(Value = character(), Count = integer())
      }
      return(value_table)
    }
  })
}