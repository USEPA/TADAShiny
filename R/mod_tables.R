#' tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param tableName Name of the table to output
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

library(EnvStats)

mod_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # Basic data table
    shinycssloaders::withSpinner(DT::DTOutput(ns("table")), type = 6, color ="#045a8d"),
    
    # set the colors of the icons
    tags$style(".fa-info-circle", # quality control color
               ".fa-exclamation-triangle", # error color
               ".fa-exclamation-circle", # warning color
               "table.dataTable tr.odd.selected td {background-color: #f9f9f9 !important}",
               "table.dataTable tr.even.selected td {background-color: #f6f6f6 !important}",
    ), 
    
    shinyalert::useShinyalert(),
    
    br()
    
    
  )
}
# updated +/- class below
js <- c("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
  return '<table><tr><td><b>CASRN</b></td><td>' + d[5] + '</td></tr><tr><td><b>Number of Observations</b></td><td>' + d[13] + '</td></tr><tr><td><b>Maximum Detected Value</b></td><td>' + d[6] + '</td></tr><tr><td><b>Notes</b></td><td>' + d[12] + '</td></tr></table>';
  };
  table.on('click', 'td.details-control', function() {
  
    var td = $(this), row = table.row(td.closest('tr'));
    // td.closest('tr')[0].classList.remove('selected');
    if (row.child.isShown()) {
      row.child.hide();
      td.html('<i class=\"fa fa-plus-circle font-awesome\" role=\"presentation\" aria-label=\"expand-circle icon\" /></i>');
    } else {
      row.child(format(row.data())).show();
      td.html('<i class=\"fa fa-minus-circle font-awesome\" role=\"presentation\" aria-label=\"collapse-circle icon\" /></i>');
      
    }
  });")


#' tables Server Function
#'
#' @param id Name of instance (Same as for UI it is meant to be controlling)
#' @param cached_ucl_array Reactive variable from which to read a processed UCL array.
#' @noRd 


mod_tables_server <- function(id, ucl_array){
  moduleServer(
    id,
    function(input, output, session){
      
      details_selections <- reactiveValues(exposure_unit="", contaminant="", media="")
      
      
      # Define table render handler
      output$table <- DT::renderDT({
        ####  Determine if need to parse data, or just read from cache
        
        #### Render table
        
        # Check if the path is null.
        if (is.null(ucl_array())) {
          # Just render nothing, because no file was uploaded.
          return(NULL)
          
        } else {
          # Apply the processing to the dataframe and render
          results_table <- create_results_summary(ucl_array())
          
          # number of rows in the dataframe
          num_buttons <- length(results_table$Contaminant)
          
          # Quality control buttons
          results_table <- results_table %>% 
            dplyr::rename(qcontrol_og = 'EPC Quality Control Flags') %>% 
            tibble::rowid_to_column("ID") %>% 
            dplyr::mutate(qcontrol = mapply(make_message_button, ID, qcontrol_og, 
                                            MoreArgs=list(which_icon = "info-circle", input_var=session$ns("qcontrol_button")))) %>% 
            dplyr::relocate(qcontrol_og, .after = "Number of Detected Observations") %>% 
            dplyr::relocate(qcontrol, .after = "EPC Type") %>% 
            dplyr::rename('EPC Quality Control Flags' = qcontrol) 
          
          
          # Warnings and eror buttons
          results_table <- results_table %>% 
            dplyr::rename(warning_error = 'R Warnings and Error Flags') %>% 
            dplyr::mutate(warn_er_test = mapply(make_warn_err_button, ID, warning_error, 
                                                MoreArgs=list(input_var=session$ns("warn_err_button")))) %>% 
            dplyr::select(-ID) %>% 
            dplyr::relocate(warning_error, .after = warn_er_test) %>% 
            dplyr::relocate(warn_er_test, .after = "EPC Quality Control Flags") %>% 
            dplyr::rename('R Warnings and Error Flags' = warn_er_test) 
          
          
          #### create a new column for the result details buttons #####
          details_button <- shinyInput(actionButton, num_buttons,
                                       'button_',
                                       label = "Details",
                                       onclick = paste0('Shiny.onInputChange(\"', session$ns("select_button"), '\", this.id, {priority: "event"})'),
                                       class = "btn-secondary"
          )
          
          results_table$'Result Details' <- details_button
          
          # remove if it's a BaP row
          results_table <- results_table %>% 
            dplyr::rename(results_details = 'Result Details') %>% 
            dplyr::mutate(results_details = ifelse(Contaminant == "BaP Equivalent" | CASRN == "NULL-CAS-017", "", results_details)) %>% 
            dplyr::rename('Result Details' = results_details)
          
          ##### Table buttons - what happens when clicked #####
          
          # Quality control buttons
          observeEvent(input$qcontrol_button, {
            
            selectedRow_Q <- as.numeric(strsplit(input$qcontrol_button, "_")[[1]][2])
            
            qc_message <- results_table[selectedRow_Q,]$qcontrol_og
            
            shinyalert::shinyalert("Quality Control Flag", qc_message, type = "info", animation = FALSE)
            
          }, ignoreInit = TRUE)
          
          # Warning and error buttons
          observeEvent(input$warn_err_button, {
            
            selectedRow_WE <- as.numeric(strsplit(input$warn_err_button, "_")[[1]][2])
            
            alert_message <- results_table[selectedRow_WE,]$warning_error
            
            if (stringr::str_detect(alert_message, "ERRORFLAG_")) {
              
              alert_message <- sub("ERRORFLAG_", "", alert_message)
              shinyalert::shinyalert("Error Flag", alert_message, type = "error", animation = FALSE)
              
            } else{
              
              alert_message <- sub("WARNINGFLAG_", "", alert_message)
              shinyalert::shinyalert("Warning Flag", alert_message, type = "warning", animation = FALSE)
              
            }
            
          }, ignoreInit = TRUE)
          
          # Result details buttons
          observeEvent(input$select_button, {
            
            selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
            
            details_selections$exposure_unit <- results_table[selectedRow,]$'Exposure Unit'
            details_selections$contaminant <- results_table[selectedRow,]$Contaminant
            details_selections$media <- results_table[selectedRow,]$Media
            
            
            # navigate to the next tab
            shinyjs::runjs("activeTab('tabs-3-3')")
            
          }, ignoreInit = TRUE)
          
          
          # render the data table
          dt <- DT::datatable(cbind(' ' = '<i class=\"fa fa-plus-circle font-awesome\" role=\"presentation\" aria-label=\"expand-circle icon\" /></i>', results_table), # Add expand symbol to start
                              escape = 1, # Make sure HTML code is not escaped for first column
                              callback=DT::JS(js),
                              options = list(
                                columnDefs = list(
                                  list(visible = FALSE, targets = c(0, 5,6,12,13, 15, 16, 17, 18, 19, 20, 21)),
                                  list(orderable = FALSE, className = 'details-control', targets = 1)
                                )
                              )
          )
          
          return(dt)
        }
      })
      
      
      return(details_selections)
      
      
    }
  )
  
}
