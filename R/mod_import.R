#' import UI Function
#'
#' @description User uploads Xls files
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # widget to upload csv
    fileInput(
      ns("file"), "",
      multiple = TRUE,
      accept = c(".xlsx", ".xls"),
      width = "100%"
    )
    
  )
}

#' import Server Function
#'
#' @noRd 

mod_import_server <- function(id, epc_data) {
  moduleServer(
    
    # Set ID
    id,
    
    # Define core mechanism
    function(input, output, session){
      
      
      # reactive file path
      input_filepath_set <- reactive({
        
        epc_data(NULL) # reset user input
        req(input$file$datapath) # require a datapath has been uploaded to run
        
        # user uploaded data
        uploaded_data <- readxl::read_excel(input$file$datapath, sheet = 1) 
        
        #change detected flag to nondetected flag
        if ("DetectedFlag" %in% names(uploaded_data)) { 
          
          uploaded_data <- uploaded_data %>% 
            dplyr::mutate(NotDetectedFlag = ifelse(DetectedFlag == 0, 1, 0))
        }
        
        return(uploaded_data)
        
      })
      
      
      
      # Return the reactive value
      return(input_filepath_set)
    }
  )
}


## To be copied in the UI
# mod_import_ui("import_ui_1")
    
## To be copied in the server
# mod_import_server("import_ui_1")
