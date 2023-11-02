tsf_prompts = c("Job id:", "Data source:", "Selected flags:", "Selected filters:")

trackEverything <- function(tadat, input) {
  #print("Memory:")
  #trackMemory(tadat, input)
  #print("Input:")
  #print(input)
  #print("Switches: ")
  #switch_vals = getFlags(input)
  #print(switch_vals)
}

trackMemory <- function(tadat, input) {
  print("  All: ")
  print(ls())
  print(input)
  if (length(names(tadat)) > 0) {
    print(" tadat:")
    for (i in 1:length(names(tadat))) {
      name <- names(tadat)[i]
      size <- as.numeric(utils::object.size(tadat[[name]]))
      print(paste0("    ", name, ": ", size))
    }
  }

}


writeFile <- function(tadat, filename) {
  job_id <- paste0("ts", format(Sys.time(), "%y%m%d%H%M%S"))
  flagSwitches <-
    dataSource <- tadat$dataSource
  dataSourceDesc <- tadat$dataSourceDesc
  selected_flags <- tadat$selected_flags
  selected_filters <- tadat$selected_filters
  save(job_id,
       dataSource,
       dataSourceDesc,
       selected_flags,
       selected_filters,
       file = filename)
}

readFile <- function(tadat, filename) {
  load(filename, verbose = TRUE)

  # Confirm compatibility
  #job_id = job_id
  #shinyjs::disable(selector = '.nav li a[data-value="Overview"]')

  # Populate flags
  if (!is.null(selected_flags)){
    tadat$selected_flags = selected_flags
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
  }
  
  # Populate filters
  if (!is.null(selected_filters)){
    tadat$selected_filters = selected_filters
    shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
  }
  
  shinyjs::enable(selector = '.nav li a[data-value="Censored"]')
  shinyjs::enable(selector = '.nav li a[data-value="Review"]')
}

invalidFile <- function(trigger){
  print("EPIC FAIL")
  print(trigger)
}
