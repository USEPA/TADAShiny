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
  org_table <- tadat$org_table
  dataSource <- tadat$dataSource
  dataSourceDesc <- tadat$dataSourceDesc
  selected_flags <- tadat$selected_flags
  m2f <- tadat$m2f
  selected_filters <- tadat$selected_filters
  nd_method <- tadat$nd_method
  od_method <- tadat$od_method
  nd_mult <- tadat$nd_mult
  od_mult <- tadat$od_mult
  print(nd_method)
  print(nd_mult)
  print(od_method)
  print(od_mult)
  
  save(job_id,
       org_table,
       m2f,
       dataSource,
       dataSourceDesc,
       selected_flags,
       selected_filters,
       nd_method,
       od_method,
       nd_mult,
       od_mult,
       file = filename)
}

readFile <- function(tadat, filename) {
  load(filename, verbose = TRUE)
  tadat$load_file = filename
  # Confirm compatibility
  #job_id = job_id
  #shinyjs::disable(selector = '.nav li a[data-value="Overview"]')
  
  # Populate organizational rankings
  if (!is.null(org_table)){
    tadat$org_table <- org_table
  }
  
  # Populate flags
  if (!is.null(selected_flags)){
    tadat$selected_flags = selected_flags
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
  }
  
  if (!is.null(m2f)){
    tadat$m2f = m2f
  }
  
  # Populate filters
  if (!is.null(selected_filters)){
    tadat$selected_filters = selected_filters
    shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
  }
  
  shinyjs::enable(selector = '.nav li a[data-value="Censored"]')
  shinyjs::enable(selector = '.nav li a[data-value="Review"]')

  # Censored data
  tadat$nd_method = nd_method
  tadat$nd_mult = nd_mult
  tadat$od_method = od_method
  tadat$od_mult = od_mult
  }

  

invalidFile <- function(trigger){
  print("Failure")
  print(trigger)
}
