
writeFile <- function(tadat, filename) {
  original_source = tadat$original_source
  job_id = tadat$job_id
  statecode = tadat$statecode
  countycode = tadat$countycode
  example_data = tadat$example_data
  huc = tadat$huc
  siteid = tadat$siteid
  siteType = tadat$siteType
  characteristicName = tadat$characteristicName
  characteristicType = tadat$characteristicType
  sampleMedia = tadat$sampleMedia
  project = tadat$project
  organization = tadat$organization
  startDate = tadat$startDate
  endDate = tadat$endDate
  org_table <- tadat$org_table
  selected_flags <- tadat$selected_flags
  m2f <- tadat$m2f
  selected_filters <- tadat$selected_filters[c("Field", "Value", "Filter")]
  nd_method <- tadat$nd_method
  od_method <- tadat$od_method
  nd_mult <- tadat$nd_mult
  od_mult <- tadat$od_mult
  
  save(
    original_source,
    job_id,
    example_data,
    statecode,
    countycode,
    huc,
    siteid,
    siteType,
    characteristicName,
    characteristicType,
    sampleMedia,
    project,
    organization,
    startDate,
    endDate,
    org_table,
    selected_flags,
    m2f,
    selected_filters,
    nd_method,
    od_method,
    nd_mult,
    od_mult,
    file = filename
  )
  
}

readFile <- function(tadat, filename) {
  load(filename, verbose = FALSE)
  tadat$load_progress_file = filename
<<<<<<< HEAD
=======
  print("Loading progress file")
  print(nrow(tadat$raw))
>>>>>>> a5e2c871b534920ebc52df213410dd6a135a9828
  # Confirm compatibility
  job_id = job_id
  if (!is.null(m2f)) {
    tadat$m2f = m2f
  }

  
  if (!is.null(selected_flags)) {
    tadat$selected_flags = selected_flags
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
  }

  # Enable tabs if certain fields are not null
  if (!is.null(selected_filters)) {
    shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
  }

  tadat$original_source = original_source
  tadat$job_id = job_id
  tadat$example_data = example_data
  tadat$statecode = statecode
  tadat$countycode = countycode
  tadat$huc = huc
  tadat$siteid = siteid
  tadat$siteType = siteType
  tadat$characteristicName = characteristicName
  tadat$characteristicType = characteristicType
  tadat$sampleMedia = sampleMedia
  tadat$project = project
  tadat$organization = organization
  tadat$startDate = startDate
  tadat$endDate = endDate
  tadat$org_table = org_table
  tadat$selected_filters =  selected_filters
  tadat$nd_method = nd_method
  tadat$od_method = od_method
  tadat$nd_mult = nd_mult
  tadat$od_mult
}


invalidFile <- function(trigger) {
  print("Failure")
  print(trigger)
}


writeNarrativeDataFrame <- function(tadat) {
  # sampleMedia needs to be a single string for this part
  tadat$sampleMedia = paste(tadat$sampleMedia, collapse=" ")
  df <- data.frame(Parameter=character(), Value=character())
  df[nrow(df) + 1, ] = c("TADA Shiny Job ID", tadat$job_id)
  df[nrow(df) + 1, ] = c("Original data source: ", tadat$original_source)
  
  # Data Query Tab
  if (tadat$original_source == "Example") {
    df[nrow(df) + 1, ] = c("Example data file", tadat$example_data)
  } else if (tadat$original_source == "Query") {
    query_params = data.frame(
      param = c(
        "State Code",
        "County Code",
        "HUC Code",
        "Site ID",
        "Site Type",
        "Characteristic Name",
        "Characteristic Type",
        "Sample Media",
        "Project Name",
        "Organization Name",
        "Start Date",
        "End Date"
      ),
      value = c(
        tadat$statecode,
        tadat$countycode,
        tadat$huc,
        tadat$siteid,
        tadat$siteType,
        tadat$characteristicName,
        tadat$characteristicType,
        tadat$sampleMedia,
        tadat$project,
        tadat$organization,
        tadat$startDate,
        tadat$endDate
      )
    )
    for (i in seq_len(nrow(query_params))) {
      if (!is.null(query_params[i, "value"])) {
        df[nrow(df) + 1, ] = query_params[i,]
      }
    }
  }

  # Overview Tab
  for (row in 1:nrow(tadat$org_table)) {
    df[nrow(df) + 1, ] = c(paste0("Organization Rank ", row), tadat$org_table[row, 'OrganizationFormalName'])
  }
  

  # Flagging Tab
  for (flag in tadat$selected_flags) {
    df[nrow(df) + 1, ] = c("Selected Flag", flag)
  }
  

  if (!is.null(tadat$m2f)) {
    df[nrow(df) + 1, ] = c("Depth unit conversion", tadat$m2f)
  } else {
    df[nrow(df) + 1, ] = c("Depth unit conversion", "None")
  }
  

  # Filtering tab
  for (row in 1:nrow(tadat$selected_filters)) {
    df[nrow(df) + 1, ] = c(
      "Selected Filter",
      paste0(
        tadat$selected_filters[row, 'Filter'],
        ": ",
        tadat$selected_filters[row, 'Field'],
        " = ",
        tadat$selected_filters[row, 'Value']
      )
    )
  }
  
  # Censored Data tab
  if (is.null(tadat$nd_mult)){
    tadat$nd_mult = "n/a"
  }
  if (is.null(tadat$od_mult)){
    tadat$od_mult = "n/a"
  }
  df[nrow(df) + 1, ] = c("Non-Detect Handling Method", sub("x",  tadat$nd_mult, tadat$nd_method))
  df[nrow(df) + 1, ] = c("Over-Detect Handling Method", sub("x", tadat$od_mult, tadat$od_method))
  
  return(df)
}
