writeFile <- function(tadat, filename) {
  original_source <- tadat$original_source
  job_id <- tadat$job_id
  statecode <- tadat$statecode
  countycode <- tadat$countycode
  example_data <- tadat$example_data
  # huc <- tadat$huc
  siteid <- tadat$siteid
  siteType <- tadat$siteType
  characteristicName <- tadat$characteristicName
  characteristicType <- tadat$characteristicType
  sampleMedia <- tadat$sampleMedia
  project <- tadat$project
  organization <- tadat$organization
  startDate <- tadat$startDate
  endDate <- tadat$endDate
  org_table <- tadat$org_table
  selected_flags <- tadat$selected_flags
  m2f <- tadat$m2f
  selected_filters <- tadat$selected_filters[c("Fields", "Value", "Filter")] # was crashing here.  Changed string 'Field' to 'Fields'
  nd_method <- tadat$nd_method
  od_method <- tadat$od_method
  nd_mult <- tadat$nd_mult
  od_mult <- tadat$od_mult
  field_sel <- tadat$field_sel

  save(
    original_source,
    job_id,
    example_data,
    statecode,
    countycode,
    # huc,
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
    field_sel,
    file = filename
  )
}

readFile <- function(tadat, filename) {
  newVals <- c()
  critical_missing <- c()
  n_missing <- 0
  load_attribute <- function(attribute, attribute_name, required = TRUE) {
    if (is.null(attribute) & !required) {
      print(paste("Missing required parameter ", attribute))
      n_missing <- n_missing + 1
      critical_missing[n_missing] <- attribute_name
    }
    return(attribute)
  }

  load(filename, verbose = FALSE)
  tadat$load_progress_file <- filename

  # Confirm compatibility
  job_id <- job_id
  if (!is.null(m2f)) {
    tadat$m2f <- m2f
  }

  if (!is.null(selected_flags)) {
    tadat$selected_flags <- selected_flags
  }

  # Enable tabs if certain fields are not null
  if (!is.null(selected_filters)) {}

  newVals$original_source <- original_source
  newVals$job_id <- load_attribute(job_id, "job_id")
  newVals$example_data <- load_attribute(example_data, "example_data")
  newVals$statecode <- load_attribute(statecode, "statecode")
  newVals$countycode <- load_attribute(countycode, "countycode")
  # newVals$huc <- load_attribute(huc, 'huc')
  newVals$siteid <- load_attribute(siteid, "siteid")
  newVals$siteType <- load_attribute(siteType, "siteType")
  newVals$characteristicName <- load_attribute(
    characteristicName,
    "characteristicName"
  )
  newVals$characteristicType <- load_attribute(
    characteristicType,
    "characteristicType"
  )
  newVals$sampleMedia <- load_attribute(sampleMedia, "sampleMedia")
  newVals$project <- load_attribute(project, "project")
  newVals$organization <- load_attribute(organization, "organization")
  newVals$startDate <- load_attribute(startDate, "startDate")
  newVals$endDate <- load_attribute(endDate, "endDate")
  newVals$org_table <- load_attribute(org_table, "org_table")
  newVals$selected_filters <- load_attribute(
    selected_filters,
    "selected_filters"
  )
  newVals$nd_method <- load_attribute(nd_method, "nd_method")
  newVals$od_method <- load_attribute(od_method, "od_method")
  newVals$nd_mult <- load_attribute(nd_mult, "nd_mult")
  newVals$od_mult <- load_attribute(od_mult, "od_mult")
  newVals$field_sel <- load_attribute(field_sel, "field_sel")

  if (n_missing > 0) {
    # shiny::showNotification(
    #   paste("Unable to load progress file. Missing fields: ", critical_missing)
    # )

    shiny::showNotification(
      ui = tagList(
        htmltools::h4(htmltools::strong("Error")),
        htmltools::hr(style = "margin-top: 5px; margin-bottom: 5px;"), # Adds a separator line
        paste(
          "Unable to load progress file. Missing fields: ",
          critical_missing
        )
      ),
      type = "error",
      duration = NULL,
      id = "uploadProgressFileError"
    )
  } else {
    updateExisting(tadat, newVals)
    shiny::showNotification("Successfully loaded progress file")
  }
}


invalidFile <- function(trigger) {
  print("Failure: Invalid File")
  # print(trigger)
}


writeNarrativeDataFrame <- function(tadat) {
  # sampleMedia, siteType, characteristicName, and characteristicType need to be a single string for this part
  # Others? Automatic check?
  tadat$sampleMedia <- paste(tadat$sampleMedia, collapse = " ")
  tadat$siteType <- paste(tadat$siteType, collapse = " ")
  tadat$characteristicType <- paste(tadat$characteristicType, collapse = " ")
  tadat$characteristicName <- paste(tadat$characteristicName, collapse = " ")
  tadat$project <- paste(tadat$project, collapse = " ")
  df <- data.frame(Parameter = character(), Value = character())
  df[nrow(df) + 1, ] <- c("TADA Shiny Job ID", tadat$job_id)
  df[nrow(df) + 1, ] <- c("Original data source: ", tadat$original_source)

  # Data Query Tab
  if (tadat$original_source == "Example") {
    df[nrow(df) + 1, ] <- c("Example data file", tadat$example_data)
  } else if (tadat$original_source == "Query") {
    query_params <- data.frame(
      param = c(
        "State Code",
        "County Code",
        # "HUC Code",
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
        # tadat$huc,
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
        df[nrow(df) + 1, ] <- query_params[i, ]
      }
    }
  }

  # Overview Tab
  for (row in 1:nrow(tadat$org_table)) {
    df[nrow(df) + 1, ] <- c(
      base::paste0("Organization Rank ", row),
      tadat$org_table[row, "OrganizationFormalName"]
    )
  }

  # Flagging Tab
  for (flag in tadat$selected_flags) {
    df[nrow(df) + 1, ] <- c("Selected Flag", flag)
  }

  if (!is.null(tadat$m2f)) {
    df[nrow(df) + 1, ] <- c("Depth unit conversion", tadat$m2f)
  } else {
    df[nrow(df) + 1, ] <- c("Depth unit conversion", "None")
  }

  # Filtering tab
  # skips the recording of selected filters in the progress file if the filters haven't been selected yet
  if (nrow(tadat$selected_filters) > 0) {
    for (row in 1:nrow(tadat$selected_filters)) {
      val <- base::paste0(
        tadat$selected_filters[row, "Filter"],
        ": ",
        tadat$selected_filters[row, "Field"],
        " = ",
        tadat$selected_filters[row, "Value"]
      )
      new_entry <- c("Selected Filter", val)
      df[nrow(df) + 1, ] <- new_entry
    }
  }

  # Censored Data tab
  if (is.null(tadat$nd_mult)) {
    tadat$nd_mult <- "n/a"
  }
  if (is.null(tadat$od_mult)) {
    tadat$od_mult <- "n/a"
  }
  df[nrow(df) + 1, ] <- c(
    "Non-Detect Handling Method",
    sub("x", tadat$nd_mult, tadat$nd_method)
  )
  df[nrow(df) + 1, ] <- c(
    "Over-Detect Handling Method",
    sub("x", tadat$od_mult, tadat$od_method)
  )

  return(df)
}

fetchExisting <- function(tadat) {
  existingVals <- list()
  existingVals$original_source <- tadat$original_source
  existingVals$job_id <- tadat$job_id
  existingVals$example_data <- tadat$example_data
  existingVals$statecode <- tadat$statecode
  existingVals$countycode <- tadat$countycode
  # existingVals$huc <- tadat$huc
  existingVals$siteid <- tadat$siteid
  existingVals$siteType <- tadat$siteType
  existingVals$characteristicName <- tadat$characteristicName
  existingVals$characteristicType <- tadat$characteristicType
  existingVals$sampleMedia <- tadat$sampleMedia
  existingVals$project <- tadat$project
  existingVals$organization <- tadat$organization
  existingVals$startDate <- tadat$startDate
  existingVals$endDate <- tadat$endDate
  existingVals$org_table <- tadat$org_table
  existingVals$selected_filters <- tadat$selected_filters
  existingVals$nd_method <- tadat$nd_method
  existingVals$od_method <- tadat$od_method
  existingVals$nd_mult <- tadat$nd_mult
  existingVals$od_mult <- tadat$od_mult
  existingVals$field_sel <- tadat$field_sel
  return(existingVals)
}

updateExisting <- function(tadat, newVals) {
  tadat$original_source <- newVals$original_source
  tadat$job_id <- newVals$job_id
  tadat$example_data <- newVals$example_data
  tadat$statecode <- newVals$statecode
  tadat$countycode <- newVals$countycode
  # tadat$huc <- newVals$huc
  tadat$siteid <- newVals$siteid
  tadat$siteType <- newVals$siteType
  tadat$characteristicName <- newVals$characteristicName
  tadat$characteristicType <- newVals$characteristicType
  tadat$sampleMedia <- newVals$sampleMedia
  tadat$project <- newVals$project
  tadat$organization <- newVals$organization
  tadat$startDate <- newVals$startDate
  tadat$endDate <- newVals$endDate
  tadat$org_table <- newVals$org_table
  tadat$selected_filters <- newVals$selected_filters
  tadat$nd_method <- newVals$nd_method
  tadat$od_method <- newVals$od_method
  tadat$nd_mult <- newVals$nd_mult
  tadat$od_mult <- newVals$od_mult
  tadat$field_sel <- newVals$field_sel
}
