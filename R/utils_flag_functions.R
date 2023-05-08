# Read in the tables
prompt_table = read.csv("inst/flag_prompts.csv")
test_table = read.csv("inst/flag_tests.csv")
#prompt_table = read.csv(app_sys("flag_prompts.csv"))
#test_table = read.csv(app_sys("flag_tests.csv"))
prompt_table <- prompt_table[order(prompt_table$Order),]
prompts <- prompt_table$Prompt
n_switches <- length(prompts)
flag_types <- prompt_table$flagType

switch_defaults <- prompt_table$Level != "Optional"
switch_disabled <- prompt_table$Level == "Required"


flagCensus <- function(raw) {
  tabular_results = data.frame(matrix(ncol = length(flag_types), nrow = nrow(raw)))
  colnames(tabular_results) <- flag_types
  test_table = subset(test_table, test_table$remove==1)
  for (flag in flag_types) {
    flag_count = 0
    tests = test_table[test_table$flagType == flag, ]
    results = integer(nrow(raw))
    if (nrow(tests) > 0) {
      for (row in 1:nrow(tests)) {
        test_col = tests[row, 'columnName']
        test_val = tests[row, 'flagValue']
        keep = tests[row, 'keep']
        if (test_col != 'Unknown') {
          test_results = as.integer(as.logical(raw[test_col] == test_val))
          if (tests[row, 'keep']){
            test_results = !test_results
          } else{
            
          }
          results = results + test_results
        }
        tabular_results[flag] <- (results > 0)
      }
    } else {
      print(paste0("No tests found for flag ", flag))
    }
  }
  
  return(tabular_results)
}

getCounts <- function(sites, removed_records){
  
  summary_names = c(
    "Total in Raw File",
    "Total Removed",
    "Total in Clean File")
  
  # Records
  n_raw_records = length(removed_records)
  n_removed_records = sum(removed_records)
  n_clean_records = n_raw_records - n_removed_records

  # Sites
  n_raw_sites = length(unique(sites))
  n_removed_sites = length(unique(sites[removed_records]))
  n_clean_sites = n_raw_sites - n_removed_sites
  
  summaryTable = data.frame(
    row.names = summary_names,
    Records = c(n_raw_records, n_removed_records, n_clean_records),
    Sites = c(n_raw_sites, n_removed_sites, n_clean_sites)
  )  
  return(summaryTable)
}
# Settings for each flag function in flag page mock up
applyFlags <- function(in_table) {
  
  # Invalid Speciation
  out <- TADA::InvalidSpeciation(in_table, clean = "none")
  
  # Invalid fraction
  out <- TADA::InvalidFraction(out, clean = FALSE)
  
  # Invalid result unit
  out <- TADA::InvalidResultUnit(out, clean = "none")
  
  # Special characters - data are already run through autoclean when downloaded, this switch doesn't require any additional functions to flag data for special characters
  out = out
  
  # Invalid analytical method
  out <- TADA::InvalidMethod(out, clean = FALSE)
  
  # QAPP Not Approved - this flag isn't looking for a TADA-created flag column,
  # so do not need to run any flag function here. If switched ON, remove all data
  # with QAPPApproved == N or NA.
  out = out
  
  # No QAPP doc available
  out <- TADA::QAPPDocAvailable(out, clean = FALSE)
  
    # Dataset includes depth profile data - no function for this? How is this one
    # supposed to work?
  out = out
  
  # Aggregated continuous data
  out <- TADA::AggregatedContinuousData(out, clean = FALSE)
  
  # True duplicates - not needed, true duplicates automatically removed in
  # autoclean. "ALMOST" duplicates function still in dev.
  #out  = out
  
  # Activity media name not water - Water media filter is not dependent upon flag
  # function, so do not need to run any flags on this one
  #out = out
  
  # Above WQX Upper Threshold
  out <- TADA::AboveNationalWQXUpperThreshold(out, clean = FALSE)
  
  # Below WQX Lower Threshold
  out <- TADA::BelowNationalWQXLowerThreshold(out, clean = FALSE)
  
  # Convert depth height units - THIS ONE ONLY GETS RUN WHEN USER RUNS THE CLEANING
  # FILTER AFTER MAKING ALL DECISIONS, AND SUMMARY COUNTS BASED ON UNIQUE UNITS IN
  # DEPTH HEIGHT COLUMNS
  out <-
    TADA::ConvertDepthUnits(out, unit = 'ft', transform = TRUE) # input$depthunit is dummy variable that would connect to the drop down

  # Convert time zones - no flag function to run beforehand. This one might be
  # tricky to implement - acts on ActivityStartTime.Time?
  #out = out
  
  # Invalid coordinates - not included in mock up page?
  out <-
    TADA::InvalidCoordinates(
      out,
      clean_outsideUSA = "no",
      clean_imprecise = FALSE,
      errorsonly = FALSE
    )

  ##### OTHERS
  # Remove ambiguous censored data records -- might be better suited later on in
  # the app?? Maybe not because could be a QC issue?
  # out = TADA::idCensoredData(out)

  return(out)
}
