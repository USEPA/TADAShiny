
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
  print(1)
  out <-
    TADA::ConvertDepthUnits(out, unit = 'ft', transform = TRUE) # input$depthunit is dummy variable that would connect to the drop down
  print(2)
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
  print(3)
  ##### OTHERS
  # Remove ambiguous censored data records -- might be better suited later on in
  # the app?? Maybe not because could be a QC issue?
  out = TADA::idCensoredData(out)
  print(4)
  return(out)
}
