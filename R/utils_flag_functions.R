# Read in the tables
prompt_table <- utils::read.csv("inst/flag_prompts.csv")
test_table <- utils::read.csv("inst/flag_tests.csv")
# prompt_table = utils::read.csv(app_sys("flag_prompts.csv"))
# test_table = utils::read.csv(app_sys("flag_tests.csv"))
prompt_table <- prompt_table[order(prompt_table$Order),]
prompts <- prompt_table$Prompt
active_flags = unique(merge(prompt_table, test_table)$columnName)
levs <- prompt_table$Level
n_switches <- length(prompts)
flag_types <- prompt_table$flagType
flag_prefix = "Flag: "

flagCensus <- function(raw) {
  # JCH - seems like there are NA values here that aren't getting counted right
  tabular_results <-
    data.frame(matrix(ncol = length(flag_types), nrow = nrow(raw)))
  colnames(tabular_results) <- flag_types
  
  test_table <- subset(test_table, test_table$remove == 1)
  for (flag in flag_types) {
    flag_count <- 0
    tests <- test_table[test_table$flagType == flag,]
    results <- integer(nrow(raw))
    if (nrow(tests) > 0) {
      for (row in 1:nrow(tests)) {
        test_col <- tests[row, "columnName"]
        test_val <- tests[row, "flagValue"]
        keep <- tests[row, "keep"]
        if (test_col != "Unknown" & test_col %in% names(raw)) {
          if (!is.na(test_val)) {
            rawt_col <- raw[, test_col]
            rawt_col[is.na(rawt_col)] <- "NA"
            test_results <-
              as.integer(as.logical(rawt_col == test_val))
          } else {
            test_results <- as.integer(as.logical(is.na(raw[test_col])))
          }
          if (tests[row, "keep"]) {
            test_results <- !test_results
          } else {
            
          }
          results <- results + test_results
        }
        tabular_results[flag] <- (results > 0)
      }
    } else {
      print(paste0("No tests found for flag ", flag))
    }
  }
  
  return(tabular_results)
}

getCounts <- function(sites, removed_records) {
  summary_names <- c("Total in Raw File",
                     "Total Removed",
                     "Total in Clean File")
  
  # Records
  n_raw_records <- length(removed_records)
  n_removed_records <- sum(removed_records)
  n_clean_records <- n_raw_records - n_removed_records
  
  # Sites
  n_raw_sites <- length(unique(sites))
  n_removed_sites <- length(unique(sites[removed_records]))
  n_clean_sites <- n_raw_sites - n_removed_sites
  
  summaryTable <- data.frame(
    row.names = summary_names,
    Records = c(n_raw_records, n_removed_records, n_clean_records),
    Sites = c(n_raw_sites, n_removed_sites, n_clean_sites)
  )
  return(summaryTable)
}
# Settings for each flag function in flag page mock up
applyFlags <- function(in_table, orgs) {
  out <- TADA::TADA_IDCensoredData(in_table)
  # Invalid Speciation
  out <- TADA::TADA_FlagSpeciation(out, clean = "none")

  # Invalid fraction
  out <- TADA::TADA_FlagFraction(out, clean = FALSE)

  # Invalid result unit
  out <- TADA::TADA_FlagResultUnit(out, clean = "none")
  print(paste0("Step 5: ", nrow(out)))
  # QC rep/blank
  out <- TADA::TADA_FindQCActivities(out, clean = FALSE)

  # Result is flagged as suspect by data submitter
  out <- TADA::TADA_FlagMeasureQualifierCode(out, clean = FALSE, define = TRUE)

  # Invalid analytical method
  out <- TADA::TADA_FlagMethod(out, clean = FALSE)
  
  # Single org duplicative uploads
  out <- TADA::TADA_FindPotentialDuplicatesSingleOrg(out)

  # multiple org duplicative uploads
  ## NOTE: THIS FUNCTION USES A REACTIVE OBJECT AS AN INPUT
  out <-
    TADA::TADA_FindPotentialDuplicatesMultipleOrgs(out, org_hierarchy = orgs)

  # QAPP Not Approved - this flag isn't looking for a TADA-created flag column,
  # so do not need to run any flag function here. If switched ON, remove all data
  # with QAPPApproved == N or NA.
  
  # No QAPP doc available
  if ("ProjectFileUrl" %in% names(out)) {
    out <- TADA::TADA_FindQAPPDoc(out, clean = FALSE)
  }

  # Dataset includes depth profile data - no function for this yet
  # out <- out

  # Aggregated continuous data
  out <- TADA::TADA_FindContinuousData(out, clean = FALSE)

  # Above WQX Upper Threshold
  out <- TADA::TADA_FlagAboveThreshold(out, clean = FALSE)

  # Below WQX Lower Threshold
  out <- TADA::TADA_FlagBelowThreshold(out, clean = FALSE)

  # Invalid coordinates
  out <-
    TADA::TADA_FlagCoordinates(
      out,
      clean_outsideUSA = "no",
      clean_imprecise = FALSE,
      flaggedonly = FALSE
    )

  return(out)
}

checkFlagColumns <- function(dataset) {
  missing <- setdiff(active_flags, names(dataset))
  found = setdiff(active_flags, missing)
  if (length(missing) > length(found)) {
    return(FALSE)
  } else {
    if (length(missing) > 0){
      print("Missing the following fields that are in the csv files:")
      print(missing)
    }
    return(TRUE)
  }
}