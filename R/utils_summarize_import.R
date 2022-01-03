#' summarize_import
#'
#' @param data_upload the excel file uploaded by the user
#'
#' @return a dataframe with summary information on the excel file uploaded by the user

summarize_import <- function(data_upload){
  
  # read in the xlsx uploaded by the user
  raw_data <- data_upload %>% 
    dplyr::mutate(identifier = paste(ExposureUnit, CASRN, Media, sep = ", "))
  
  # number of observations
  observations <- length(raw_data$Level)
  
  # number of exposure units
  exp_units <- length(unique(raw_data$ExposureUnit))
  
  # number of contaminants
  contaminants <- length(unique(raw_data$CASRN))
  
  # number of media
  media <- length(unique(raw_data$Media))
  
  # number of epcs to be calculated
  epcs <- length(unique(raw_data$identifier))
  
  # bind together in final dataframe
  import_summary_df <- data.frame("Number of Observations" = observations,
                                  "Number of Exposure Units" = exp_units,
                                  "Number of Contaminants (by CASRN)" = contaminants,
                                  "Number of Media" = media,
                                  "Number of EPCs to be Calculated" = epcs,
                                  check.names = FALSE) 
  
  
  return(import_summary_df)
}