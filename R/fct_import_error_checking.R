
#' import_error_checking
#' 
#' Does quality control on the excel file uploaded by user
#'
#' @param uploaded_data data uploaded by user
#'

import_error_checking = function(uploaded_data){
  
  # Check if: 
  
  # The right column names were used 
  col_names <- c("ExposureUnit",	"Contaminant",	"CASRN",	"Media",	"Level",	"Unit",	"DetectedFlag", "SampleID")
  
  names_data <- names(uploaded_data)
  
  ok_col_names <- length(setdiff(col_names, names_data)) == 0
  
  # Set variables for checks that need to loop through unique identifiers
  # add column with unique ids
  data_identifier <- uploaded_data %>%
    dplyr::mutate(identifier = paste(ExposureUnit, Contaminant, Media, sep = ", ")) 
  
  # list of unique combinations
  unique_ids <- unique(data_identifier$identifier)  
  
  # The media in the media column only includes one of the set ones 
  accepted_media <- c("Water", "Soil/Sediment", "Air", "Food", "SVI-Soil Gas", "SVI-Groundwater")
  
  check_media_type <- data_identifier %>% 
    dplyr::mutate(media_ok = ifelse(Media %in% accepted_media, TRUE, FALSE))
  
  media_ok <- all(check_media_type$media_ok)
  
  # All unique contaminant names have only one unique CASRN and vice-versa
  uniq_casrn <- unique(uploaded_data$CASRN)
  uniq_contam <- unique(uploaded_data$Contaminant)
  
  # function to check each casrn has only one contaminant
  check_casrn = function(uniq_casrn){
    
    casrn_id <- uploaded_data %>% 
      dplyr::filter(CASRN == uniq_casrn)
    
    one_contam <- length(unique(casrn_id$Contaminant)) == 1
    
    return(one_contam)
  }
  
  check_casrn_list <- lapply(uniq_casrn, check_casrn)
  one_contam_per_casrn <- all(as.logical(check_casrn_list))
  
  # function to check each contaminant has only one casrn
  check_contam = function(uniq_contam){
    
    contam_id <- uploaded_data %>% 
      dplyr::filter(Contaminant == uniq_contam)
    
    one_casrn <- length(unique(contam_id$CASRN)) == 1
    
    return(one_casrn)
  }
  
  check_contam_list <- lapply(uniq_contam, check_contam)
  one_casrn_per_contam <- all(as.logical(check_contam_list))
  
  unique_contam_casrn <- all(one_casrn_per_contam, one_contam_per_casrn)
  
  # Units are the same for all records with the same combination of contaminant, media and exposure unit
  
  # function that checks whether there is only 1 unit per combination
  check_units = function(uniq_id){
    
    data_id <- data_identifier %>% 
      dplyr::filter(identifier == uniq_id)
    
    same_units <- length(unique(data_id$Unit)) == 1
    
    return(same_units)
  }
  
  # loop through the function for each unique combination
  same_units <- lapply(unique_ids, check_units)
  
  # check if any of the combinations had more than one unit 
  same_units <- isTRUE(unique(same_units)[[1]])
  
  # The NotDetectedFlag field includes only zeros or ones
  flags_binary <- all(uploaded_data$NotDetectedFlag %in% c(0, 1))
  
  # All fields are populated for each record (i.e. no blanks)
  no_blanks <- all(!is.na(uploaded_data))
  
  # CAS Numbers are entered in the format ######-##-# 
  cas_correct <- uploaded_data %>% 
    dplyr::mutate(correct_cas_num = stringr::str_detect(as.character(CASRN), "^0*(\\d{1,6})-?(\\d{2})-?(\\d)$"))
  
  cas_correct <- all(cas_correct$correct_cas_num)
  
  # The Level field includes only numeric data
  level_numeric <- is.numeric(uploaded_data$Level)
  
  # check if anything failed the quality check           
  all_ok <- all(unique_contam_casrn, no_blanks, same_units, flags_binary, level_numeric, cas_correct, media_ok, ok_col_names)   
  
  if (isTRUE(all_ok)) {
    
    return("")
    
  } else {
    
    if (isFALSE(ok_col_names)){
      
      ok_col_names <- "\U2022 Column names do not match import template. Please use the import template to upload your data.<br/>"
      
    } else {ok_col_names<-""}
    
    if (isFALSE(unique_contam_casrn)) {
      
      unique_contam_casrn <- "\U2022 Unique contaminant names have more than one unique CASRN or vice-versa.<br/>"
      
    } else {unique_contam_casrn<-""}
    
    if (isFALSE(no_blanks)){
      
      no_blanks <- "\U2022 One or more fields are blank.<br/>"
      
    } else {no_blanks<-""}
    
    if (isFALSE(same_units)){
      
      same_units <- "\U2022 Units are NOT the same for all records with the same combination of contaminant, media and exposure unit.<br/>"
      
    } else {same_units<-""}
    
    if (isFALSE(flags_binary)){
      
      flags_binary <- "\U2022 The NotDetectedFlag field DOES NOT include only zeros or ones.<br/>"
      
    } else {flags_binary<-""}
    
    if (isFALSE(level_numeric)){
      
      level_numeric <- "\U2022 The Level field DOES NOT include only numeric data.<br/>"
      
    } else {level_numeric<-""}
    
    if (isFALSE(cas_correct)){
      
      cas_correct <- "\U2022 CASRN numbers not in a correct format.<br/>"
      
    } else {cas_correct<-""}
    
    if (isFALSE(media_ok)){
      
      media_ok <- "\U2022 Not all media in data are of the accepted type.<br/>"
      
    } else {media_ok<-""}
    
    
    return(paste0(ok_col_names, unique_contam_casrn, no_blanks, same_units, flags_binary, level_numeric, cas_correct, media_ok))
  }
  
}