function (startDate = "null", endDate = "null", aoi_sf = NULL, 
          countrycode = "null", countycode = "null", huc = "null", 
          siteid = "null", siteType = "null", tribal_area_type = "null", 
          tribe_name_parcel = "null", characteristicName = "null", 
          characteristicType = "null", sampleMedia = "null", statecode = "null", 
          organization = "null", project = "null", providers = "null", 
          maxrecs = 350000, ask = TRUE, applyautoclean = TRUE) 
{
  if (length(tribal_area_type) > 1) {
    stop("tribal_area_type must be of length 1.")
  }
  if (!is.null(aoi_sf) & any((tribal_area_type != "null") | 
                             (tribe_name_parcel != "null"))) {
    stop(paste0("Both sf data and tribal information have been provided. ", 
                "Please use only one of these query options."))
  }
  if ((!is.null(aoi_sf) & inherits(aoi_sf, "sf")) & any((countrycode != 
                                                         "null"), (countycode != "null"), (huc != "null"), (siteid != 
                                                                                                            "null"), (statecode != "null"))) {
    warning(paste0("Location information has been provided in addition to an sf object. ", 
                   "Only the sf object will be used in the query."))
  }
  else if ((tribal_area_type != "null") & any((countrycode != 
                                               "null"), (countycode != "null"), (huc != "null"), (siteid != 
                                                                                                  "null"), (statecode != "null"))) {
    warning(paste0("Location information has been provided in addition to tribal information. ", 
                   "Only the tribal information will be used in the query."))
  }
  if ((tribal_area_type != "null") & all(tribe_name_parcel == 
                                         "null")) {
    stop("A tribe_name_parcel is required if tribal_area_type is provided.")
  }
  if ((tribal_area_type == "null") & all(tribe_name_parcel != 
                                         "null")) {
    stop("A tribal_area_type is required if tribe_name_parcel is provided.")
  }
  quiet_whatWQPsites <- purrr::quietly(dataRetrieval::whatWQPsites)
  quiet_whatWQPdata <- purrr::quietly(dataRetrieval::whatWQPdata)
  quiet_readWQPdata <- purrr::quietly(dataRetrieval::readWQPdata)
  if ((!is.null(aoi_sf) & inherits(aoi_sf, "sf")) | (tribal_area_type != 
                                                     "null")) {
    WQPquery <- list()
    if (length(startDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = list(startDate))
    }
    else if (startDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = startDate)
    }
    if (length(siteType) > 1) {
      WQPquery <- c(WQPquery, siteType = list(siteType))
    }
    else if (siteType != "null") {
      WQPquery <- c(WQPquery, siteType = siteType)
    }
    if (length(characteristicName) > 1) {
      WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
    }
    else if (characteristicName != "null") {
      WQPquery <- c(WQPquery, characteristicName = characteristicName)
    }
    if (length(characteristicType) > 1) {
      WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
    }
    else if (characteristicType != "null") {
      WQPquery <- c(WQPquery, characteristicType = characteristicType)
    }
    if (length(sampleMedia) > 1) {
      WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
    }
    else if (sampleMedia != "null") {
      WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
    }
    if (length(project) > 1) {
      WQPquery <- c(WQPquery, project = list(project))
    }
    else if (project != "null") {
      WQPquery <- c(WQPquery, project = project)
    }
    if (length(providers) > 1) {
      WQPquery <- c(WQPquery, providers = list(providers))
    }
    else if (providers != "null") {
      WQPquery <- c(WQPquery, providers = providers)
    }
    if (length(organization) > 1) {
      WQPquery <- c(WQPquery, organization = list(organization))
    }
    else if (organization != "null") {
      WQPquery <- c(WQPquery, organization = organization)
    }
    if (length(endDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = list(endDate))
    }
    else if (endDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = endDate)
    }
    if (tribal_area_type != "null") {
      map_service_urls <- tibble::tribble(~tribal_area, 
                                          ~url, "Alaska Native Allotments", "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/0", 
                                          "American Indian Reservations", "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/2", 
                                          "Off-reservation Trust Lands", "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/3", 
                                          "Oklahoma Tribal Statistical Areas", "https://geopub.epa.gov/arcgis/rest/services/EMEF/Tribal/MapServer/4")
      if (tribal_area_type == "Alaska Native Villages") {
        stop("Alaska Native Villages data are centroid points, not spatial boundaries.")
      }
      else if (tribal_area_type == "Virginia Federally Recognized Tribes") {
        stop("Federally recognized tribal entities in Virginia do not have any available spatial boundaries.")
      }
      if (tribal_area_type %in% c("American Indian Reservations", 
                                  "Off-reservation Trust Lands", "Oklahoma Tribal Statistical Areas")) {
        aoi_sf <- dplyr::filter(map_service_urls, tribal_area == 
                                  tribal_area_type)$url %>% arcgislayers::arc_open() %>% 
          arcgislayers::arc_select() %>% {
            if (all(tribe_name_parcel != "null")) {
              dplyr::filter(., TRIBE_NAME %in% tribe_name_parcel)
            }
            else {
              .
            }
          }
      }
      else if (tribal_area_type == "Alaska Native Allotments") {
        aoi_sf <- dplyr::filter(map_service_urls, tribal_area == 
                                  tribal_area_type)$url %>% arcgislayers::arc_open() %>% 
          arcgislayers::arc_select() %>% {
            if (all(tribe_name_parcel != "null")) {
              dplyr::filter(., PARCEL_NO %in% tribe_name_parcel)
            }
            else {
              .
            }
          }
      }
      else {
        stop("Tribal area type or tribal name parcel not recognized. Refer to TADA_TribalOptions() for query options.")
      }
    }
    aoi_sf <- sf::st_make_valid(aoi_sf)
    if (sf::st_crs(aoi_sf) != 4326) {
      aoi_sf <- sf::st_transform(aoi_sf, crs = 4326)
    }
    input_bbox <- sf::st_bbox(aoi_sf)
    message("Checking for available data. This may take a moment.")
    quiet_bbox_avail <- quiet_whatWQPdata(WQPquery, bBox = c(input_bbox$xmin, 
                                                             input_bbox$ymin, input_bbox$xmax, input_bbox$ymax))
    if (is.null(quiet_bbox_avail$result)) {
      stop_message <- quiet_bbox_avail$messages %>% grep(pattern = "failed|HTTP", 
                                                         x = ., ignore.case = FALSE, value = TRUE) %>% 
        paste("\n", ., collapse = "") %>% paste("The WQP request returned a NULL with the following message(s): \n", 
                                                ., "The bounding box may be too large for this process. Reduce your area of interest and try again.", 
                                                collapse = "\n")
      stop(stop_message)
    }
    bbox_avail <- quiet_bbox_avail$result
    if ((nrow(bbox_avail) > 0) == FALSE) {
      stop("No monitoring sites were returned within your area of interest (no data available).")
    }
    quiet_bbox_sites <- quiet_whatWQPsites(siteid = bbox_avail$MonitoringLocationIdentifier)
    if (is.null(quiet_bbox_sites$result)) {
      stop_message <- quiet_bbox_sites$messages %>% grep(pattern = "failed|HTTP", 
                                                         x = ., ignore.case = FALSE, value = TRUE) %>% 
        paste("\n", ., collapse = "") %>% paste("The WQP request returned a NULL with the following message(s): \n", 
                                                ., collapse = "\n")
      stop(stop_message)
    }
    bbox_sites_sf <- TADA_MakeSpatial(quiet_bbox_sites$result, 
                                      crs = 4326)
    clipped_sites_sf <- bbox_sites_sf[aoi_sf, ]
    clipped_site_ids <- clipped_sites_sf$MonitoringLocationIdentifier
    if ((length(clipped_site_ids) > 0) == FALSE) {
      stop("No monitoring sites were returned within your area of interest (no data available).")
    }
    record_count <- bbox_avail %>% dplyr::filter(MonitoringLocationIdentifier %in% 
                                                   clipped_site_ids) %>% dplyr::pull(resultCount) %>% 
      sum()
    if (ask == TRUE) {
      user_decision <- ask_user(n_records = record_count)
      if (user_decision == "yes") {
        print("Proceeding with download.")
      }
      else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }
    site_count <- length(clipped_site_ids)
    if (site_count > 300 | record_count > maxrecs) {
      message(paste0("The number of sites and/or records matched by the AOI and query terms is large, so the download may take some time. ", 
                     "If your AOI is a county, state, country, or HUC boundary it would be more efficient to provide a code instead of an sf object."))
      results.DR <- withCallingHandlers(TADA_BigDataHelper(record_summary = bbox_avail %>% 
                                                             dplyr::select(MonitoringLocationIdentifier, resultCount) %>% 
                                                             dplyr::filter(MonitoringLocationIdentifier %in% 
                                                                             clipped_site_ids), WQPquery = WQPquery, maxrecs = maxrecs, 
                                                           maxsites = 300), message = function(m) message(m$message))
      rm(bbox_avail, bbox_sites_sf)
      gc()
      if ((nrow(results.DR) > 0) == FALSE) {
        print(paste0("Returning empty results dataframe: ", 
                     "Your WQP query returned no results (no data available). ", 
                     "Try a different query. ", "Removing some of your query filters OR broadening your search area may help."))
        TADAprofile.clean <- results.DR
      }
      else {
        sites.DR <- clipped_sites_sf %>% dplyr::as_tibble() %>% 
          dplyr::select(-geometry)
        quiet_projects.DR <- quiet_readWQPdata(siteid = clipped_site_ids, 
                                               WQPquery, ignore_attributes = TRUE, service = "Project")
        if (is.null(quiet_projects.DR$result)) {
          stop_message <- quiet_projects.DR$messages %>% 
            grep(pattern = "failed|HTTP", x = ., ignore.case = FALSE, 
                 value = TRUE) %>% paste("\n", ., collapse = "") %>% 
            paste("The WQP request returned a NULL with the following message(s): \n", 
                  ., collapse = "\n")
          stop(stop_message)
        }
        projects.DR <- quiet_projects.DR$result
        TADAprofile <- TADA_JoinWQPProfiles(FullPhysChem = results.DR, 
                                            Sites = sites.DR, Projects = projects.DR) %>% 
          dplyr::mutate(dplyr::across(tidyselect::everything(), 
                                      as.character))
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")
          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        }
        else {
          TADAprofile.clean <- TADAprofile
        }
      }
      return(TADAprofile.clean)
    }
    else {
      print("Downloading WQP query results. This may take some time depending upon the query size.")
      print(WQPquery)
      results.DR <- suppressMessages(dataRetrieval::readWQPdata(siteid = clipped_site_ids, 
                                                                WQPquery, dataProfile = "resultPhysChem", ignore_attributes = TRUE))
      if ((nrow(results.DR) > 0) == FALSE) {
        paste0("Returning empty results dataframe: ", 
               "Your WQP query returned no results (no data available). ", 
               "Try a different query. ", "Removing some of your query filters OR broadening your search area may help.")
        TADAprofile.clean <- results.DR
      }
      else {
        sites.DR <- clipped_sites_sf %>% dplyr::as_tibble() %>% 
          dplyr::select(-geometry)
        quiet_projects.DR <- quiet_readWQPdata(siteid = clipped_site_ids, 
                                               WQPquery, ignore_attributes = TRUE, service = "Project")
        if (is.null(quiet_projects.DR$result)) {
          stop_message <- quiet_projects.DR$messages %>% 
            grep(pattern = "failed|HTTP", x = ., ignore.case = FALSE, 
                 value = TRUE) %>% paste("\n", ., collapse = "") %>% 
            paste("The WQP request returned a NULL with the following message(s): \n", 
                  ., collapse = "\n")
          stop(stop_message)
        }
        projects.DR <- quiet_projects.DR$result
        TADAprofile <- TADA_JoinWQPProfiles(FullPhysChem = results.DR, 
                                            Sites = sites.DR, Projects = projects.DR) %>% 
          dplyr::mutate(dplyr::across(tidyselect::everything(), 
                                      as.character))
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")
          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        }
        else {
          TADAprofile.clean <- TADAprofile
        }
      }
      return(TADAprofile.clean)
    }
  }
  else {
    WQPquery <- list()
    if (!"null" %in% statecode) {
      load(system.file("extdata", "statecodes_df.Rdata", 
                       package = "EPATADA"))
      statecode <- as.character(statecode)
      statecodes_sub <- statecodes_df %>% dplyr::filter(STUSAB %in% 
                                                          statecode)
      statecd <- paste0("US:", statecodes_sub$STATE)
      if (nrow(statecodes_sub) == 0) {
        stop("State code is not valid. Check FIPS state/territory abbreviations.")
      }
      if (length(statecode) >= 1) {
        WQPquery <- c(WQPquery, statecode = list(statecd))
      }
    }
    if (length(huc) > 1) {
      WQPquery <- c(WQPquery, huc = list(huc))
    }
    else if (huc != "null") {
      WQPquery <- c(WQPquery, huc = huc)
    }
    if (length(startDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate[1], 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = list(startDate))
    }
    else if (startDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(startDate, 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, startDate = startDate)
    }
    if (length(countrycode) > 1) {
      WQPquery <- c(WQPquery, countrycode = list(countrycode))
    }
    else if (countrycode != "null") {
      WQPquery <- c(WQPquery, countrycode = countrycode)
    }
    if (length(countycode) > 1) {
      WQPquery <- c(WQPquery, countycode = list(countycode))
    }
    else if (countycode != "null") {
      WQPquery <- c(WQPquery, countycode = countycode)
    }
    if (length(siteid) > 1) {
      WQPquery <- c(WQPquery, siteid = list(siteid))
    }
    else if (siteid != "null") {
      WQPquery <- c(WQPquery, siteid = siteid)
    }
    if (length(siteType) > 1) {
      WQPquery <- c(WQPquery, siteType = list(siteType))
    }
    else if (siteType != "null") {
      WQPquery <- c(WQPquery, siteType = siteType)
    }
    if (length(characteristicName) > 1) {
      WQPquery <- c(WQPquery, characteristicName = list(characteristicName))
    }
    else if (characteristicName != "null") {
      WQPquery <- c(WQPquery, characteristicName = characteristicName)
    }
    if (length(characteristicType) > 1) {
      WQPquery <- c(WQPquery, characteristicType = list(characteristicType))
    }
    else if (characteristicType != "null") {
      WQPquery <- c(WQPquery, characteristicType = characteristicType)
    }
    if (length(sampleMedia) > 1) {
      WQPquery <- c(WQPquery, sampleMedia = list(sampleMedia))
    }
    else if (sampleMedia != "null") {
      WQPquery <- c(WQPquery, sampleMedia = sampleMedia)
    }
    if (length(project) > 1) {
      WQPquery <- c(WQPquery, project = list(project))
    }
    else if (project != "null") {
      WQPquery <- c(WQPquery, project = project)
    }
    if (length(providers) > 1) {
      WQPquery <- c(WQPquery, providers = list(providers))
    }
    else if (providers != "null") {
      WQPquery <- c(WQPquery, providers = providers)
    }
    if (length(organization) > 1) {
      WQPquery <- c(WQPquery, organization = list(organization))
    }
    else if (organization != "null") {
      WQPquery <- c(WQPquery, organization = organization)
    }
    if (length(endDate) > 1) {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate[1], 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = list(endDate))
    }
    else if (endDate != "null") {
      if (is.na(suppressWarnings(lubridate::parse_date_time(endDate, 
                                                            orders = "ymd")))) {
        stop("Incorrect date format. Please use the format YYYY-MM-DD.")
      }
      WQPquery <- c(WQPquery, endDate = endDate)
    }
    message("Checking what data is available. This may take a moment.")
    quiet_query_avail <- quiet_whatWQPdata(WQPquery)
    if (is.null(quiet_query_avail$result)) {
      stop_message <- quiet_query_avail$messages %>% grep(pattern = "failed|HTTP", 
                                                          x = ., ignore.case = FALSE, value = TRUE) %>% 
        paste("\n", ., collapse = "") %>% paste("The WQP request returned a NULL with the following message(s): \n", 
                                                ., collapse = "\n")
      stop(stop_message)
    }
    query_avail <- quiet_query_avail$result
    site_count <- length(query_avail$MonitoringLocationIdentifier)
    record_count <- query_avail %>% dplyr::pull(resultCount) %>% 
      sum()
    if (ask == TRUE) {
      user_decision <- ask_user(n_records = record_count)
      if (user_decision == "yes") {
        print("Proceeding with download.")
      }
      else {
        stop("Cancelled by user.", call. = FALSE)
      }
    }
    if (site_count > 300 | record_count > maxrecs) {
      message("The number of sites and/or records matched by the query terms is large, so the download may take some time.")
      results.DR <- suppressMessages(TADA_BigDataHelper(record_summary = query_avail %>% 
                                                          dplyr::select(MonitoringLocationIdentifier, resultCount), 
                                                        WQPquery = WQPquery, maxrecs = maxrecs, maxsites = 300))
      rm(query_avail)
      gc()
      quiet_sites.DR <- quiet_whatWQPsites(siteid = unique(results.DR$MonitoringLocationIdentifier))
      if (is.null(quiet_sites.DR$result)) {
        stop_message <- quiet_sites.DR$messages %>% grep(pattern = "failed|HTTP", 
                                                         x = ., ignore.case = FALSE, value = TRUE) %>% 
          paste("\n", ., collapse = "") %>% paste("The WQP request returned a NULL with the following message(s): \n", 
                                                  ., collapse = "\n")
        stop(stop_message)
      }
      sites.DR <- quiet_sites.DR$result
      quiet_projects.DR <- quiet_readWQPdata(siteid = unique(results.DR$MonitoringLocationIdentifier), 
                                             WQPquery, ignore_attributes = TRUE, service = "Project")
      if (is.null(quiet_projects.DR$result)) {
        stop_message <- quiet_projects.DR$messages %>% 
          grep(pattern = "failed|HTTP", x = ., ignore.case = FALSE, 
               value = TRUE) %>% paste("\n", ., collapse = "") %>% 
          paste("The WQP request returned a NULL with the following message(s): \n", 
                ., collapse = "\n")
        stop(stop_message)
      }
      projects.DR <- quiet_projects.DR$result
      TADAprofile <- TADA_JoinWQPProfiles(FullPhysChem = results.DR, 
                                          Sites = sites.DR, Projects = projects.DR) %>% 
        dplyr::mutate(dplyr::across(tidyselect::everything(), 
                                    as.character))
      if (applyautoclean == TRUE) {
        print("Data successfully downloaded. Running TADA_AutoClean function.")
        TADAprofile.clean <- TADA_AutoClean(TADAprofile)
      }
      else {
        TADAprofile.clean <- TADAprofile
      }
      return(TADAprofile.clean)
    }
    else {
      print("Downloading WQP query results. This may take some time depending upon the query size.")
      print(WQPquery)
      results.DR <- suppressMessages(dataRetrieval::readWQPdata(WQPquery, 
                                                                dataProfile = "resultPhysChem", ignore_attributes = TRUE))
      if ((nrow(results.DR) > 0) == FALSE) {
        print("Returning empty results dataframe: Your WQP query returned no results (no data available). Try a different query. Removing some of your query filters OR broadening your search area may help.")
        TADAprofile.clean <- results.DR
      }
      else {
        sites.DR <- suppressMessages(dataRetrieval::whatWQPsites(WQPquery))
        projects.DR <- suppressMessages(dataRetrieval::readWQPdata(WQPquery, 
                                                                   ignore_attributes = TRUE, service = "Project"))
        TADAprofile <- TADA_JoinWQPProfiles(FullPhysChem = results.DR, 
                                            Sites = sites.DR, Projects = projects.DR) %>% 
          dplyr::mutate(dplyr::across(tidyselect::everything(), 
                                      as.character))
        if (applyautoclean == TRUE) {
          print("Data successfully downloaded. Running TADA_AutoClean function.")
          TADAprofile.clean <- TADA_AutoClean(TADAprofile)
        }
        else {
          TADAprofile.clean <- TADAprofile
        }
      }
      return(TADAprofile.clean)
    }
  }
}