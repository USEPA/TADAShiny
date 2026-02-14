# we need to figure out how to split the WQP into 2 parts,
# one for WQP and one for 'USGS'  this is a (temporary) requirement since
# there is a Beta version of WQP, but it does not yet have all the USGS data
# cristina provided these examples
.tadas_offline <- function() {
  nzchar(Sys.getenv("TADAS_OFFLINE", "")) # set TADAS_OFFLINE=true in CI to force offline
}
.safe_req_string <- function(u, timeout = 30, max_tries = 3) {
  if (.tadas_offline()) {
    return(NULL)
  }
  tryCatch(
    {
      httr2::request(u) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = max_tries) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_string()
    },
    error = function(e) NULL
  )
}

# County: census file has no header; on failure return empty data.frame with expected columns
.safe_fetch_county <- function(u) {
  txt <- .safe_req_string(u)
  cols <- c("STUSAB", "STATE", "COUNTY", "COUNTY_NAME", "COUNTY_ID")
  # should be 
  cols <- c("STATE_CD", "STATE_FIPS", "COUNTY_FIPS", "COUNTY_NAME", "COUNTY_FOOBAR")
  # dataRetrieval::read_waterdata_samples needs "US:{STATE_FIPS}"
  # and "US:{STATE_FIPS}:{COUNTY_FIPS}"
  # and EPATADA::TADA_DataRetrieval needs "STATE_CD" and COUNTY_NAME
  if (is.null(txt)) {
    return(data.frame(
      STATE_CD = character(), STATE_FIPS = character(), COUNTY_FIPS = character(),
      COUNTY_NAME = character(), COUNTY_FOOBAR = character(), stringsAsFactors = FALSE
    ))
  }
  dt <- tryCatch(
    data.table::fread(txt, header = FALSE, col.names = cols, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(dt)) {
    return(data.frame(
      STUSAB = character(), STATE = character(), COUNTY = character(),
      COUNTY_NAME = character(), COUNTY_ID = character(), stringsAsFactors = FALSE
    ))
  }
  as.data.frame(dt)
}


counties <- .safe_fetch_county("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt")

start_date <- "2025-01-01"
end_date <- "2025-12-01"
characteristic_name <- "pH"
state_abbrev = 'CO'
county_name = 'Chaffee County'
browser()
county = counties[counties$STATE_CD == state_abbrev & counties$COUNTY_NAME == county_name,]
state_fips_arg = paste('US', sprintf("%02d", county$STATE_FIPS), sep = ':')
county_fips_arg = paste('US', sprintf("%02d", county$STATE_FIPS), sprintf("%03d", county$COUNTY_FIPS), sep = ':')


# this does 2 queries of WQP
#> GET: https://www.waterqualitydata.us/data/Result/ 
#> GET: https://www.waterqualitydata.us/data/Station/
WQP3_results <- 
  dataRetrieval::readWQPdata(statecode = county$STATE_CD,
                             countycode = county$COUNTY_NAME,
                             characteristicName = characteristic_name,
                             startDate = start_date,
                             endDate = end_date,
                             service = "ResultWQX3",
                             dataProfile = "fullPhysChem",
                             ignore_attributes = TRUE,
                             providers = "STORET")
# remove 2 columns that are not found in the NWIS_results (yet)
# WQP3_results <- WQP3_results[, !(names(WQP3_results) %in% c("Activity_EndTimeZone_offset", "Activity_EndDateTime"))]

# this does not use WQP
# GET: https://api.waterdata.usgs.gov/samples-data/results/
NWIS_results <- 
  dataRetrieval::read_waterdata_samples(stateFips = state_fips_arg,
                                        countyFips = county_fips_arg,
                                        characteristic = characteristic_name,
                                        activityStartDateLower = start_date,
                                        activityStartDateUpper = end_date,
                                        dataProfile = "fullphyschem")
browser()

# this field is all NA but still needs to be recast as date
NWIS_results$Activity_EndDate <- as.Date(NWIS_results$Activity_EndDate)

All_results <- dplyr::bind_rows(WQP3_results, NWIS_results)

All_results_rename <- EPATADA::TADA_RenametoLegacy(All_results)

All_results_clean <- EPATADA::TADA_AutoClean(All_results_rename)

All_results_ordered <- EPATADA::TADA_OrderCols(All_results_clean)