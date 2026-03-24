# legacy/production WQP, using only to verify counts, EPA WQX counts and data are the 
# same for legacy/production AND new WQX3 beta service
# only the USGS counts are different for legacy/production AND new WQP WQX3 beta service after March 2024

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

start_date <- "2025-05-01"
end_date <- "2025-12-01"
characteristic_name <- "Phosphorus"
state_abbrev = 'WI'
county_name = 'Dane County'

county = county[county$STATE_CD == state_abbrev & county$COUNTY_NAME == county_name,]
state_fips_arg = paste('US', c$STATE_FIPS, sep = ':')
county_fips_arg = paste('US', c$STATE_FIPS, sprintf("%03d", c$COUNTY_FIPS), sep = ':')

tada1 <- EPATADA::TADA_DataRetrieval(
                                      statecode = county$STATE_CD, 
                                      countycode = county$COUNTY_NAME,
                                      characteristicName = characteristic_name,
                                      startDate = start_date,
                                      endDate = end_date,
                                      provider = "STORET", # only EPA WQX data = "STORET"
                                      ask = FALSE, 
                                      applyautoclean = FALSE
                                    )
browser()
# new stand alone USGS service for their samples data
# this data also gets delivered to the new WQP beta
# use this to get the USGS data only
tada2 <- dataRetrieval::read_waterdata_samples(
                                                stateFips = state_fips_arg,
                                                countyFips = county_fips_arg,
                                                characteristic = characteristic_name,
                                                activityStartDateLower = start_date,
                                                activityStartDateUpper = end_date, 
                                                dataProfile = "fullphyschem"
                                              )

# new WQP WQX3 beta service, represents both EPA and USGS
# use to get the total expected counts for both USGS and EPA, regardless of dates
tada3 <- dataRetrieval::readWQPdata(statecode = county$STATE_CD,
                                    countycode = county$COUNTY_NAME, 
                                    characteristicName = characteristic_name,   
                                    startDate = start_date,
                                    endDate = end_date,
                                    service = "ResultWQX3",
                                    dataProfile = "fullPhysChem",
                                    ignore_attributes = TRUE
                                    )

# new WQP WQX3 beta service, WQX only
tada4 <- dataRetrieval::readWQPdata(statecode = county$STATE_CD,
                                    countycode = county$COUNTY_NAME,
                                    characteristicName = characteristic_name,
                                    startDate = start_date,
                                    endDate = end_date,
                                    service = "ResultWQX3",
                                    dataProfile = "fullPhysChem",
                                    ignore_attributes = TRUE,
                                    providers = "STORET"
)

# fail if the counts don't match
stopifnot(nrow(tada1) + nrow(tada2) == nrow(tada3))

# fail if the counts don't match, there is a bug so it will fail
stopifnot(nrow(tada1) == nrow(tada4))

# final to use in TADAShiny
# Add tada2 with tada4 (full join)
browser()
# 1) Ensure both have the exact same columns
stopifnot(setequal(names(tada4), names(tada2)))

# Align column order so names are identical and in the same sequence
tada2 <- tada2[, names(tada4)]

# Optional: sanity check types (will only warn; bind_rows can coerce)
classes_wqx <- sapply(tada4, function(x) paste(class(x), collapse = "+"))
classes_usgs <- sapply(tada2,  function(x) paste(class(x), collapse = "+"))
type_mismatch <- names(tada4)[classes_wqx != classes_usgs]
if (length(type_mismatch) > 0) {
  message("Warning: Type mismatches detected in columns: ",
          paste(type_mismatch, collapse = ", "))
  # If needed, harmonize types here before bind_rows (e.g., as.character, as.numeric, etc.)
}

# 2) Bind rows (stack records); no new columns should be introduced
tada_all <- dplyr::bind_rows(tada4, tada2)

# 3) Quick checks
stopifnot(ncol(tada_all) == ncol(tada4))
stopifnot(identical(names(tada_all), names(tada4)))
stopifnot(nrow(tada_all) == nrow(tada4) + nrow(tada2))