#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

### A function to construct the argument list
args_create <- function(statecode = NULL,
                        countycode = NULL,
                        countrycode = NULL,
                        huc = NULL,
                        siteid = NULL,
                        siteType = NULL,
                        characteristicName = NULL,
                        characteristicType = NULL,
                        sampleMedia = NULL,
                        project = NULL,
                        organization = NULL,
                        startDateLo = NULL,
                        startDateHi = NULL,
                        providers = NULL,
                        bBox = NULL) {
  # Construct the arguments for downloads
  args <- list(
    "statecode" = statecode,
    "countycode" = countycode,
    "countrycode" = countrycode,
    "huc" = huc,
    "siteid" = siteid,
    "siteType" = siteType,
    "characteristicName" = characteristicName,
    "characteristicType" = characteristicType,
    "sampleMedia" = sampleMedia,
    "project" = project,
    "organization" = organization,
    "startDateLo" = startDateLo,
    "startDateHi" = startDateHi,
    "providers" = providers,
    "bBox" = bBox
  )

  # Replace null with NULL
  args[args %in% "null"] <- list(NULL)

  # Remove NULL attribute
  args <- args[purrr::map_lgl(args, function(x) !is.null(x))]

  return(args)
}

### A function to construct the argument list for the NWIS function dataRetrieval::read_waterdata_samples(args_temp)
nwis_args_create <- function(stateFips = NULL,
                             countyFips = NULL,
                             # countrycode = NULL,
                             hydrologicUnit = NULL,
                             monitoringLocationIdentifier = NULL,
                             siteTypeName = NULL,
                             characteristic = NULL,
                             characteristicGroup = NULL,
                             activityMediaName = NULL,
                             projectIdentifier = NULL,
                             organizationIdentifier = NULL,
                             activityStartDateLower = NULL,
                             activityStartDateUpper = NULL,
                             dataType = NULL,
                             dataProfile = NULL,
                             # providers = NULL,
                             boundingBox = NULL
) {
  # Construct the arguments for downloads
  args <- list(
    "stateFips" = stateFips,
    "countyFips" = countyFips,
    # "countrycode" = countrycode,
    "hydrologicUnit" = hydrologicUnit,
    "monitoringLocationIdentifier" = monitoringLocationIdentifier,
    "siteTypeName" = siteTypeName,
    "characteristic" = characteristic,
    "characteristicGroup" = characteristicGroup,
    "activityMediaName" = activityMediaName,
    "projectIdentifier" = projectIdentifier,
    # "organizationIdentifier" = organizationIdentifier,
    "activityStartDateLower" = activityStartDateLower,
    "activityStartDateUpper" = activityStartDateUpper,
    "dataType" = dataType,
    "dataProfile" = dataProfile,
    # "providers" = providers,
    "boundingBox" = boundingBox
  )

  # Replace null with NULL
  args[args %in% "null"] <- list(NULL)

  # Remove NULL attribute
  args <- args[purrr::map_lgl(args, function(x) !is.null(x))]

  return(args)
}
