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
