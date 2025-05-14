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
                        bBox = NULL){
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

# A function to calculate the cumsum grouping
cumsum_group <- function(dat, col, threshold = 25000){
  num <- nrow(dat)
  x <- dat[[col]]
  y <- numeric(num)
  z <- integer(num)
  group_num <- 1L
  current_sum <- 0
  for (i in 1:num){
    if (x[i] >= threshold){
      y[i] <- x[i]
      group_num <- group_num + 1L
      z[i] <- group_num
      current_sum <- 0
      group_num <- group_num + 1L
    } else if (current_sum + x[i] >= threshold){
      current_sum <- x[i]
      group_num <- group_num + 1L
      y[i] <- current_sum
      z[i] <- group_num
    } else {
      current_sum <- current_sum + x[i]
      y[i] <- current_sum
      z[i] <- group_num
    }
  }
  dat2 <- dat %>% dplyr::mutate(Cumsum = y, CumGroup = z)
  return(dat2)
}

# A function to get the summarized year for each group
summarized_year <- function(dat){
  dat2 <- dat %>%
    dplyr::group_by(HUCEightDigitCode, CumGroup) %>%
    dplyr::slice(c(1, dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Type = rep(c("Start", "End"), times = dplyr::n()/2)) %>%
    dplyr::select(HUCEightDigitCode, YearSummarized, CumGroup, Type) %>%
    tidyr::pivot_wider(names_from = "Type", values_from = "YearSummarized") %>%
    dplyr::mutate(Start = paste0(Start, "-01", "-01"),
            End = paste0(End, "-12", "-31"))
  return(dat2)
}