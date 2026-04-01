# tests/testthat/helpers.R (or paste at top of a test file)
#' Generate a minimal sample_raw dataset suitable for mod_depth_server tests
#'
#' Creates a data.frame covering the common columns used in mod_depth_server,
#' with multiple depths per site/date (so depth-profile logic finds groups >= 3).
#'
#' @param n_sites number of distinct site IDs (default 2)
#' @param n_dates number of distinct dates per site (default 2)
#' @param depths numeric vector of depths (metres) to use for each site/date (default c(0.5,1.5,3))
#' @param characteristics character vector of comparable IDs to use (default common ones)
#' @param org organization identifier (default "REDLAKE_WQX")
#' @param start_date first ActivityStartDate (character or Date). Additional dates increment by 1 day.
#' @return data.frame
#' @examples
#' sample_raw <- make_sample_raw()
make_depth_profile_sample_raw <- function(n_sites = 2,
                            n_dates = 2,
                            depths = c(0.5, 1.5, 3.0),
                            characteristics = c("TEMPERATURE, WATER_NA_NA_DEG C",
                                                "DISSOLVED OXYGEN (DO)_NA_NA_MG/L",
                                                "PH_NA_NA_NONE"),
                            org = "REDLAKE_WQX",
                            start_date = "2025-06-01") {
  # ensure character representations
  start_date <- as.Date(start_date)
  site_ids <- paste0("SITE_", seq_len(n_sites))
  date_seq <- as.character(start_date + seq(0, n_dates - 1L))

  rows <- list()
  rid <- 1L
  for (sid in site_ids) {
    for (d in date_seq) {
      # create one record per specified depth for each characteristic to ensure overlap
      for (ch in seq_along(characteristics)) {
        for (depth in depths) {
          rows[[rid]] <- list(
            OrganizationIdentifier = as.character(org),
            TADA.MonitoringLocationIdentifier = as.character(sid),
            ActivityStartDate = as.character(d),
            # TADA.ComparableDataIdentifier used in many places for matching; include a per-characteristic token
            TADA.ComparableDataIdentifier = as.character(gsub("[^A-Z0-9_]", "_", toupper(characteristics[ch]))),
            # Store a user-facing characteristic name too
            TADA.CharacteristicName = as.character(characteristics[ch]),
            # Result value: make numeric but stored as character in many pipelines
            TADA.ResultMeasureValue = as.character(10 + ch + depth),
            # Depth as character
            TADA.ConsolidatedDepth = as.character(depth),
            # generic columns that module may search for (units)
            ResultMeasure.MeasureUnitCode = ifelse(ch == 1, "deg C", ifelse(ch == 2, "mg/L", NA_character_)),
            Unit = ifelse(ch == 1, "deg C", ifelse(ch == 2, "mg/L", NA_character_)),
            # Identifier columns
            ResultIdentifier = paste0("R", formatC(rid, width = 4, flag = "0")),
            stringsAsFactors = FALSE
          )
          rid <- rid + 1L
        }
      }
    }
  }

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  # Ensure column order and types similar to real data (character for many fields)
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
  # Provide some other optional columns downstream code might inspect
  if (!"OrganizationFormalName" %in% names(df)) df$OrganizationFormalName <- df$OrganizationIdentifier
  if (!"MonitoringLocationName" %in% names(df)) df$MonitoringLocationName <- df$TADA.MonitoringLocationIdentifier

  # Make sure we have at least 3 rows per (site, date) so TADA_IDDepthProfiles grouping passes
  # (the nested loop above with depths length >= 3 ensures this by default)
  df
}