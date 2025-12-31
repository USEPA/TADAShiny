#' @keywords internal
"_PACKAGE"

#' Internal imports
#'
#' Declare imports so R CMD check recognizes usage of packages listed in
#' DESCRIPTION Imports. Keep only functions you actually use.
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom golem activate_js add_resource_path bundle_resources favicon with_golem_options
#' @importFrom shiny column HTML NS shinyApp tagAppendAttributes tagList tags
#' @importFrom data.table data.table as.data.table setDT fread rbindlist
#' @importFrom jsonlite fromJSON
#' @importFrom pkgload load_all
#' @import rExpertQuery
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# Silence NSE binding notes for common data.table symbols
utils::globalVariables(c(
  "TADA.CensoredData.Flag", "ResultIdentifier", "TADA.ResultMeasureValue",
  "TADA.CensoredMethod", "TADA.Remove", "MonitoringLocationIdentifier",
  "MonitoringLocationName", "TADA.LatitudeMeasure", "TADA.LongitudeMeasure",
  "ActivityStartDate", "TADA.CharacteristicName", "OrganizationIdentifier",
  "OrganizationFormalName",
  "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "TADA.ResultMeasure.MeasureUnitCode", "MonitoringLocationTypeName",
  "Target.TADA.CharacteristicName", "Target.TADA.MethodSpeciationName",
  "Target.TADA.ResultSampleFractionText",
  "DetectionQuantitationLimitMeasure.MeasureValue",
  "Count", "Description", "Field", "Legend", "Rank", "Result_Count", "Step", "TADA.Chars",
  "TRIBE_NAME", "characteristicName", "characteristicType", "countycode", "endDate",
  "example_data", "field_sel", "flag_simple", "group", "groupname", "m2f",
  "nd_method", "nd_mult", "num", "num_chr", "od_method", "od_mult", "org_table",
  "organization", "original_source", "project", "resultCount", "sampleMedia",
  "selected_filters", "selected_flags", "siteType", "siteid", "startDate", "statecode",
  "tot_n", "TADA.RemovalReason", "Fields", "Value", "old_warn"
))
