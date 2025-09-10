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
  "tot_n"
  ))

# Auto generates or updates TADAShiny-package.R file
# how to import specific functions from packages for use throughout app
# usethis::use_import_from(package = "shiny", fun = c("NS", "tagList"))
