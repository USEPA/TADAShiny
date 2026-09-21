############

# WQX only from WQP via DR (legacy/production)
test <- dataRetrieval::readWQPdata(
  sampleMedia = c("Water", "water"),
  startDateLo = "01-01-1991",
  startDateHi = "12-31-2005",
  service = "Result",
  dataProfile = "resultPhysChem",
  ignore_attributes = TRUE,
  bBox = c(-110.495, 45.192, -109.693, 45.911),
  providers = "STORET"
)

# WQX only from WQP via TADA (legacy/production)
test2 <- EPATADA::TADA_DataRetrieval(
  sampleMedia = c("Water", "water"),
  startDate = "1991-01-01",
  endDate = "2005-12-31",
  bBox = c(-110.495, 45.192, -109.693, 45.911),
  provider = "STORET",
  ask = FALSE
)

# USGS only from DR (WQX 3.0/beta)
test3 = dataRetrieval::read_waterdata_samples(
  activityMediaName = c("Water"),
  activityStartDateLower = "01-01-1991",
  activityStartDateUpper = "12-31-2005",
  boundingBox = c(-110.495, 45.192, -109.693, 45.911),
  dataType = "results",
  dataProfile = "fullphyschem"
)

# both USGS and WQX from DR (WQX 3.0/beta)
test4 <- dataRetrieval::readWQPdata(
  sampleMedia = c("Water", "water"),
  startDateLo = "01-01-1991",
  startDateHi = "12-31-2005",
  service = "ResultWQX3",
  dataProfile = "fullPhysChem",
  ignore_attributes = TRUE,
  bBox = c(-110.495, 45.192, -109.693, 45.911)
)

############

# WQX only from WQP via DR 
test <- dataRetrieval::readWQPdata(
  sitetype = c("Stream", "Lake, Reservoir, Impoundment"),
  countryocean_choices = "United States of America",
  sampleMedia = c("Water"),
  characteristicgroup = "Organics, Pesticides",
  startDateLo = "01-01-2020",
  service = "Result",
  dataProfile = "resultPhysChem",
  ignore_attributes = TRUE,
  providers = "STORET"
)

# # WQX only from WQP via TADA
# test2 <- EPATADA::TADA_DataRetrieval(
#   sampleMedia = c("Water", "water"),
#   startDate = "1991-01-01",
#   endDate = "2005-12-31",
#   bBox = c(-110.495, 45.192, -109.693, 45.911),
#   provider = "STORET",
#   ask = FALSE
# )

# USGS only from DR
test3 = dataRetrieval::read_waterdata_samples(
  siteTypeName = c("Stream", "Lake"),
  countryFips = "US",
  characteristicGroup = "Organics, Pesticide",
  activityMediaName = c("Water"),
  activityStartDateLower = "01-01-2020",
  dataType = "results",
  dataProfile = "fullphyschem"
)

# # both USGS and WQX from DR
# test4 <- dataRetrieval::readWQPdata(
#   sampleMedia = c("Water", "water"),
#   startDateLo = "01-01-1991",
#   startDateHi = "12-31-2005",
#   service = "ResultWQX3",
#   dataProfile = "fullPhysChem",
#   ignore_attributes = TRUE,
#   bBox = c(-110.495, 45.192, -109.693, 45.911)
# )

######################################################################

tada1 <- TADA_DataRetrieval(
  statecode = "AZ", 
  startDate = "2026-08-18",
  endDate = "2026-09-18",
  providers = "NWIS",
  ask = FALSE, 
  applyautoclean = FALSE
)

tada2 <- TADA_DataRetrieval(
  statecode = "AZ", 
  startDate = "2026-08-18",
  endDate = "2026-09-18",
  providers = "STORET",
  ask = FALSE, 
  applyautoclean = FALSE
)

tada3 <- TADA_DataRetrieval(
  statecode = "AZ", 
  startDate = "2026-08-18",
  endDate = "2026-09-18",
  ask = FALSE, 
  applyautoclean = FALSE
)
