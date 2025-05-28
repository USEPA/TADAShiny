### This script downloads the tribal boundary

# Clear work space
rm(list = ls())

### Plot the map for comparisons

# Load the Region 8 HUC
load("inst/extdata/HUC8.RData")
load("inst/extdata/tribal_boundary.RData")

# AN
mapview::mapView(HUC8_dat) + mapview::mapView(tribal_list$`Alaska Native Allotments`)

# AIR
mapview::mapView(HUC8_dat) + mapview::mapView(tribal_list$`American Indian Reservations`)

# OTL
mapview::mapView(HUC8_dat) + mapview::mapView(tribal_list$`Off-reservation Trust Lands`)

# OTS
mapview::mapView(HUC8_dat) + mapview::mapView(tribal_list$`Oklahoma Tribal Statistical Areas`)

### Test the TADA_DataRetrieval function with the arguments using tribes as inputs

# Create a function that performs the EPATADA::TADA_DataRetrieval with purrr::possibly
# to handle the error case when downloading tribal data
poss_TADA_DataRetrieval <- EPATADA::TADA_DataRetrieval |>
  purrr::possibly(otherwise = TADA_download_temp)

# AN
AN_dat <- EPATADA::TADA_DataRetrieval(
  startDate = "2019-01-01",
  endDate = "2020-01-31",
  tribal_area_type = "Oklahoma Tribal Statistical Areas",
  tribe_name_parcel = c("Iowa Tribe of Oklahoma"),
  ask = FALSE
)



