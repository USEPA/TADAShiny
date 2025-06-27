# # Download tribal data when loading the tool
# Alaska_Native_Allotments <-
#   EPATADA::TADA_TribalOptions(tribal_area_type = "Alaska Native Allotments",
#                               return_sf = TRUE) |>
#   # Change PARCEL_NO to TRIBE_NAME consistency with other data frames
#   dplyr::rename(TRIBE_NAME = PARCEL_NO)
# 
# # American Indian Reservations
# American_Indian_Reservations <-
#   EPATADA::TADA_TribalOptions(tribal_area_type = "American Indian Reservations",
#                               return_sf = TRUE)
# 
# # Off-reservation Trust Lands
# Off_reservation_Trust_Lands <-
#   EPATADA::TADA_TribalOptions(tribal_area_type = "Off-reservation Trust Lands",
#                               return_sf = TRUE)
# 
# # Oklahoma Tribal Statistical Areas
# Oklahoma_Tribal_StatisticalAreas <-
#   EPATADA::TADA_TribalOptions(tribal_area_type = "Oklahoma Tribal Statistical Areas",
#                               return_sf = TRUE)
# 
# # Create a list containing the four data frames
# tribal_list <- list(
#   "Alaska Native Allotments" = Alaska_Native_Allotments,
#   "American Indian Reservations" = American_Indian_Reservations,
#   "Off-reservation Trust Lands" = Off_reservation_Trust_Lands,
#   "Oklahoma Tribal Statistical Areas" = Oklahoma_Tribal_StatisticalAreas
# )
# 
# rm(Alaska_Native_Allotments)
# rm(American_Indian_Reservations)
# rm(Off_reservation_Trust_Lands)
# rm(Oklahoma_Tribal_StatisticalAreas)
# 
# save.image("./inst/extdata/tribal_boundary.RData")
