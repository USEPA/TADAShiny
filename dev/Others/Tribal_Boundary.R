### This script downloads the tribal boundary

# Clear work space
rm(list = ls())

# Load packages
library(tidyverse)
library(EPATADA)
library(sf)
library(mapview)

### Download the tribal_area_type

# Alaska Native Allotments
AN <- TADA_TribalOptions(tribal_area_type = "Alaska Native Allotments",
                         return_sf = TRUE)

# American Indian Reservations
AIR <- TADA_TribalOptions(tribal_area_type = "American Indian Reservations",
                         return_sf = TRUE)

# Off-reservation Trust Lands
OTL <- TADA_TribalOptions(tribal_area_type = "Off-reservation Trust Lands",
                          return_sf = TRUE)

# Oklahoma Tribal Statistical Areas
OTS <- TADA_TribalOptions(tribal_area_type = "Oklahoma Tribal Statistical Areas",
                          return_sf = TRUE)

### Plot the map for comparisons

# Load the Region 8 HUC
load("inst/extdata/InputData40.RData")

# AN
mapview(HUC8_dat) + mapview(AN)

# AIR
mapview(HUC8_dat) + mapview(AIR)

# OTL
mapview(HUC8_dat) + mapview(OTL)

# OTS
mapview(HUC8_dat) + mapview(OTS)



