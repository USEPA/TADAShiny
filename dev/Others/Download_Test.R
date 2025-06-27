library(tigris)
library(mapview)
library(tidyverse)
library(EPATADA)
library(dataRetrieval)

county_sf <- counties()
mapview(county_sf)

a <- TADA_DataRetrieval(countycode = "US:56:037")


b <- readWQPdata(countycode = "US:56:037")


# aa <- whatWQPdata(bBox = c(-116.67286, 43.020714, -109.284509, 45.966425),
#                   characteristicName = "Ammonia and ammonium",
#                   startDateLo = "2020-01-01", startDateHi = "2023-01-01")
# 
# bb <- readWQPsummary(bBox = c(-116.67286, 43.020714, -109.284509, 45.966425),
#                      characteristicName = "Ammonia and ammonium",
#                      summaryYears = "all")

aa <- whatWQPdata(bBox = c(-116.67286, 43.020714, -109.284509, 45.966425),
                  characteristicName = "Ammonia and ammonium",
                  startDateLo = "2020-01-01", startDateHi = "2023-01-01")

bb <- readWQPsummary(bBox = c(-116.67286, 43.020714, -109.284509, 45.966425),
                     characteristicName = "Ammonia and ammonium",
                     summaryYears = "all")

site_all <- bb

site_all_list <- split(site_all, site_all$HUCEightDigitCode)
site_all2 <- purrr::map_dfr(site_all_list, cumsum_group, col = "ResultCount", threshold = 25000)
site_all3 <- site_all2 |> summarized_year()