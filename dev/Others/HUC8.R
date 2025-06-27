# ### This script loads the HUC8 dataset
# 
# library(tidyverse)
# library(sf)
# 
# # Load the data
# HUC8_dat <- st_read(dsn = "C:/Users/User/Work/TT_Backup2/WY_Tool/WBD_National_GDB/WBD_National_GDB.gdb",
#                     layer = "WBDHU8")
# 
# # Simplify the HUC8_dat to save space
# HUC8_dat <- HUC8_dat %>%
#   st_transform(crs = 5070) %>%
#   st_simplify(dTolerance = 1e3) %>%
#   st_transform(crs = 4326)
# 
# save.image("inst/extdata/HUC8.RData")
# 
