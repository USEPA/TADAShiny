# Download example data

# original
start_date <- "2018-01-01"
end_date <- "2023-01-01"

# start_date <- "2018-07-08"
# end_date <- "2018-07-10"
monitoring_location_id <- "REDLAKE_WQX-LRC"

df0 <- EPATADA::TADA_DataRetrieval(
  # organization = c("REDLAKE_WQX",
  #                  "SFNOES_WQX",
  #                  "PUEBLO_POJOAQUE",
  #                  "FONDULAC_WQX",
  #                  "PUEBLOOFTESUQUE", "CNENVSER"),
  siteid = monitoring_location_id,
  startDate = start_date,
  endDate = end_date,
  ask=FALSE)

# FIRST
# Categorize depth data as surface, middle and bottom
df1 <- EPATADA::TADA_FlagDepthCategory(df0, 
                                    bycategory = "no", # require no in shiny?
                                    bottomvalue = 2, # include option for user input in shiny
                                    surfacevalue = 2, # include option for user input in shiny
                                    dailyagg = "none", # include option for user input in shiny, be aware if user chooses to calculate "avg", "min", or "max" this will increase the number of rows in the dataset
                                    clean = FALSE) # require FALSE in shiny?

# SECOND
# ID location/date/characteristic combinations in the data set that can be used for depth profile plots or analysis
 # This output will be used to generate options for the plot below
df2 <- EPATADA::TADA_IDDepthProfiles(df1, 
                                  nresults = TRUE, 
                                  nvalue = 2, # include option for user input in shiny 
                                  aggregates = FALSE)
#  get the list of options 
# unique(df1$TADA.ComparableDataIdentifier)
# characteristics_group <- c('TEMPERATURE, WATER_NA_NA_DEG C', 
#                   'DEPTH, SECCHI DISK DEPTH_NA_NA_M',
#                   'DISSOLVED OXYGEN (DO)_NA_NA_MG/L')

# check that they are all in that list
# all(characteristics_group %in% unique(df1$TADA.ComparableDataIdentifier))


# THIRD
# Users can now explore depth profiles for selected characteristics (up to 3) at specific site on a single date
# TADA_DepthProfilePlot can plot up to three characteristics against depth
# EPATADA::TADA_DepthProfilePlot(df1, 
#                                groups = c('TEMPERATURE, WATER_NA_NA_DEG C', 
#                                             'DEPTH, SECCHI DISK DEPTH_NA_NA_M',
#                                             'DISSOLVED OXYGEN (DO)_NA_NA_MG/L'), # second in shiny, after users select a specific location and date, users should select up to three chars (use TADA::TADA_IDDepthProfiles output (df2) to generate list of options)
#                             location = monitoring_location_id, # first in shiny, users should select a specific location and date (use TADA::TADA_IDDepthProfiles output (df2) to generate list of options)
#                             activity_date = "2018-07-09", # first in shiny, users should select a specific location and date (use TADA::TADA_IDDepthProfiles output (df2) to generate list of options)
#                             depthcat = TRUE, # include check box in shiny to turn off or on on plot. This could also be a TADA_DepthProfilePlot function update to inlcude the check bix in the fig itself instead of just shiny?
#                             surfacevalue = 2, # include option for user input on shiny
#                             bottomvalue = 2, # include option for user input on shiny
#                             unit = "m") # include option for user input on shiny ("ft", "m", "in")
