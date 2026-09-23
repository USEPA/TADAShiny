## code used to prepare extdata files

# Update and save Country/Ocean(s) choice list
countrycode_url <- "https://www.waterqualitydata.us/Codes/countrycode?mimeType=json"
countryocean_source <- jsonlite::fromJSON(txt = countrycode_url)$codes
countryocean_source <- dplyr::select(countryocean_source, -dplyr::any_of("providers"))
countryocean_source <- dplyr::arrange(countryocean_source, desc)
countryocean_choices <- countryocean_source$value
# names(countryocean_choices) <- countryocean_source$desc
saveRDS(countryocean_choices, file.path("inst", "extdata", "countryocean.rds"))

# Update extdata/mlids for WQP ML drop down mod_query_data.R
# Define the URL of the web service
url <- "https://www.waterqualitydata.us/data/Station/search?mimeType=csv&zip=no"
# Use httr::GET to make a GET request to the web service
response <- httr::GET(url)
# Use httr::content to parse the CSV content from the response
csv_content <- httr::content(response, "text")
# Use base::read.csv to read the CSV content into a data frame
data <- read.csv(text = csv_content, stringsAsFactors = FALSE)
# Extract unique monitoring location identifiers
mlids <- unique(data$MonitoringLocationIdentifier)
# Save the unique monitoring location identifiers
saveRDS(mlids, file.path("inst", "extdata", "mlids.rds"))

# Save each object as an .rds in inst/extdata (one object per file)
saveRDS(filter_dat, file.path("inst", "extdata", "filter_descriptions.rds"))
saveRDS(statecodes_df, file.path("inst", "extdata", "statecodes_df.rds"))
saveRDS(tribal_list, file.path("inst", "extdata", "tribal_list.rds"))
saveRDS(TADA_download_temp, file.path("inst", "extdata", "TADA_download_temp.rds"))
saveRDS(TADA_download_temp_type, file.path("inst", "extdata", "TADA_download_temp_type.rds"))

# Call file inside a function or server code
filter_dat <- readRDS(system.file("extdata", "filter_descriptions.rds", package = "TADAShiny"))
statecodes_df <- readRDS(system.file("extdata", "statecodes_df.rds", package = "TADAShiny"))
mlids <- readRDS(system.file("extdata", "mlids.rds", package = "TADAShiny"))
tribal_list <- readRDS(system.file("extdata", "tribal_list.rds", package = "TADAShiny"))
TADA_download_temp <- readRDS(system.file("extdata", "TADA_download_temp.rds", package = "TADAShiny"))
TADA_download_temp_type <- readRDS(system.file("extdata", "TADA_download_temp_type.rds", package = "TADAShiny"))
countryocean_choices <- readRDS(system.file("extdata", "countryocean.rds", package = "TADAShiny"))
