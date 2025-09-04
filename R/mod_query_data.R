#' query_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
# Load the input data
data_path1 <- app_sys("extdata/statecodes_df.Rdata")
load(data_path1)

data_path2 <- app_sys("extdata/query_choices.Rdata")
load(data_path2)

data_path3 <- app_sys("extdata/tribal_boundary.RData")
load(data_path3)

data_path4 <- app_sys("extdata/TADA_Download_Temp.RData")
load(data_path4)

# Create a function that performs the EPATADA::TADA_DataRetrieval with purrr::possibly
# to handle the error case when downloading tribal data
poss_TADA_DataRetrieval <- EPATADA::TADA_DataRetrieval %>%
  purrr::possibly(otherwise = TADA_download_temp)

# Create a function that performs the dataRetrieval::whatWQPdata with purrr::possibly
# to handle the error case
poss_whatWQPdata <- dataRetrieval::whatWQPdata %>%
  purrr::possibly(otherwise = NULL)

# A function to return the tribal data frame with tribal name as an sf object
return_tribal_sf <- function(tribal_layer, tribal_name, tribal_list = tribal_list) {
  tribal_data2 <- tribal_list %>%
    purrr::pluck(tribal_layer) %>%
    dplyr::filter(TRIBE_NAME %in% tribal_name)

  return(tribal_data2)
}

# new (2024-05-23) list for new Country/Ocean(s) Query the Water Quality Portal option. Not included in saved query_choices file
countrycode_url <- "https://www.waterqualitydata.us/Codes/countrycode?mimeType=json"
countryocean_source <- jsonlite::fromJSON(txt = countrycode_url)
countryocean_source <- countryocean_source$codes %>% dplyr::select(-one_of("providers"))
countryocean_source <- countryocean_source[order(countryocean_source$desc), ]
countryocean_choices <- countryocean_source$value
names(countryocean_choices) <- countryocean_source$desc

# # Last run by CAM on 09/16/24
# county = readr::read_tsv(url("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"), col_names = FALSE)
# county = county%>%tidyr::separate(X1,into = c("STUSAB","STATE","COUNTY","COUNTY_NAME","COUNTY_ID"), sep=",")
# orgs = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Organization.CSV"))$ID)
# chars = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV"))$Name)
# chargroup = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicGroup.CSV"))$Name)
# media = c(unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/ActivityMedia.CSV"))$Name),"water","Biological Tissue","No media")
# # sitetype = unique(utils::read.csv(url("https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV"))$Name)
# sitetype = c("Aggregate groundwater use","Aggregate surface-water-use","Aggregate water-use establishment","Atmosphere","Estuary","Facility","Glacier","Lake, Reservoir, Impoundment","Land","Not Assigned","Ocean","Spring","Stream","Subsurface","Well","Wetland")
# projects = unique(data.table::fread("https://www.waterqualitydata.us/data/Project/search?mimeType=csv&zip=no&providers=NWIS&providers=STORET")$ProjectIdentifier)
# mlids = unique(data.table::fread("https://www.waterqualitydata.us/data/Station/search?mimeType=csv&zip=no")$MonitoringLocationIdentifier)
# save(orgs, chars, chargroup, media, county, sitetype, projects, mlids2, file = "inst/extdata/query_choices.Rdata")

mod_query_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      htmltools::h3("Option A: Use example data"),
      column(3, shiny::selectInput(
        ns("example_data"),
        "Use example data",
        choices = c(
          "",
          "Nutrients Utah (15k results)",
          "EPA Region 5 May 1-7 2019 (172k results)",
          "Tribal (136k results)"
        )
      ))
    ),
    shiny::fluidRow(column(
      3,
      shiny::actionButton(
        ns("example_data_go"),
        "Load",
        shiny::icon("truck-ramp-box"),
        disabled = TRUE,
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )),
    htmltools::hr(),
    shiny::fluidRow(
      htmltools::h3("Option B: Query the Water Quality Portal (WQP)"),
      "Use the fields below to download a dataset directly from WQP. Fields with '(s)' in the label allow multiple selections. 
      Hydrologic Units may be at any scale, from subwatershed to region. However, be mindful that large queries may time out."
    ),
    htmltools::br(),
    # styling several fluid rows with columns to hold the input drop down widgets
    htmltools::h4("Date Range"),
    shiny::fluidRow(
      column(
        4,
        shiny::dateInput(
          ns("startDate"),
          "Start Date",
          format = "yyyy-mm-dd",
          startview = "year"
        )
      ),
      column(
        4,
        shiny::dateInput(
          ns("endDate"),
          "End Date",
          format = "yyyy-mm-dd",
          startview = "year"
        )
      )
    ),
    htmltools::h4("Location Information"),
    "Choose at least one spatial location from the following options. If multiple options are used, the locations must be overlapping.",
    htmltools::br(),
    shiny::fluidRow(
      column(4, shiny::selectizeInput(ns("countryocean"),
        "Country/Ocean(s)",
        choices = NULL,
        multiple = TRUE
      ))
    ),
    shiny::fluidRow(
      column(4, shiny::selectizeInput(ns("state"), "State", choices = NULL)),
      column(
        4,
        shiny::selectizeInput(ns("county"), "County (pick state first)", choices = NULL)
      )
    ),
    shiny::fluidRow(
      column(4, shiny::selectizeInput(ns("tribe_layer"), "Tribe Data Layers",
        choices = NULL
      )),
      column(
        4,
        shiny::selectizeInput(ns("tribe_name"), "Tribe Name (pick data layers first)",
          choices = NULL
        )
      )
    ),
    shiny::fluidRow(
      column(
        6,
        shiny::strong("Provide the latitude and longitude by drawing a rectangle on the map"),
        htmltools::br(),
        mod_map_bboxUI(ns("BBox_map"))
      )
    ),
    shiny::fluidRow(
      column(
        4,
        shiny::selectizeInput(ns("siteid"),
          "Monitoring Location ID(s)",
          choices = NULL,
          multiple = TRUE
        )
      )
    ),
    htmltools::h4("Metadata Filters"),
    shiny::fluidRow(
      column(
        4,
        shiny::selectizeInput(
          ns("org"),
          "Organization(s)",
          choices = NULL,
          options = list(placeholder = "Start typing or use drop down menu"),
          multiple = TRUE
        )
      ),
      column(
        4,
        shiny::selectizeInput(
          ns("project"),
          "Project(s)",
          choices = NULL,
          options = list(placeholder = "Start typing or use drop down menu"),
          multiple = TRUE
        )
      ),
      column(
        4,
        shiny::selectizeInput(
          ns("type"),
          "Site Type(s)",
          choices = c(sitetype),
          options = list(placeholder = "Start typing or use drop down menu"),
          multiple = TRUE
        )
      )
    ),
    shiny::fluidRow(
      column(
        4,
        shiny::selectizeInput(
          ns("media"),
          tags$span(
            "Sample Media",
            tags$i(
              class = "glyphicon glyphicon-info-sign",
              style = "color:#0072B2;",
              title = "TADA is designed to work with water data"
            )
          ),
          choices = c("", media),
          selected = c("Water", "water"),
          multiple = TRUE
        )
      ),
      column(
        4,
        shiny::selectizeInput(
          ns("characteristic"),
          "Characteristic(s)",
          choices = NULL,
          options = list(placeholder = "Start typing or use drop down menu"),
          multiple = TRUE
        )
      ),
      column(
        4,
        shiny::selectizeInput(
          ns("chargroup"),
          "Characteristic Group",
          choices = NULL,
          options = list(placeholder = "Start typing or use drop down menu"),
          multiple = TRUE
        )
      )
    ),
    shiny::fluidRow(
      column(
        4,
        shiny::radioButtons(ns("providers"),
          "Data Source",
          c("NWIS (USGS)" = "NWIS", "WQX (EPA)" = "STORET", "Both (NWIS and WQX)" = "all"),
          selected = "all"
        )
      )
    ),
    shiny::fluidRow(column(
      4,
      shiny::actionButton(ns("querynow"), "Run Query", shiny::icon("cloud"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    )),
    htmltools::hr(),
    shiny::fluidRow(
      htmltools::h3("Option C: Upload dataset"),
      htmltools::HTML((
        "Upload a compatible dataset from your computer. This upload feature only accepts data in .xls and .xlsx formats. Data must be formatted in the EPA Water Quality eXchange (WQX) schema (and include all columns required for this TADA R Shiny application) to run
                                    this tool. The file can be a <B>fresh</B> dataset you created using the TADA template below or a <B>working</B> dataset that you downloaded from this application using the Download Working Dataset feature, and are now returning to the
                                    app to iterate on."
      )),
      # widget to upload WQP profile or WQX formatted spreadsheet
      column(
        9,
        shiny::fileInput(
          ns("file"),
          "",
          multiple = TRUE,
          accept = c(".xlsx", ".xls"),
          width = "100%"
        )
      )
    ),
    shiny::fluidRow(
      htmltools::HTML(
        "Download a blank TADA data template in .xlsx format. This template is available to assist users that do not have data available in the WQP (and therefore cannot use Option B) prepare their data for upload to this R Shiny application using import Option C.
          You may reach out to the TADA team through the helpdesk at mywaterway@epa.gov for assistance preparing your data. If your data is not in the WQP yet and you are interested in submitting it, you may reach out to the WQX helpdesk at WQX@epa.gov for assistance preparing and submitting your data
                                    to the WQP through EPA's WQX.<br><br>"
      ),
      column(
        9,
        shiny::downloadButton(
          ns("download_template"),
          "Download Template",
          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
        )
      )
    ),
    htmltools::hr(),
    shiny::fluidRow(
      htmltools::h3("Optional: Upload Progress File"),
      htmltools::HTML((
        "Upload a progress file from your computer. This upload feature only accepts data in the .RData format.
        The TADA Shiny application keeps track of all user selections, and makes a .RData file
        available for download at any time. If you saved a progress file you generated during a
        previous use of the TADA Shiny application, then it can be uploaded here and used
        to automatically parameterize the TADA Shiny app with the same selections. This file can
        be used to regenerate a dataset with the same decisions as before, or can be used
        to apply the same user selections to a new dataset"
      )),
      # widget to upload WQP profile or WQX formatted spreadsheet
      column(
        9,
        shiny::fileInput(
          ns("progress_file"),
          "",
          multiple = TRUE,
          accept = c(".RData"),
          width = "100%"
        )
      )
    ),
  )
}

#' query_data Server Functions
#'
#' @noRd
mod_query_data_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Call the bbox map module and capture its return value
    bbox_data <- mod_map_bboxServer("BBox_map")

    ## creates download template button used for importing data to TADAShiny - used in option C
    template_data <- shiny::reactive(EPATADA::TADA_GetTemplate())
    # return an ms excel file with the template columns
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        base::paste0("tada_template", ".xlsx")
      },
      content = function(file) {
        ## format csv.  contentType = "text/csv"
        # write.csv(template_data(), file)
        ## format excel (xlsx)
        d <- template_data()
        writexl::write_xlsx(d, path = file, use_zip64 = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    ## greys out Load button for example data until file has been selected
    # https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput
    shiny::observeEvent(input$example_data, {
      if (!is.na(input$example_data) && nchar(input$example_data) > 1) {
        shinyjs::enable("example_data_go")
      }
    })

    ####################

    # handles option C user data uploads
    shiny::observeEvent(input$file, {
      # a modal that pops up showing it's working on uploading the dataset from the users file
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Uploading dataset from excel file ...",
        session = shiny::getDefaultReactiveDomain()
      )

      success <- FALSE # Flag to track if the process completes successfully

      tryCatch(
        {
          # Temporarily treat warnings as errors
          old_warn <- options("warn")
          options(warn = 2)

          # Validate file input
          if (is.null(input$file)) {
            stop("No file uploaded.")
          }

          # user uploaded data
          raw <- readxl::read_excel(input$file$datapath, sheet = 1, col_types = "text")

          # Validate data structure
          if (!is.data.frame(raw)) {
            stop("Uploaded file is not a valid data frame.")
          }

          # Check for multiple rows
          if (nrow(raw) <= 1) {
            stop("The uploaded file must contain more than one row.")
          }

          # Define the required columns
          required_cols <- c(
            "ActivityMediaName", "ResultMeasureValue", "ResultMeasure.MeasureUnitCode",
            "CharacteristicName", "ResultSampleFractionText", "MethodSpeciationName",
            "DetectionQuantitationLimitMeasure.MeasureUnitCode", "ResultDetectionConditionText",
            "ResultIdentifier", "DetectionQuantitationLimitMeasure.MeasureValue",
            "LatitudeMeasure", "LongitudeMeasure"
          )

          # Check for missing columns
          missing_cols <- setdiff(required_cols, names(raw))

          # If any required columns are missing, stop processing and show an error
          if (length(missing_cols) > 0) {
            stop(paste(
              "Data upload is missing required column(s).",
              "Please make sure the following columns are included:",
              paste(missing_cols, collapse = ", ")
            ))
          }

          # run autoclean
          raw <- EPATADA::TADA_AutoClean(raw)

          # check for ALL required fields (after autoclean is run)
          if (!EPATADA::TADA_CheckRequiredFields(raw)) {
            stop("The uploaded file is missing required columns.")
          }

          success <- TRUE # Set flag to true if all operations succeed

          # Restore warning options
          options(old_warn)
        },
        error = function(e) {
          # Restore warning options in case of error
          options(old_warn)

          # Log error details for debugging
          cat("Error: ", e$message, "\n")

          # Show error notification to the user
          shiny::showNotification(
            ui = tagList(htmltools::h4(htmltools::strong("Error")), 
                         htmltools::hr(style = "margin-top: 5px; margin-bottom: 5px;"), # Adds a separator line
                         paste(e$message)),
            type = "error",
            duration = NULL,
            id = "uploadError"
          )
        }
      )

      # Ensure spinner is removed regardless of success or error
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())

      # If successful, initialize table and add blank TADA.Remove column
      if (success == TRUE) {
        # add empty TADA.Remove column
        raw$TADA.Remove <- NULL

        initializeTable(tadat, raw)

        if (!is.null(tadat$original_source)) {
          tadat$original_source <- "Upload"
        }

        # Clear any existing notification with the same ID
        shiny::removeNotification("uploadError")
      }
    })

    ####################

    # Read the TADA progress file
    shiny::observe({
      shiny::req(input$progress_file)
      # user uploaded data
      readFile(tadat, input$progress_file$datapath)
    })

    # if user presses example data button, make tadat$raw the one of the example_data contained within the TADA package.
    shiny::observeEvent(input$example_data_go, {
      # a modal that pops up showing it's working on querying the portal
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Loading example data...",
        session = shiny::getDefaultReactiveDomain()
      )

      tadat$example_data <- input$example_data
      if (input$example_data == "EPA Region 5 May 1-7 2019 (172k results)") {
        raw <- EPATADA::TADA_AutoClean(EPATADA::Data_R5_TADAPackageDemo)
      }
      if (input$example_data == "Tribal (136k results)") {
        raw <- EPATADA::Data_6Tribes_5y
      }
      if (input$example_data == "Nutrients Utah (15k results)") {
        raw <- EPATADA::Data_Nutrients_UT
      }
      initializeTable(tadat, raw)

      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())
      
      disableLoading()
    })

    # this section has widget update commands for the selectizeinputs that have a lot of possible selections - shiny suggested hosting the choices server-side rather than ui-side
    shiny::updateSelectizeInput(
      session,
      "state",
      choices = c(unique(statecodes_df$STUSAB)),
      selected = character(0),
      options = list(placeholder = "Select state", maxItems = 1),
      server = TRUE
    )
    shiny::updateSelectizeInput(session,
      "org",
      choices = c(orgs),
      server = TRUE
    )
    shiny::updateSelectizeInput(
      session,
      "chargroup",
      choices = c(chargroup),
      # selected = character(0),
      # options = list(placeholder = ""),
      options = list(placeholder = "Start typing or use drop down menu"),
      server = TRUE
    )
    shiny::updateSelectizeInput(session,
      "characteristic",
      choices = c(chars),
      server = TRUE
    )
    shiny::updateSelectizeInput(session,
      "project",
      choices = c(projects),
      options = list(placeholder = "Start typing or use drop down menu"),
      server = TRUE
    )
    shiny::updateSelectizeInput(
      session,
      "siteid",
      choices = c(mlids2),
      options = list(placeholder = "Start typing or use drop down menu"),
      server = TRUE
    )
    shiny::updateSelectizeInput(
      session,
      "countryocean",
      choices = countryocean_choices,
      selected = character(0),
      options = list(placeholder = "Start typing or use drop down menu"),
      server = TRUE
    )
    shiny::updateSelectizeInput(
      session,
      "tribe_layer",
      choices = names(tribal_list),
      selected = character(0),
      options = list(placeholder = "Select tribal data layer", maxItems = 1),
      server = TRUE
    )

    # this observes when the user inputs a state into the drop down and subsets the choices for counties to only those counties within that state.
    shiny::observeEvent(input$state, {
      state_counties <- subset(county, county$STUSAB == input$state)
      shiny::updateSelectizeInput(
        session,
        "county",
        choices = c(unique(state_counties$COUNTY_NAME)),
        selected = character(0),
        options = list(
          placeholder = "Select county",
          maxItems = 1
        ),
        server = TRUE
      )
    })

    # this observes when the user inputs a tribal data layer into the drop down and subsets the choices for data layer to only those tribes within that dataset.
    shiny::observeEvent(input$tribe_layer, {
      tribal_names <- sort(tribal_list[[input$tribe_layer]][["TRIBE_NAME"]])
      shiny::updateSelectizeInput(
        session,
        "tribe_name",
        choices = tribal_names,
        selected = character(0),
        options = list(
          placeholder = "Select tribe name or ID",
          maxItems = 1
        ),
        server = TRUE
      )
    })

    # remove the modal once the dataset has been pulled
    shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())

    # this event observer is triggered when the user hits the "Query Now" button, and then runs the TADAdataRetrieval function
    shiny::observeEvent(input$querynow, {
      tadat$original_source <- "Query"
      # convert to null when needed
      if (input$state == "") {
        # changing inputs of "" or NULL to "null"
        tadat$statecode <- "null"
      } else {
        tadat$statecode <- input$state
      }
      if (input$county == "") {
        tadat$countycode <- "null"
      } else {
        tadat$countycode <- input$county
      }
      # this is an overloaded field which can be 2-character Country or Ocean
      if (is.null(input$countryocean)) {
        tadat$countrycode <- "null"
      } else {
        tadat$countrycode <- input$countryocean
      }
      if (is.null(input$providers) | input$providers == "all") {
        tadat$providers <- "null"
      } else {
        tadat$providers <- input$providers
      }
      # if (input$huc == "") {
      #   tadat$huc <- "null"
      # } else {
      #   tadat$huc <- gsub("\\s", "", input$huc)
      # }
      if (is.null(input$siteid)) {
        tadat$siteid <- "null"
      } else {
        tadat$siteid <- input$siteid
      }
      if (is.null(input$type)) {
        tadat$siteType <- "null"
      } else {
        tadat$siteType <- input$type
      }
      if (is.null(input$chargroup)) {
        tadat$characteristicType <- "null"
      } else {
        tadat$characteristicType <- input$chargroup
      }
      if (is.null(input$characteristic)) {
        tadat$characteristicName <- "null"
      } else {
        tadat$characteristicName <- input$characteristic
      }
      if (is.null(input$media)) {
        tadat$sampleMedia <- "null"
      } else {
        tadat$sampleMedia <- input$media
      }
      if (is.null(input$project)) {
        tadat$project <- "null"
      } else {
        tadat$project <- input$project
      }
      if (is.null(input$org)) {
        tadat$organization <- "null"
      } else {
        tadat$organization <- input$org
      }

      if (length(input$endDate) == 0) {
        # ensure if date is empty, the query receives a proper input ("null")
        tadat$endDate <- "null"
      } else {
        tadat$endDate <- as.character(input$endDate)
      }
      if (length(input$startDate) == 0) {
        # ensure if date is empty, the query receives a proper input ("null")
        tadat$startDate <- "null"
      } else {
        tadat$startDate <- as.character(input$startDate)
      }

      # If there are tribal information, get the tribal as a polygon
      if (!input$tribe_layer %in% "" & !input$tribe_name %in% "") {
        # ensure if date is empty, the query receives a proper input ("null")
        tribal_sf_object <- return_tribal_sf(
          tribal_layer = input$tribe_layer,
          tribal_name = input$tribe_name,
          tribal_list = tribal_list
        )
        tadat$tribal_boundary <- tribal_sf_object
        tadat$tribal_bBox <- unname(sf::st_bbox(tribal_sf_object))
      } else {
        tadat$tribal_boundary <- "null"
        tadat$tribal_bBox <- "null"
      }

      # Handle bounding box data
      if (!is.null(bbox_data$bBox)) {
        tadat$bBox <- bbox_data$bBox
      } else {
        tadat$bBox <- "null"
      }

      # If use tribal dataset, use the tribal dataset's bbox
      # Otherwise, use the bbox from tadat
      bbox_reactive <- shiny::reactive({
        if (!inherits(tadat$tribal_boundary, "sf")) {
          tadat$bBox
        } else {
          tadat$tribal_bBox
        }
      })

      # a modal that pops up showing it's working on querying the portal
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = "Querying WQP database...",
        session = shiny::getDefaultReactiveDomain()
      )

      # a section to estimate the sample size
      shiny::showModal(shiny::modalDialog(
        title =
          "Downloading the data ...",
        footer = NULL
      ))

      # Create the list of input arguments for dataRetrieval::readWQPsummary
      args_temp <- args_create(
        statecode = tadat$statecode,
        countycode = tadat$countycode,
        countrycode = tadat$countrycode,
        siteid = tadat$siteid,
        siteType = tadat$siteType,
        characteristicName = tadat$characteristicName,
        characteristicType = tadat$characteristicType,
        sampleMedia = tadat$sampleMedia,
        project = tadat$project,
        organization = tadat$organization,
        startDateLo = tadat$startDate,
        startDateHi = tadat$endDate,
        providers = tadat$providers,
        bBox = bbox_reactive()
      )

      # Get the data summary
      result_summary <- dataRetrieval::whatWQPdata(args_temp)

      # Check if anything is outside the tribal's shapefile boundary
      if (inherits(tadat$tribal_boundary, "sf")) {
        # Convert result_summary to sf object
        result_summary_sf <- result_summary %>%
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
          sf::st_transform(crs = sf::st_crs(tadat$tribal_boundary))

        # Filter the sites within the tribal boundary
        result_summary_sf_filter <- result_summary_sf %>%
          sf::st_filter(tadat$tribal_boundary)

        result_summary <- result_summary_sf_filter %>%
          sf::st_set_geometry(NULL)
      }

      # A warning section to show if the sample size is zero
      if (nrow(result_summary) == 0) {
        shinyalert::shinyalert(
          title = "Empty Query",
          text = "Your query returned zero results. Please adjust your search inputs and try again. Remember to update the start and end dates.",
          type = "warning"
        )
        shiny::removeModal()
        return()
      }

      tot_sites <- result_summary %>%
        dplyr::group_by(MonitoringLocationIdentifier) %>%
        dplyr::summarise(tot_n = sum(resultCount)) %>%
        dplyr::filter(tot_n > 0) %>%
        dplyr::arrange(tot_n)

      # A warning section to show if the sample size is zero
      if (nrow(tot_sites) == 0) {
        shinyalert::shinyalert(
          title = "Empty Query",
          text = "Your query returned zero results. Please adjust your search inputs and try again. Remember to update the start and end dates.",
          type = "warning"
        )
        shiny::removeModal()
        return()
      }

      # Separate the sites into small and big sites

      # Set the cut point to decide the small or big sites
      maxrecs <- 100000
      pretty_maxrecs <- prettyNum(maxrecs, big.mark = ",", scientific = FALSE)

      smallsites <- tot_sites %>% dplyr::filter(tot_n <= maxrecs)
      bigsites <- tot_sites %>% dplyr::filter(tot_n > maxrecs)

      # Set other location inputs to be NULL as site ID is available
      args_temp2 <- args_temp

      args_temp2[["statecode"]] <- NULL
      args_temp2[["countycode"]] <- NULL
      args_temp2[["countrycode"]] <- NULL
      args_temp2[["bBox"]] <- NULL

      # Download the data for small sites
      if (nrow(smallsites) > 0) {
        smallsitesgrp <- smallsites %>%
          dplyr::mutate(group = MESS::cumsumbinning(
            x = tot_n,
            threshold = maxrecs,
            maxgroupsize = 300
          ))

        smallsites_list <- list()

        small_title <- base::paste0(
          "Downloading data from sites with less than or equal to ", pretty_maxrecs,
          " results."
        )

        shiny::withProgress(message = small_title, detail = "0%", value = 0, {
          for (i in 1:max(smallsitesgrp$group)) {
            shiny::incProgress(1 / max(smallsitesgrp$group),
              detail = base::paste0(round(i / max(smallsitesgrp$group) * 100), "%")
            )

            small_site_chunk <- subset(
              smallsitesgrp$MonitoringLocationIdentifier,
              smallsitesgrp$group == i
            )

            args_temp_small <- args_temp2

            args_temp_small[["siteid"]] <- small_site_chunk

            # Download the result data
            smallsites_result_temp <- dataRetrieval::readWQPdata(args_temp_small,
              dataProfile = "resultPhysChem",
              ignore_attributes = TRUE
            )

            # Download the site data
            smallsites_site_temp <- dataRetrieval::whatWQPsites(args_temp_small)

            # Download the project data
            smallsites_project_temp <- dataRetrieval::readWQPdata(args_temp_small,
              service = "Project",
              ignore_attributes = TRUE
            )

            # Create TADA data frame
            TADAprofile_smallsites_temp <- EPATADA::TADA_JoinWQPProfiles(
              FullPhysChem = smallsites_result_temp,
              Sites = smallsites_site_temp,
              Projects = smallsites_project_temp
            ) %>%
              dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

            # Assign the data to the list
            smallsites_list[[i]] <- TADAprofile_smallsites_temp
          }
        })

        # Combine the data
        TADA_smallsites <- dplyr::bind_rows(smallsites_list)

        # Apply TADA_autoclean
        TADA_smallsites_clean <- EPATADA::TADA_AutoClean(TADA_smallsites) %>%
          dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
      } else {
        TADA_smallsites_clean <- TADA_download_temp
      }

      # Download the data for big sites
      if (nrow(bigsites) > 0) {
        bigsites_list <- list()

        bsitesvec <- unique(bigsites$MonitoringLocationIdentifier)

        big_title <- base::paste0(
          "Downloading data from sites with greater than ", pretty_maxrecs,
          " results."
        )

        shiny::withProgress(message = big_title, detail = "0%", value = 0, {
          for (i in 1:length(bsitesvec)) {
            shiny::incProgress(1 / length(bsitesvec),
              detail = base::paste0(round(i / length(bsitesvec) * 100), "%")
            )

            args_temp_big <- args_temp2

            args_temp_big[["siteid"]] <- bsitesvec[i]

            # Download the result data
            bigsites_result_temp <- dataRetrieval::readWQPdata(args_temp_big,
              dataProfile = "resultPhysChem",
              ignore_attributes = TRUE
            )

            # Download the site data
            bigsites_site_temp <- dataRetrieval::whatWQPsites(args_temp_big)

            # Download the project data
            bigsites_project_temp <- dataRetrieval::readWQPdata(args_temp_big,
              service = "Project",
              ignore_attributes = TRUE
            )

            # Create TADA data frame
            TADAprofile_bigsites_temp <- EPATADA::TADA_JoinWQPProfiles(
              FullPhysChem = bigsites_result_temp,
              Sites = bigsites_site_temp,
              Projects = bigsites_project_temp
            ) %>%
              dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))

            # Assign the data to the list
            bigsites_list[[i]] <- TADAprofile_bigsites_temp
          }
        })

        # Combine the data
        TADA_bigsites <- dplyr::bind_rows(bigsites_list)

        # Apply TADA_autoclean
        TADA_bigsites_clean <- EPATADA::TADA_AutoClean(TADA_bigsites) %>%
          dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
      } else {
        TADA_bigsites_clean <- TADA_download_temp
      }
      
      disableLoading()
      
      # Combine the Small and Big sites
      raw <- dplyr::bind_rows(TADA_smallsites_clean, TADA_bigsites_clean)

      # Convert the column types
      raw <- raw %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), ~ {
          col_name <- dplyr::cur_column()
          target_class <- class(TADA_download_temp_type[[col_name]])[1]
          switch(target_class,
            "integer" = as.integer(.x),
            "numeric" = as.numeric(.x),
            "logical" = as.logical(.x),
            "Date" = as.Date(.x),
            "factor" = as.factor(.x),
            as.character(.x) # default case
          )
        }))

      # remove the modal once the dataset has been pulled
      shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())

      # show a modal dialog box when tadat$raw is empty and the query didn't return any records.
      # but if tadat$raw isn't empty, perform some initial QC of data that aren't media type water
      # or have NA Resultvalue and no detection limit data
      if (dim(raw)[1] < 1) {
        shiny::showModal(
          shiny::modalDialog(
            title = "Empty Query",
            "Your query returned zero results. Please adjust your search inputs and try again. Remember to update the start and end dates."
          )
        )
      } else {
        initializeTable(tadat, raw)
      }
    })

    # Update the run parameters if example data is selected
    shiny::observeEvent(input$example_data_go, {
      tadat$original_source <- "Example"
    })

    # Populate the boxes if a progress file is loaded
    shiny::observeEvent(tadat$load_progress_file, {
      if (!is.na(tadat$load_progress_file)) {
        if (tadat$original_source == "Example") {
          shiny::updateSelectInput(session, "example_data", selected = tadat$example_data)
        } else if (tadat$original_source == "Query") {
          shiny::updateSelectizeInput(session, "state", selected = tadat$statecode)
          shiny::updateSelectizeInput(session, "county", selected = tadat$countycode)
          shiny::updateSelectizeInput(session, "siteid", selected = tadat$siteid)
          shiny::updateSelectizeInput(session, "type", selected = tadat$siteType)
          shiny::updateSelectizeInput(session, "characteristic", selected = tadat$characteristicName)
          shiny::updateSelectizeInput(session, "chargroup", selected = tadat$characteristicType)
          shiny::updateSelectizeInput(session, "media", selected = tadat$sampleMedia)
          shiny::updateSelectizeInput(session, "project", selected = tadat$project)
          shiny::updateSelectizeInput(session, "org", selected = tadat$organization)
          shiny::updateDateInput(session, "startDate", value = tadat$startDate)
          shiny::updateDateInput(session, "endDate", value = tadat$endDate)
        }
        disableLoading()
      }
    })
  })
}

initializeTable <- function(tadat, raw) {
  # Test to see if this is a raw table or one previously worked on in TADA
  if ("TADA.Remove" %in% names(raw)) {
    tadat$reup <- TRUE
    tadat$ovgo <- FALSE
    shinyjs::enable(selector = '.nav li a[data-value="Overview"]')
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
    shinyjs::enable(selector = '.nav li a[data-value="Filter"]')
    shinyjs::enable(selector = '.nav li a[data-value="Censored"]')
    shinyjs::enable(selector = '.nav li a[data-value="Harmonize"]')
    shinyjs::enable(selector = '.nav li a[data-value="Figures"]')
    shinyjs::enable(selector = '.nav li a[data-value="Review"]')
  } else {
    tadat$new <- TRUE # this is used to determine if the app should go to the overview page first - only for datasets that are new to TADAShiny
    tadat$ovgo <- TRUE # load data into overview page
    shinyjs::enable(selector = '.nav li a[data-value="Overview"]')
    shinyjs::enable(selector = '.nav li a[data-value="Flag"]')
    # shinyjs::enable(selector = '.nav li a[data-value="Figures"]')
    # Set flagging column to FALSE
    raw$TADA.Remove <- FALSE
  }


  
  
  removals <- data.frame(matrix(nrow = nrow(raw), ncol = 0))
  tadat$raw <- raw
  tadat$removals <- removals

  # display the download buttons
  tadat$ready_for_download <- TRUE
}

disableLoading <- function() {
      # disable the button and show text telling the user to reload TADAShiny if they want to restart with new data
      shinyjs::disable("#query_data_1-example_data_go")
      shinyjs::disable("querynow")
      shinyjs::hide("file")
      shinyjs::disable("progress_file")
      shiny::insertUI(
          selector = "#query_data_1-example_data_go", # Insert relative to the button
          where = "afterEnd",    # Place it immediately after the button
          ui = tags$span("Reload the TADAShiny app to load new data", style = "margin-left: 10px;") # The text to insert
      )
      shiny::insertUI(
          selector = "#query_data_1-querynow", # Insert relative to the button
          where = "afterEnd",    # Place it immediately after the button
          ui = tags$span("Reload the TADAShiny app to Query the Water Quality Portal", style = "margin-left: 10px;") # The text to insert
      )
      shiny::insertUI(
          selector = "#query_data_1-download_template", # Insert relative to the button
          where = "beforeBegin",    # Place it immediately after the button
          ui = tags$span(HTML("Reload the TADAShiny app to Upload dataset<br><br>"), style = "margin-left: 10px;") # The text to insert
      )
      shiny::insertUI(
          selector = "#query_data_1-progress_file_progress", # Insert relative to the button
          where = "beforeBegin",    # Place it immediately after the button
          ui = tags$span(HTML("Reload the TADAShiny app to Upload Progress File<br><br>"), style = "margin-left: 10px;") # The text to insert
      )
}

## To be copied in the UI
# mod_query_data_ui("query_data_1")

## To be copied in the server
# mod_query_data_server("query_data_1")
