# Minimal safe helpers to avoid failing on install/lazy load/CI
.tadas_offline <- function() {
  nzchar(Sys.getenv("TADAS_OFFLINE", "")) # set TADAS_OFFLINE=true in CI to force offline
}

# source of Example UI options and then loaded data frames
# for the example datasets.  These are used to populate the
# example dataset dropdown and load the data when selected.
get_example_data_map <- function() {
  m <- list(
    "Utah Nutrients (15k results)" = function() EPATADA::Data_Nutrients_UT,
    "EPA Region 5 May 1-7 2019 (173k results)" = function() {
      EPATADA::Data_R5_TADAPackageDemo
    },
    "Six Tribal Nations (143k results)" = function() EPATADA::Data_TribalNations
  )
  m
}

# Build map lazily so objects are only touched when selected
example_data_map <- get_example_data_map()

.safe_req_string <- function(u, timeout = 30, max_tries = 3) {
  if (.tadas_offline()) {
    return(NULL)
  }
  tryCatch(
    {
      httr2::request(u) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = max_tries) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_string()
    },
    error = function(e) NULL
  )
}

# Generic: fetch a CSV and return a unique vector from a column; else default
.safe_fetch_csv_column <- function(u, column, default = character()) {
  txt <- .safe_req_string(u)
  if (is.null(txt)) {
    return(default)
  }
  dt <- tryCatch(
    data.table::fread(txt, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(dt) || !column %in% names(dt)) {
    return(default)
  }
  unique(dt[[column]])
}

# Projects: return ProjectIdentifier vector; else empty
.safe_fetch_projects <- function(u) {
  txt <- .safe_req_string(u)
  if (is.null(txt)) {
    return(character())
  }
  dt <- tryCatch(
    data.table::fread(txt, showProgress = FALSE),
    error = function(e) NULL
  )
  if (is.null(dt) || !"ProjectIdentifier" %in% names(dt)) {
    return(character())
  }
  unique(dt$ProjectIdentifier)
}

# County: census file has no header; on failure return empty data.frame with expected columns
.safe_fetch_county <- function(u) {
  txt <- .safe_req_string(u)
  cols <- c(
    "STATE_CD",
    "STATE_FIPS",
    "COUNTY_FIPS",
    "COUNTY_NAME",
    "COUNTY_FOOBAR"
  )
  # dataRetrieval::read_waterdata_samples needs "US:{STATE_FIPS}"
  # and "US:{STATE_FIPS}:{COUNTY_FIPS}"
  # and EPATADA::TADA_DataRetrieval needs "STATE_CD" and COUNTY_NAME
  if (is.null(txt)) {
    return(data.frame(
      STATE_CD = character(),
      STATE_FIPS = character(),
      COUNTY_FIPS = character(),
      COUNTY_NAME = character(),
      COUNTY_FOOBAR = character(),
      stringsAsFactors = FALSE
    ))
  }
  dt <- tryCatch(
    data.table::fread(
      txt,
      header = FALSE,
      col.names = cols,
      showProgress = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(dt)) {
    return(data.frame(
      STATE_CD = character(),
      STATE_FIPS = character(),
      COUNTY_FIPS = character(),
      COUNTY_NAME = character(),
      COUNTY_FOOBAR = character(),
      stringsAsFactors = FALSE
    ))
  }
  as.data.frame(dt)
}

.format_wqp_query_error_message <- function(error_message) {
  if (is.null(error_message)) {
    return("An error occurred while querying WQX (EPA). Please try again.")
  }

  message_text <- gsub("\033\\[[0-9;]*m", "", as.character(error_message), perl = TRUE)  
  
  normalized_message <- tolower(message_text)

  timeout_detected <- grepl(
    "timeout|timed out|gateway timeout|\\b504\\b",
    normalized_message,
    perl = TRUE
  )

  if (timeout_detected) {
    return(paste(
      "The query timed out before the Water Quality Portal could return data.",
      "Try reducing the bounding box size or adding more filters, then run the query again."
    ))
  }

  too_large_detected <- grepl(
    "uri too long|url too long|request-uri too large|\\b414\\b",
    normalized_message,
    perl = TRUE
  )

  if (too_large_detected) {
    return(paste(
      "The query returned too many sites for the Water Quality Portal to fetch in one request",
      "(the request URL exceeded the server length limit).",
      "Try reducing the bounding box size or adding more filters, then run the query again."
    ))
  }

  if (!nzchar(message_text)) {
    return("An error occurred while querying WQX (EPA). Please try again.")
  }

  paste("An error occurred while querying WQX (EPA):", message_text)
}

TADA_download_temp <- readRDS(system.file(
  "extdata",
  "TADA_download_temp.rds",
  package = "TADAShiny"
))
tribal_list <- readRDS(system.file(
  "extdata",
  "tribal_list.rds",
  package = "TADAShiny"
))

# A function to return the tribal data frame with tribal name as an sf object
return_tribal_sf <- function(
  tribal_layer,
  tribal_name,
  tribal_list = tribal_list
) {
  tribal_data2 <- tribal_list |>
    purrr::pluck(tribal_layer) |>
    dplyr::filter(TRIBE_NAME %in% tribal_name)

  return(tribal_data2)
}

# Load Country/Ocean(s) choice list
countryocean_choices <- readRDS(system.file(
  "extdata",
  "countryocean.rds",
  package = "TADAShiny"
))

# Fetch Project choices (safe)
project_url <- "https://www.waterqualitydata.us/data/Project/search?mimeType=csv&zip=no&providers=NWIS&providers=STORET"
projects <- .safe_fetch_projects(project_url)

# Fetch County choices
# Beware that some of the counties are historic, see: https://github.com/DOI-USGS/dataRetrieval/issues/711
# Using USGS counties from dataRetrieval does not resolve https://github.com/USEPA/TADAShiny/issues/231
# Fetch County choices (safe)
counties <- .safe_fetch_county(
  "https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt"
)

# Fetch orgs, chars, chargroup, media choices (safe)
orgs <- .safe_fetch_csv_column(
  "https://cdx.epa.gov/wqx/download/DomainValues/Organization.CSV",
  "ID",
  default = character()
)

chars <- .safe_fetch_csv_column(
  "https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV",
  "Name",
  default = character()
)

chargroup <- .safe_fetch_csv_column(
  "https://cdx.epa.gov/wqx/download/DomainValues/CharacteristicGroup.CSV",
  "Name",
  default = character()
)

media <- c(
  .safe_fetch_csv_column(
    "https://cdx.epa.gov/wqx/download/DomainValues/ActivityMedia.CSV",
    "Name",
    default = character()
  ),
  "Biological Tissue",
  "No media"
)

# sitetype <- c(
#       unique(utils::read.csv(url(
#         "https://cdx.epa.gov/wqx/download/DomainValues/MonitoringLocationType.CSV"
#       ))$Name),
#       "Glacier", "Aggregate water-use establishment", "Not Assigned", "Subsurface"
#       )

sitetype <- c(
  "Aggregate groundwater use",
  "Aggregate surface-water-use",
  "Aggregate water-use establishment",
  "Atmosphere",
  "Estuary",
  "Facility",
  "Glacier",
  "Lake, Reservoir, Impoundment",
  "Land",
  "Not Assigned",
  "Ocean",
  "Spring",
  "Stream",
  "Subsurface",
  "Well",
  "Wetland"
)

# these are the types of text matches used in searching the Characteristic(s) list
match_types <- c(
  "Starts With" = "starts_with",
  "Ends With" = "ends_with",
  "Contains" = "contains",
  "Equals" = "matches"
)

# start of UI
# Updated UI with lighter subgroup styling, subtle background clusters,
# reworded location guidance, reordered Site ID(s), and consistent Tribal Data styling
mod_query_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Enable Bootstrap tooltips globally
    tags$script(HTML(
      "$(function () { $('[data-toggle=\"tooltip\"]').tooltip({container: 'body'}); });"
    )),
    # Card styling and spacing system
    tags$head(htmltools::tags$style(htmltools::HTML(
      "
    /* ============================
       TADA - Typography baseline
       ============================ */
    :root{
      --tada-font-family: system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, 'Noto Sans', 'Liberation Sans', sans-serif;
      --tada-font-size-base: 16px;     /* 1rem */
      --tada-line-height: 1.5;
      --tada-text-color: #111827;      /* gray-900 */
      --tada-muted-color: #4b5563;     /* gray-600 */
      --tada-font-size-sm: 0.875rem;   /* ~14px */
      --tada-font-size-md: 1rem;       /* 16px */
      --tada-font-size-lg: 1.125rem;   /* 18px */
      --tada-h3-size: 1.125rem;        /* 18px */
      --tada-h4-size: 1rem;            /* 16px */
      --tada-headings-weight: 600;
      --tada-headings-line-height: 1.25;
    }

    html, body {
      font-family: var(--tada-font-family);
      font-size: var(--tada-font-size-base);
      line-height: var(--tada-line-height);
      color: var(--tada-text-color);
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }

    /* Card base (kept from your original, plus typography) */
    .tada-card {
      background: #ffffff;
      border: 1px solid #e5e7eb;
      border-radius: 8px;
      padding: 20px;
      margin-bottom: 20px;
      font-size: var(--tada-font-size-md);
      line-height: var(--tada-line-height);
      color: var(--tada-text-color);
    }

    /* Headings and rhythm within cards */
    .tada-card h3 {
      margin: 0 0 10px;
      font-size: var(--tada-h3-size);
      line-height: var(--tada-headings-line-height);
      font-weight: var(--tada-headings-weight);
    }
    .tada-card h4 {
      margin: 16px 0 8px;
      font-size: var(--tada-h4-size);
      line-height: var(--tada-headings-line-height);
      font-weight: var(--tada-headings-weight);
    }

    /* Labels, help text, and paragraph copy */
    .tada-card p,
    .tada-card .help-block,
    .tada-card .shiny-text-output,
    .tada-card .control-label {
      margin: 6px 0 10px;
    }
    .tada-card .control-label {
      font-size: var(--tada-font-size-md);
      font-weight: 600;
      line-height: 1.4;
      color: var(--tada-text-color);
    }
    .tada-note {
      font-size: var(--tada-font-size-sm);
      color: var(--tada-muted-color);
      margin: 6px 0 12px;
    }

    /* Form/input sizing (Bootstrap + Selectize) */
    .tada-card .form-group { margin-bottom: 10px; }
    .tada-card .form-control,
    .selectize-control .selectize-input,
    .selectize-dropdown .option,
    .selectize-dropdown .optgroup-header,
    .selectize-control .item {
      font-size: var(--tada-font-size-md);
      line-height: 1.4;
      font-family: var(--tada-font-family);
      color: var(--tada-text-color);
    }
    /* Placeholder text for selectize */
    .selectize-control .selectize-input input::placeholder {
      color: #9CA3AF; /* gray-400 */
      opacity: 1;
    }

    /* Details (collapsible panel) */
    .tada-details { margin-top: 8px; }
    .tada-details > summary {
      cursor: pointer;
      margin-bottom: 8px;
      font-size: var(--tada-font-size-md);
      font-weight: 600;
      color: var(--tada-text-color);
    }
    .tada-details[open] { margin-bottom: 6px; }

    /* Simple boxed section for nested UI (legacy use) */
    .tada-box {
      border: 1px solid #e5e7eb;
      border-radius: 6px;
      padding: 12px;
      background: #fafafa;
      margin: 8px 0 12px;
      font-size: var(--tada-font-size-md);
      line-height: var(--tada-line-height);
    }

    /* New: lighter subgroup styling (fieldset/legend) */
    .tada-fieldset {
      border: 0;
      margin: 8px 0 12px;
      padding: 8px 0 0 12px;              /* slight indent */
      border-left: 2px dotted #e5e7eb;    /* lighter, dotted accent */
    }
    .tada-fieldset .tada-legend {
      margin: 0 0 6px;
      padding: 0;
      font-size: var(--tada-font-size-md);
      font-weight: 600;
      line-height: 1.3;
      color: var(--tada-text-color);
    }

    /* Subtle background wrapper for a cluster */
    .tada-subsection-bg {
      background: #fafafa;
      border-radius: 6px;
      padding: 10px 12px;
      margin-top: 8px;
      margin-bottom: 12px;
    }

    /* Buttons (actionButton, downloadButton) */
    .tada-card .btn,
    .tada-card .btn-default,
    .tada-card .btn-primary {
      font-size: var(--tada-font-size-md);
      font-weight: 600;
      line-height: 1.2;
    }

    /* Modal titles (shiny modal + shinybusy modal-like content) */
    .modal-title,
    .modal-header h3,
    .modal-header h4 {
      font-size: var(--tada-h3-size);
      font-weight: var(--tada-headings-weight);
      line-height: var(--tada-headings-line-height);
      margin: 0;
    }

    /* Stopwatch text: monospace for steady width */
    #js_time_display {
      font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;
      font-size: var(--tada-font-size-sm);
      color: var(--tada-muted-color);
    }

    /* Row spacing and responsive stacking */
    .tada-field-row + .tada-field-row { margin-top: 12px; }
    .tada-actions { margin-top: 12px; }
    @media (max-width: 767px) {
      .tada-field-row [class*='col-'] { margin-bottom: 10px; }
      /* Slightly larger base type on small screens for readability */
      :root { --tada-font-size-base: 17px; }
    }
    
    /* Bounding box UI: responsive polish */
    .tada-bbox .leaflet-container {
      height: 420px !important; /* desktop/tablet default */
    }
    @media (max-width: 767px) {
      .tada-bbox .leaflet-container {
        height: 320px !important; /* smaller height on phones */
      }
      /* Ensure nice vertical rhythm when stacked */
      .tada-bbox .tada-bbox-map { margin-bottom: 12px; }
      .tada-bbox .tada-bbox-controls .form-group { margin-bottom: 10px; }
    }

    /* Ensure radio/checkbox labels match other labels (Data Source, etc.) */
    .tada-card .radio label,
    .tada-card .checkbox label {
      font-size: var(--tada-font-size-md);
      line-height: 1.4;
      font-weight: 400; /* keep normal weight for inline option labels */
      color: var(--tada-text-color);
      margin-bottom: 6px;
    }

    /* Make selectize dropdown menus match input font size */
    .selectize-dropdown {
      font-size: var(--tada-font-size-md);
      font-family: var(--tada-font-family);
      line-height: 1.4;
    }

    /* Slightly tighten label spacing in the bbox inputs */
    .tada-bbox .form-group .control-label { margin-bottom: 4px; }

    /* Optional: a bit taller map on very large displays */
    @media (min-width: 1200px) and (min-height: 900px) {
      .tada-bbox .leaflet-container { height: 480px !important; }
    }

    /* Leaflet attribution: keep subtle but readable */
    .leaflet-control-attribution {
      font-size: 0.75rem; /* ~12px */
      line-height: 1.2;
    }

    /* Metadata Filters: compact, aligned spacing */
    .tada-metadata .tada-field-row + .tada-field-row { margin-top: 10px; }
    .tada-metadata .control-label { margin-bottom: 6px; }
    .tada-metadata .tada-box { margin-top: 10px; }
    .tada-metadata .tada-box > .control-label { display: block; margin-bottom: 8px; }
  "
    ))),

    # Card 1 - Option A: Use example data
    htmltools::div(
      class = "tada-card",
      shiny::fluidRow(htmltools::h3("Option A: Use example data")),
      shiny::fluidRow(
        class = "tada-field-row",
        shiny::column(
          3,
          shiny::selectInput(
            ns("example_data"),
            "Select dataset to load",
            choices = c("", names(example_data_map))
          )
        )
      ),
      shiny::fluidRow(
        class = "tada-actions",
        shiny::column(
          3,
          shiny::actionButton(
            ns("example_data_go"),
            "Load",
            shiny::icon("truck-ramp-box"),
            disabled = TRUE,
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
        )
      )
    ),

    # Card 2 - Option B
    htmltools::div(
      class = "tada-card",
      shiny::fluidRow(htmltools::h3(
        "Option B: Query the Water Quality Portal (WQP)"
      )),
      htmltools::p(
        class = "tada-note",
        "Use the fields below to download a dataset directly from WQP. Fields with '(s)' in the label allow multiple selections. Be mindful that large queries may time out."
      ),

      # Location Information
      htmltools::h4("Select Location Parameters"),
      htmltools::p(
        class = "tada-note",
        "Select one or more location parameters to define the spatial extent of your dataset. ",
        "If you use multiple, they are combined with ",
        htmltools::strong("AND logic"),
        "-results must fall within the overlap of all selected locations. ",
        "All location fields are optional."
      ),

      # Subtle background wrapper for the location cluster
      htmltools::div(
        class = "tada-subsection-bg",

        # State and County subgroup
        htmltools::tags$fieldset(
          class = "tada-fieldset",
          htmltools::tags$legend(class = "tada-legend", "State and County"),
          shiny::fluidRow(
            class = "tada-field-row",
            shiny::column(
              6,
              shiny::selectizeInput(ns("state"), "State", choices = NULL)
            ),
            shiny::column(
              6,
              shiny::selectizeInput(
                ns("county"),
                "County (pick state first)",
                choices = NULL
              )
            )
          )
        ),

        # Site ID(s) immediately after State/County (tooltip on label)
        shiny::fluidRow(
          class = "tada-field-row",
          shiny::column(
            12,
            shiny::selectizeInput(
              ns("siteid"),
              shiny::tags$span(
                "Site ID(s) ",
                shiny::tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  `data-toggle` = "tooltip",
                  title = "If Site ID(s) are specified, the query is limited to those sites regardless of State, County, or Bounding Box."
                )
              ),
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Start typing or use drop down menu")
            )
          )
        ),

        # Site Type(s)
        shiny::fluidRow(
          class = "tada-field-row",
          shiny::column(
            12,
            shiny::selectizeInput(
              ns("type"),
              "Site Type(s)",
              choices = c(sitetype),
              options = list(
                placeholder = "Start typing or use drop down menu"
              ),
              multiple = TRUE
            )
          )
        ),

        # Bounding Box subgroup (map + coordinates; search pans only)
        shiny::fluidRow(
          class = "tada-field-row",
          shiny::column(
            12,
            htmltools::tags$fieldset(
              class = "tada-fieldset tada-bbox",
              htmltools::tags$legend(
                class = "tada-legend",
                "Bounding Box - Map and Coordinates"
              ),
              htmltools::p(
                class = "tada-note",
                "Define a single bounding box by drawing on the map or entering North/West/East/South coordinates."
              ),
              mod_map_bboxUI(ns("BBox_map"))
            )
          )
        )
      ), # end location cluster wrapper

      # Metadata Filters (with subtle background like Location Parameters)
      htmltools::h4("Filter Results"),
      htmltools::div(
        class = "tada-metadata tada-subsection-bg",

        # New description
        htmltools::p(
          class = "tada-note",
          "Select one or more filters to narrow your query. ",
          "If you use multiple, they are combined with ",
          htmltools::strong("AND logic"),
          "-results must fall within the overlap of all selected filters. ",
          "Note: Adjusting the Date Range is required; the default dates (today) will return no results."
        ),

        # Row 1: Sample Media, Characteristic Group
        shiny::fluidRow(
          class = "tada-field-row",
          shiny::column(
            6,
            shiny::selectizeInput(
              ns("media"),
              shiny::tags$span(
                "Sample Media ",
                shiny::tags$i(
                  class = "glyphicon glyphicon-info-sign",
                  `data-toggle` = "tooltip",
                  title = "TADA is designed to work primarily with 'Water' data"
                )
              ),
              choices = c("", media),
              selected = c("Water"),
              multiple = TRUE
            )
          ),
          shiny::column(
            6,
            shiny::selectizeInput(
              ns("chargroup"),
              "Characteristic Group",
              choices = NULL,
              options = list(
                placeholder = "Start typing or use drop down menu"
              ),
              multiple = TRUE
            )
          )
        ),

        # Characteristic(s) subgroup
        htmltools::tags$fieldset(
          class = "tada-fieldset",
          htmltools::tags$legend(class = "tada-legend", "Characteristic(s)"),
          shiny::fluidRow(
            class = "tada-field-row",
            shiny::column(
              width = 3,
              shiny::selectizeInput(
                inputId = ns("match_type_selector"),
                label = "1. Match type",
                choices = match_types,
                selected = "contains",
                multiple = FALSE
              )
            ),
            shiny::column(
              width = 3,
              shiny::textInput(
                inputId = ns("text_string"),
                label = "2. Search string",
                value = ""
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectizeInput(
                inputId = ns("characteristic_select"),
                label = "3. Select matching characteristics",
                choices = NULL,
                multiple = TRUE,
                options = list(
                  placeholder = "Start typing or use drop down menu",
                  openOnFocus = TRUE,
                  plugins = list("remove_button")
                )
              )
            )
          )
        ),

        # Date Range subgroup (Required; tooltip on legend)
        htmltools::tags$fieldset(
          class = "tada-fieldset",
          htmltools::tags$legend(
            class = "tada-legend",
            shiny::tags$span(
              "Date Range (Required) ",
              shiny::tags$i(
                class = "glyphicon glyphicon-info-sign",
                `data-toggle` = "tooltip",
                title = "Default dates are today (returns no results). Enter a date range, or clear both dates if other filters sufficiently limit your query. For timeouts, shorten the range or add filters."
              )
            )
          ),
          shiny::fluidRow(
            class = "tada-field-row",
            shiny::column(
              4,
              shiny::dateInput(
                ns("startDate"),
                "Start Date",
                format = "yyyy-mm-dd",
                startview = "year"
              )
            ),
            shiny::column(
              4,
              shiny::dateInput(
                ns("endDate"),
                "End Date",
                format = "yyyy-mm-dd",
                startview = "year"
              )
            )
          )
        )
      ),

      # Data Source (with subtle background like Location Parameters)
      htmltools::h4("Data Source"),
      htmltools::div(
        class = "tada-subsection-bg",

        shiny::fluidRow(
          class = "tada-field-row",
          shiny::column(
            4,
            shiny::radioButtons(
              ns("providers"),
              label = NULL,
              c(
                "USGS (Samples Data API)" = "NWIS",
                "EPA (WQX)" = "STORET",
                "Both (USGS and EPA)" = "all"
              ),
              selected = "all"
            )
          )
        ),

        # Hint when not WQX
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] !== 'STORET'", ns("providers")),
          htmltools::div(
            class = "tada-note",
            htmltools::HTML(
              "<em>Select <strong>EPA (WQX)</strong> as the Data Source to enable additional filters below.</em>"
            )
          )
        ),

        # Additional Filters (EPA WQX only)
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] === 'STORET'", ns("providers")),
          htmltools::tags$details(
            class = "tada-details",
            open = "open",
            htmltools::tags$summary(
              "Additional Filters Only Compatible With the EPA (WQX) Data Source"
            ),
            htmltools::div(
              # Filters row
              shiny::fluidRow(
                class = "tada-field-row",
                shiny::column(
                  4,
                  shiny::selectizeInput(
                    ns("countryocean"),
                    "Country/Ocean(s)",
                    choices = NULL,
                    multiple = TRUE
                  )
                ),
                shiny::column(
                  4,
                  shiny::selectizeInput(
                    ns("org"),
                    shiny::tags$span("Organization(s)"),
                    choices = NULL,
                    options = list(
                      placeholder = "Start typing or use drop down menu"
                    ),
                    multiple = TRUE
                  )
                ),
                shiny::column(
                  4,
                  shiny::selectizeInput(
                    ns("project"),
                    "Project(s)",
                    choices = NULL,
                    options = list(
                      placeholder = "Start typing or use drop down menu"
                    ),
                    multiple = TRUE
                  )
                )
              ),

              # Tribal Data subgroup (match fieldset look)
              htmltools::tags$fieldset(
                class = "tada-fieldset",
                htmltools::tags$legend(
                  class = "tada-legend",
                  "Tribal Data (requires both fields)"
                ),
                shiny::fluidRow(
                  class = "tada-field-row",
                  shiny::column(
                    5,
                    shiny::selectizeInput(
                      ns("tribe_layer"),
                      "Step 1 - Tribal Data Layer",
                      choices = NULL
                    )
                  ),
                  shiny::column(
                    7,
                    shiny::selectizeInput(
                      ns("tribe_name"),
                      "Step 2 - Tribe Name",
                      choices = NULL
                    )
                  )
                )
              )
            )
          )
        )
      ),

      # Run Query
      shiny::fluidRow(
        class = "tada-actions",
        shiny::column(
          4,
          shiny::actionButton(
            ns("querynow"),
            "Run Query",
            shiny::icon("cloud"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          )
        )
      )
    ),

    # Card 3 - Option C (Upload) + Optional Progress File
    htmltools::div(
      class = "tada-card",
      # Option C: Upload dataset
      shiny::fluidRow(htmltools::h3("Option C: Upload dataset")),
      shiny::fluidRow(
        class = "tada-field-row",
        htmltools::HTML(
          "Upload a compatible dataset from your computer. This upload feature only accepts data in .xls and .xlsx formats. Data must be formatted in the EPA Water Quality eXchange (WQX) schema (and include all columns required for this TADA R Shiny application) to run this tool. The file can be a <b>fresh</b> dataset you created using the TADA template below or a <b>working</b> dataset that you downloaded from this application using the Download Working Dataset feature, and are now returning to the app to iterate on."
        )
      ),
      shiny::fluidRow(
        class = "tada-field-row",
        shiny::column(
          9,
          shiny::tags$div(
            id = "file-upload-wrapper",
            shiny::fileInput(
              ns("file"),
              "",
              multiple = TRUE,
              accept = c(".xlsx", ".xls"),
              width = "100%"
            )
          )
        )
      ),
      shiny::fluidRow(
        class = "tada-field-row",
        htmltools::HTML(
          "Download a blank TADA data template in .xlsx format. This template is available to assist users that do not have data available in the WQP (and therefore cannot use Option B) prepare their data for upload to this R Shiny application using import Option C. You may reach out to the TADA team through the helpdesk at mywaterway@epa.gov for assistance preparing your data. If your data is not in the WQP yet and you are interested in submitting it, you may reach out to the WQX helpdesk at WQX@epa.gov for assistance preparing and submitting your data to the WQP through EPA's WQX."
        )
      ),
      shiny::fluidRow(
        class = "tada-actions",
        shiny::column(
          9,
          shiny::downloadButton(
            ns("download_template"),
            "Download Template",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
          )
        )
      ),

      # Optional: Upload Progress File
      htmltools::hr(),
      shiny::fluidRow(htmltools::h3("Optional: Upload Progress File")),
      shiny::fluidRow(
        class = "tada-field-row",
        htmltools::HTML(
          "Upload a progress file from your computer. This upload feature only accepts data in the .RData format. The TADA Shiny application keeps track of all user selections, and makes a .RData file available for download at any time. If you saved a progress file you generated during a previous use of the TADA Shiny application, then it can be uploaded here and used to automatically parameterize the TADA Shiny app with the same selections. This file can be used to regenerate a dataset with the same decisions as before, or can be used to apply the same user selections to a new dataset."
        )
      ),
      shiny::fluidRow(
        class = "tada-field-row",
        shiny::column(
          9,
          shiny::tags$div(
            id = "progress-file-wrapper",
            shiny::fileInput(
              ns("progress_file"),
              "",
              multiple = TRUE,
              accept = c(".RData"),
              width = "100%"
            )
          )
        )
      )
    ),

    # JavaScript implementing the stopwatch (client-side)
    shiny::tags$script(HTML(
      "
(function () {
    var running = false;
    var startTs = null;
    var acc = 0;
    var rafId = null;
    var lastSent = 0;

    function pad(n) { return (n < 10 ? '0' : '') + n; }
    function formatMs(ms) {
        var totalSec = Math.floor(ms / 1000);
        var s = totalSec % 60;
        var m = Math.floor(totalSec / 60) % 60;
        var h = Math.floor(totalSec / 3600);
        return pad(h) + ':' + pad(m) + ':' + pad(s);
    }

    function update() {
        var now = performance.now();
        var elapsed = acc;
        if (running && startTs !== null) elapsed += (now - startTs);
        var disp = 'Elapsed Time: ' + formatMs(elapsed);
        var el = document.getElementById('js_time_display');
        if (el) el.textContent = disp;

        if (now - lastSent > 500) {
            var secondsVal = Math.floor(elapsed / 1000);
            var hidden = document.getElementById('js_elapsed_seconds');
            if (hidden) hidden.value = secondsVal;
            if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('js_elapsed_seconds', secondsVal, {priority: 'event'});
            }
            lastSent = now;
        }

        rafId = window.requestAnimationFrame(update);
    }

    rafId = window.requestAnimationFrame(update);

    function startTimerNow() {
        acc = 0;
        startTs = performance.now();
        running = true;
        var el = document.getElementById('js_time_display');
        if (el) el.textContent = 'Elapsed Time: 00:00:00';
        lastSent = 0;
    }

    function stopTimerNow() {
        if (running && startTs !== null) {
            var now = performance.now();
            acc += (now - startTs);
            startTs = null;
        }
        running = false;
        var el = document.getElementById('js_time_display');
        if (el) el.textContent = 'Elapsed Time: ' + formatMs(acc);
        var secondsVal = Math.floor(acc / 1000);
        if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('js_elapsed_seconds', secondsVal, {priority: 'event'});
        }
    }

    var observer = new MutationObserver(function (muts) {
        muts.forEach(function (m) {
            m.removedNodes && m.removedNodes.forEach(function (node) {
                if (node && node.classList && node.classList.contains('modal')) {
                    if (rafId) {
                        window.cancelAnimationFrame(rafId);
                        rafId = null;
                    }
                    stopTimerNow();
                    acc = 0;
                    lastSent = 0;
                    startTs = null;
                    running = false;
                    rafId = window.requestAnimationFrame(update);
                }
            });
            m.addedNodes && m.addedNodes.forEach(function (node) {
                if (node && node.querySelector) {
                    var timer = node.querySelector('#js_time_display');
                    if (timer) startTimerNow();
                }
            });
        });
    });
    observer.observe(document.body, {childList: true, subtree: true});

    if (window.jQuery) {
        try {
            window.jQuery(document).on('shown.bs.modal', function (e) {
                if (e.target && e.target.querySelector && e.target.querySelector('#js_time_display')) {
                    startTimerNow();
                }
            });
            window.jQuery(document).on('hidden.bs.modal', function (e) {
                if (e.target && e.target.querySelector && e.target.querySelector('#js_time_display')) {
                    stopTimerNow();
                    acc = 0;
                    lastSent = 0;
                    startTs = null;
                    running = false;
                }
            });
        } catch (err) {}
    }

    var existing = document.getElementById('js_time_display');
    if (existing) existing.textContent = 'Elapsed Time: 00:00:00';
})();
"
    ))
  )
}
# end of UI

all.cols <- c(
  "ResultIdentifier",
  "ActivityTypeCode",
  "TADA.ActivityType.Flag",
  "TADA.ReplicateSampleID",
  "ActivityMediaName",
  "TADA.ActivityMediaName",
  "ActivityMediaSubdivisionName",
  "TADA.Media.Flag",
  "CountryCode",
  "StateCode",
  "CountyCode",
  "MonitoringLocationName",
  "TADA.MonitoringLocationName",
  "MonitoringLocationTypeName",
  "TADA.MonitoringLocationTypeName",
  "MonitoringLocationDescriptionText",
  "LatitudeMeasure",
  "TADA.LatitudeMeasure",
  "LongitudeMeasure",
  "TADA.LongitudeMeasure",
  "HorizontalCoordinateReferenceSystemDatumName",
  "TADA.SuspectCoordinates.Flag",
  "HUCEightDigitCode",
  "MonitoringLocationIdentifier",
  "TADA.MonitoringLocationIdentifier",
  "TADA.NearbySites.Flag",
  "TADA.NearbySiteGroup",
  "TADA.DistanceAway.Meters",
  "TADA.AURefSource",
  "ResultSampleFractionText",
  "TADA.ResultSampleFractionText",
  "TADA.SampleFraction.Flag",
  "Target.TADA.ResultSampleFractionText",
  "TADA.FractionAssumptions",
  "CharacteristicName",
  "TADA.CharacteristicName",
  "Target.TADA.CharacteristicName",
  "TADA.CharacteristicNameAssumptions",
  "SubjectTaxonomicName",
  "SampleTissueAnatomyName",
  "MethodSpeciationName",
  "TADA.MethodSpeciationName",
  "TADA.Target.MethodSpeciationName",
  "TADA.MethodSpeciation.Flag",
  "Target.TADA.MethodSpeciationName",
  "Target.TADA.SpeciationConversionFactor",
  "TADA.SpeciationAssumptions",
  "TADA.SpeciationUnitConversion",
  "TADA.SpeciationConversionFactor",
  "TADA.ComparableDataIdentifier",
  "TADA.Harmonized.Flag",
  "ActivityStartDate",
  "ActivityStartTime.Time",
  "ActivityStartTime.TimeZoneCode",
  "ActivityStartDateTime",
  "ResultMeasureValue",
  "ResultMeasure.MeasureUnitCode",
  "TADA.ResultMeasureValue",
  "TADA.ResultMeasure.MeasureUnitCode",
  "TADA.Target.ResultMeasure.MeasureUnitCode",
  "TADA.WQXUnitConversionFactor",
  "TADA.WQXUnitConversionCoefficient",
  "TADA.WQXResultUnitConversion",
  "TADA.ResultUnit.Flag",
  "ResultValueTypeName",
  "TADA.ResultMeasureValueDataTypes.Flag",
  "TADA.ResultValueAboveUpperThreshold.Flag",
  "TADA.ResultValueBelowLowerThreshold.Flag",
  "ResultDetectionConditionText",
  "DetectionQuantitationLimitTypeName",
  "DetectionQuantitationLimitMeasure.MeasureValue",
  "DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "TADA.DetectionQuantitationLimitMeasure.MeasureValue",
  "TADA.DetectionQuantitationLimitMeasure.MeasureUnitCode",
  "TADA.DetectionQuantitationLimitMeasure.MeasureValueDataTypes.Flag",
  "TADA.CensoredData.Flag",
  "TADA.CensoredMethod",
  "TADA.ConsolidatedDepth",
  "TADA.ConsolidatedDepth.Bottom",
  "TADA.ConsolidatedDepth.Unit",
  "TADA.DepthCategory.Flag",
  "TADA.DepthProfileAggregation.Flag",
  "ResultDepthHeightMeasure.MeasureValue",
  "TADA.ResultDepthHeightMeasure.MeasureValue",
  "TADA.ResultDepthHeightMeasure.MeasureValueDataTypes.Flag",
  "ResultDepthHeightMeasure.MeasureUnitCode",
  "TADA.ResultDepthHeightMeasure.MeasureUnitCode",
  "TADA.WQXConversionFactor.ResultDepthHeightMeasure",
  "ResultDepthAltitudeReferencePointText",
  "ActivityRelativeDepthName",
  "ActivityDepthHeightMeasure.MeasureValue",
  "TADA.WQXConversionFactor.ActivityDepthHeightMeasure",
  "TADA.ActivityDepthHeightMeasure.MeasureValue",
  "TADA.ActivityDepthHeightMeasure.MeasureValueDataTypes.Flag",
  "ActivityDepthHeightMeasure.MeasureUnitCode",
  "TADA.ActivityDepthHeightMeasure.MeasureUnitCode",
  "ActivityTopDepthHeightMeasure.MeasureValue",
  "TADA.ActivityTopDepthHeightMeasure.MeasureValue",
  "TADA.WQXConversionFactor.ActivityTopDepthHeightMeasure",
  "TADA.ActivityTopDepthHeightMeasure.MeasureValueDataTypes.Flag",
  "ActivityTopDepthHeightMeasure.MeasureUnitCode",
  "TADA.ActivityTopDepthHeightMeasure.MeasureUnitCode",
  "ActivityBottomDepthHeightMeasure.MeasureValue",
  "TADA.ActivityBottomDepthHeightMeasure.MeasureValue",
  "TADA.WQXConversionFactor.ActivityBottomDepthHeightMeasure",
  "TADA.ActivityBottomDepthHeightMeasure.MeasureValueDataTypes.Flag",
  "ActivityBottomDepthHeightMeasure.MeasureUnitCode",
  "TADA.ActivityBottomDepthHeightMeasure.MeasureUnitCode",
  "ResultTimeBasisText",
  "StatisticalBaseCode",
  "ResultFileUrl",
  "TADA.ContinuousData.Flag",
  "TADA.ResultValueAggregation.Flag",
  "TADA.NutrientSummation.Flag",
  "TADA.NutrientSummationGroup",
  "TADA.NutrientSummationEquation",
  "ResultAnalyticalMethod.MethodName",
  "ResultAnalyticalMethod.MethodDescriptionText",
  "ResultAnalyticalMethod.MethodIdentifier",
  "ResultAnalyticalMethod.MethodIdentifierContext",
  "ResultAnalyticalMethod.MethodUrl",
  "TADA.AnalyticalMethod.Flag",
  "SampleCollectionMethod.MethodIdentifier",
  "SampleCollectionMethod.MethodIdentifierContext",
  "SampleCollectionMethod.MethodName",
  "SampleCollectionMethod.MethodDescriptionText",
  "SampleCollectionEquipmentName",
  "MeasureQualifierCode",
  "ResultStatusIdentifier",
  "TADA.MeasureQualifierCode.Flag",
  "TADA.MeasureQualifierCode.Def",
  "ResultCommentText",
  "ActivityCommentText",
  "HydrologicCondition",
  "HydrologicEvent",
  "DataQuality.PrecisionValue",
  "DataQuality.BiasValue",
  "DataQuality.ConfidenceIntervalValue",
  "DataQuality.UpperConfidenceLimitValue",
  "DataQuality.LowerConfidenceLimitValue",
  "SamplingDesignTypeCode",
  "LaboratoryName",
  "ResultLaboratoryCommentText",
  "ActivityIdentifier",
  "OrganizationIdentifier",
  "OrganizationFormalName",
  "TADA.MultipleOrgDuplicate",
  "TADA.MultipleOrgDupGroupID",
  "TADA.ResultSelectedMultipleOrgs",
  "TADA.SingleOrgDupGroupID",
  "TADA.SingleOrgDup.Flag",
  "ProjectName",
  "ProjectDescriptionText",
  "ProjectIdentifier",
  "ProjectFileUrl",
  "QAPPApprovedIndicator",
  "QAPPApprovalAgencyName",
  "TADA.QAPPDocAvailable",
  "AquiferName",
  "AquiferTypeName",
  "LocalAqfrName",
  "ConstructionDateText",
  "WellDepthMeasure.MeasureValue",
  "WellDepthMeasure.MeasureUnitCode",
  "WellHoleDepthMeasure.MeasureValue",
  "WellHoleDepthMeasure.MeasureUnitCode",
  "ActivityDepthAltitudeReferencePointText",
  "ActivityEndDate",
  "ActivityEndTime.Time",
  "ActivityEndTime.TimeZoneCode",
  "ActivityEndDateTime",
  "ActivityConductingOrganizationText",
  "SampleAquifer",
  "ActivityLocation.LatitudeMeasure",
  "ActivityLocation.LongitudeMeasure",
  "ResultWeightBasisText",
  "ResultTemperatureBasisText",
  "ResultParticleSizeBasisText",
  "USGSPCode",
  "BinaryObjectFileName",
  "BinaryObjectFileTypeCode",
  "AnalysisStartDate",
  "ResultDetectionQuantitationLimitUrl",
  "LabSamplePreparationUrl",
  "timeZoneStart",
  "timeZoneEnd",
  "ActivityStartTime.TimeZoneCode_offset",
  "ActivityEndTime.TimeZoneCode_offset",
  "SourceMapScaleNumeric",
  "HorizontalAccuracyMeasure.MeasureValue",
  "HorizontalAccuracyMeasure.MeasureUnitCode",
  "HorizontalCollectionMethodName",
  "VerticalMeasure.MeasureValue",
  "VerticalMeasure.MeasureUnitCode",
  "VerticalAccuracyMeasure.MeasureValue",
  "VerticalAccuracyMeasure.MeasureUnitCode",
  "VerticalCollectionMethodName",
  "VerticalCoordinateReferenceSystemDatumName",
  "FormationTypeText",
  "ProjectMonitoringLocationWeightingUrl",
  "DrainageAreaMeasure.MeasureValue",
  "DrainageAreaMeasure.MeasureUnitCode",
  "ContributingDrainageAreaMeasure.MeasureValue",
  "ContributingDrainageAreaMeasure.MeasureUnitCode",
  "ProviderName",
  "LastUpdated",
  "ATTAINS.OrganizationIdentifier",
  "ATTAINS.SubmissionId",
  "ATTAINS.HasProtectionPlan",
  "ATTAINS.AssessmentUnitName",
  "ATTAINS.NhdPlusId",
  "ATTAINS.Tas303d",
  "ATTAINS.IsThreatened",
  "ATTAINS.State",
  "ATTAINS.On303dList",
  "ATTAINS.OrganizationName",
  "ATTAINS.Region",
  "ATTAINS.ShapeLength",
  "ATTAINS.ReportingCycle",
  "ATTAINS.AssmntJoinKey",
  "ATTAINS.HasTmdl",
  "ATTAINS.OrgType",
  "ATTAINS.PermIdJoinKey",
  "ATTAINS.CatchmentIsTribal",
  "ATTAINS.IrCategory",
  "ATTAINS.WaterbodyReportLink",
  "ATTAINS.AssessmentUnitIdentifier",
  "ATTAINS.OverallStatus",
  "ATTAINS.IsAssessed",
  "ATTAINS.IsImpaired",
  "ATTAINS.Has4bPlan",
  "ATTAINS.Huc12",
  "ATTAINS.HasAlternativePlan",
  "ATTAINS.VisionPriority303d",
  "ATTAINS.AreaSqkm",
  "ATTAINS.CatchmentAreaSqkm",
  "ATTAINS.CatchmentStateCode",
  "ATTAINS.CatchmentResolution",
  "ATTAINS.WaterType",
  "ATTAINS.ShapeArea",
  "TADA.Remove",
  "TADA.RemovalReason",
  "TADAShiny.tab",
  "geometry"
)

# Keep only the columns in 'keep_cols' (in order); print removed and missing
restrict_to_keep_cols <- function(df, keep_cols = all.cols, verbose = TRUE) {
  orig_names <- names(df)

  # Preserve the order you provided in keep_cols (skip those not in df)
  keep_ordered <- keep_cols[keep_cols %in% orig_names]

  # Columns to remove (present in df but not in keep list)
  removed <- setdiff(orig_names, keep_cols)

  # Columns requested but not present in df (informational only)
  missing <- setdiff(keep_cols, orig_names)

  # Subset and return
  df_out <- df[, keep_ordered, drop = FALSE]

  if (isTRUE(verbose)) {
    if (length(removed)) {
      message(
        "Removing ",
        length(removed),
        " column(s): ",
        paste(removed, collapse = ", ")
      )
    } else {
      message("No columns removed.")
    }
    if (length(missing)) {
      message(
        "Requested but not present in input (not added): ",
        paste(missing, collapse = ", ")
      )
    }
  }
  df_out
}

#' query_data Server Functions
#'
#' @noRd
mod_query_data_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Increase timeout to 5 minutes
    withr::local_options(list(timeout = max(getOption("timeout"), 300)))

    # Call the bbox map module and capture its return value
    bbox_data <- mod_map_bboxServer("BBox_map")

    ## creates download template button used for importing data to TADAShiny - used in option C
    template_data <- shiny::reactive(EPATADA::TADA_GetTemplate())

    # hold error message for NWIS queries in a reactive value so it can be displayed in a modal if needed
    nwis_error_message_text <- NULL

    # return an ms excel file with the template columns
    output$download_template <- shiny::downloadHandler(
      filename = function() {
        base::paste0("tada_template", ".xlsx")
      },
      content = function(file) {
        # format excel (xlsx)
        d <- template_data()
        writexl::write_xlsx(d, path = file, use_zip64 = TRUE)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )

    ## greys out Load button for example data until file has been selected
    # https://stackoverflow.com/questions/24175997/force-no-default-selection-in-selectinput
    shiny::observeEvent(input$example_data, {
      if (!is.null(input$example_data) && nzchar(input$example_data)) {
        shinyjs::enable("example_data_go")
      } else {
        shinyjs::disable("example_data_go")
      }
    })

    ####################

    # handles option C user data uploads
    shiny::observeEvent(input$file, {
      # extra safeguard for spinner removal even in unexpected control-flow issues
      on.exit(
        shinybusy::remove_modal_spinner(
          session = shiny::getDefaultReactiveDomain()
        ),
        add = TRUE
      )
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
          # only in interactive dev - withr will auto-restore at the end of this block
          # Consider whether you want warn = 2 to apply in Shiny deployments. If yes, remove the interactive() guard
          if (interactive()) {
            withr::local_options(list(warn = 2))
          }

          # Validate file input
          if (is.null(input$file)) {
            stop("No file uploaded.")
          }

          # added this to make sure it is not null later
          tadat$original_source <- "Upload"

          # user uploaded data
          raw <- readxl::read_excel(
            input$file$datapath,
            sheet = 1,
            col_types = "text"
          )

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
            "ActivityMediaName",
            "ResultMeasureValue",
            "ResultMeasure.MeasureUnitCode",
            "CharacteristicName",
            "ResultSampleFractionText",
            "MethodSpeciationName",
            "DetectionQuantitationLimitMeasure.MeasureUnitCode",
            "ResultDetectionConditionText",
            "ResultIdentifier",
            "DetectionQuantitationLimitMeasure.MeasureValue",
            "LatitudeMeasure",
            "LongitudeMeasure"
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
        },
        error = function(e) {
          # Log error details for debugging
          cat("Error: ", e$message, "\n")

          # Show error notification to the user
          shiny::showNotification(
            ui = tagList(
              htmltools::h4(htmltools::strong("Error")),
              htmltools::hr(style = "margin-top: 5px; margin-bottom: 5px;"),
              paste(e$message)
            ),
            type = "error",
            duration = NULL,
            id = "uploadError"
          )
        }
      )

      # Ensure spinner is removed regardless of success or error
      shinybusy::remove_modal_spinner(
        session = shiny::getDefaultReactiveDomain()
      )

      # If successful, reduce columns then initialize
      if (success == TRUE) {
        # Standardize to TADA template order before restricting
        raw <- EPATADA::TADA_OrderCols(raw)

        # Trim to keep list (prints removed columns to console)
        raw <- restrict_to_keep_cols(raw, keep_cols = all.cols, verbose = TRUE)

        # Let initializeTable add TADA.Remove for new datasets
        raw$TADA.Remove <- NULL

        initializeTable(tadat, raw)

        tadat$original_source <- "Upload"

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
      # resumed session will not trim the users input dataset, extra columns they have would be carried through
    })

    # if user presses example data button, make tadat$raw the one of the example_data contained within the TADA package.
    shiny::observeEvent(input$example_data_go, {
      # a modal that pops up showing it's working on loading the data
      shinybusy::show_modal_spinner(
        spin = "double-bounce",
        color = "#0071bc",
        text = tagList(
          shiny::tags$div(
            shiny::tags$p(
              "Loading example data",
              shiny::tags$br(),
              input$example_data
            ),
            style = "text-align:center; padding: 12px;",
            shiny::tags$p(id = "js_time_display", "00:00:00")
          ),
          # Hidden input to hold elapsed seconds for server (JS updates it)
          shiny::tags$input(
            id = "js_elapsed_seconds",
            type = "hidden",
            value = "0"
          )
        ),
        session = shiny::getDefaultReactiveDomain()
      )

      # get the data from the example_data_map based on the user's selection.
      # This is a named list of functions that each return a dataset, so we
      # call the function corresponding to the user's selection to get the dataset.
      raw <- example_data_map[[input$example_data]]()

      # Clean -> order -> restrict -> initialize
      raw <- EPATADA::TADA_AutoClean(raw)
      raw <- EPATADA::TADA_OrderCols(raw)
      raw <- restrict_to_keep_cols(raw, keep_cols = all.cols, verbose = TRUE)

      initializeTable(tadat, raw)

      shinybusy::remove_modal_spinner() # session = session)  # shiny::getDefaultReactiveDomain())

      disableLoading(session)
    })

    statecodes_df <- readRDS(system.file(
      "extdata",
      "statecodes_df.rds",
      package = "TADAShiny"
    ))

    # this section has widget update commands for the selectizeinputs that have a lot of possible selections - shiny suggested hosting the choices server-side rather than ui-side
    shiny::updateSelectizeInput(
      session,
      "state",
      choices = c(unique(statecodes_df$STUSAB)), # these are 2-character state abbreviations
      selected = character(0),
      options = list(placeholder = "Select state", maxItems = 1),
      server = TRUE
    )
    shiny::updateSelectizeInput(
      session,
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

    # A reactive expression that filters the choices based on the input pattern
    filtered_list <- shiny::reactive({
      text_string <- input$text_string

      if (is.null(text_string) || text_string == "") {
        # If the text string is empty, return all choices
        return(chars)
      } else {
        match_type <- "contains"
        if (isTRUE(nzchar(input$match_type_selector))) {
          match_type <- input$match_type_selector
        }
        # set the grep pattern for each match type
        if (match_type == "starts_with") {
          grep_pattern <- paste0("^", text_string)
        } else if (match_type == "ends_with") {
          grep_pattern <- paste0(text_string, "$")
        } else if (match_type == "matches") {
          grep_pattern <- paste0("^", text_string, "$")
        } else {
          # contains
          grep_pattern <- text_string
        }

        my_filtered_list <- chars[grep(grep_pattern, chars, ignore.case = TRUE)]

        return(my_filtered_list)
      }
    })

    # Observer to update the selectizeInput choices whenever the filtered_list changes
    shiny::observe({
      # using isolate() here is key to this whole thing working.
      # the value would be subject to an event when the updateSelectizeInput() happens below,
      # so you need to 'isolate' the current value before you run the update
      previous_selected <- shiny::isolate(input$characteristic_select)

      shiny::updateSelectizeInput(
        session,
        "characteristic_select",
        choices = c(filtered_list(), previous_selected),
        server = TRUE,
        selected = previous_selected
      )
    })

    shiny::updateSelectizeInput(
      session,
      "characteristic",
      choices = c(chars),
      server = TRUE
    )
    shiny::updateSelectizeInput(
      session,
      "project",
      choices = c(projects),
      options = list(placeholder = "Start typing or use drop down menu"),
      server = TRUE
    )
    mlids <- readRDS(system.file("extdata", "mlids.rds", package = "TADAShiny"))
    shiny::updateSelectizeInput(
      session,
      "siteid",
      choices = c(mlids),
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
      choices = c("", names(tribal_list)),
      selected = character(0),
      options = list(placeholder = "Select tribal data layer", maxItems = 1),
      server = TRUE
    )

    # this observes when the user inputs a state into the drop down and subsets the choices for counties to only those counties within that state.
    shiny::observeEvent(input$state, {
      state_counties <- subset(counties, counties$STATE_CD == input$state)
      shiny::updateSelectizeInput(
        session,
        "county",
        choices = c(unique(state_counties$COUNTY_NAME)),
        selected = character(0),
        options = list(placeholder = "Select county", maxItems = 1),
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
        options = list(placeholder = "Select tribe name or ID", maxItems = 1),
        server = TRUE
      )
    })

    # not sure why this is here
    # remove the modal once the dataset has been pulled
    # shinybusy::remove_modal_spinner(session = shiny::getDefaultReactiveDomain())

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

      if (
        (input$providers == "all" || input$providers == "NWIS") &&
          (shiny::isTruthy(input$org) ||
            shiny::isTruthy(input$project) ||
            shiny::isTruthy(input$countryocean) ||
            shiny::isTruthy(input$tribe_layer) ||
            shiny::isTruthy(input$tribe_name))
      ) {
        # display a modal and return because these are not compatible
        # browser()
        shiny::showModal(shiny::modalDialog(
          title = "Input warning",
          shiny::HTML(paste0(
            "The Data Source '<strong>USGS (Samples Data API)</strong>' is not compatible ",
            "with any of the EPA (WQX) Metadata Filters. Please either change your Data Source ",
            "selection to '<strong>EPA (WQX)</strong>' or remove any of the following filters: ",
            "Country/Ocean(s), Organization(s), Project(s), and Tribal Data."
          )),
          easyClose = TRUE
        ))
        return(NULL)
      }

      # this is used for toggling retrievals for 1 or both or the services
      providers_arg <- c("NWIS", "STORET")
      if (is.null(input$providers) | input$providers == "all") {
        tadat$providers <- "null"
      } else {
        tadat$providers <- input$providers
        providers_arg <- c(input$providers)
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
      if (is.null(input$characteristic_select)) {
        tadat$characteristicName <- "null"
      } else {
        tadat$characteristicName <- input$characteristic_select
      }
      if (is.null(input$media)) {
        tadat$sampleMedia <- "null"
      } else {
        tadat$sampleMedia <- input$media
        # "If 'Water' found in input$media then add 'water' to tadat$sampleMedia
        # this is used for some older USGS data only
        if (sum(grep("Water", input$media)) > 0) {
          tadat$sampleMedia <- append(tadat$sampleMedia, "water")
        }
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

      if ("STORET" %in% providers_arg) {
        # a modal that pops up showing it's working on querying the portal
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = tagList(
            shiny::tags$div(
              shiny::tags$p(
                "Querying Data Source",
                shiny::tags$br(),
                "EPA (WQX)"
              ),
              style = "text-align:center; padding: 12px;",
              shiny::tags$p(id = "js_time_display", "00:00:00")
            ),
            # Hidden input to hold elapsed seconds for server (JS updates it)
            shiny::tags$input(
              id = "js_elapsed_seconds",
              type = "hidden",
              value = "0"
            )
          ),
          session = shiny::getDefaultReactiveDomain()
        )

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
        # does this have recent USGS data????
        result_summary <- dataRetrieval::whatWQPdata(args_temp)

        # Check if anything is outside the tribal's shapefile boundary
        if (inherits(tadat$tribal_boundary, "sf")) {
          # Convert result_summary to sf object
          result_summary_sf <- result_summary |>
            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
            sf::st_transform(crs = sf::st_crs(tadat$tribal_boundary))

          # Filter the sites within the tribal boundary
          result_summary_sf_filter <- result_summary_sf |>
            sf::st_filter(tadat$tribal_boundary)

          result_summary <- result_summary_sf_filter |>
            sf::st_set_geometry(NULL)
        }

        # A warning section to show if the sample size is zero
        if (nrow(result_summary) == 0) {
          shiny::showModal(shiny::modalDialog(
            title = "Empty Query",
            "Your query returned zero results. Please adjust your search inputs and try again.
              Remember to update the Start Date and End Date."
          ))
          return()
        }

        tot_sites <- result_summary |>
          dplyr::group_by(MonitoringLocationIdentifier) |>
          dplyr::summarise(tot_n = sum(resultCount)) |>
          dplyr::filter(tot_n > 0) |>
          dplyr::arrange(tot_n)

        # A warning section to show if the sample size is zero
        if (nrow(tot_sites) == 0) {
          shiny::showModal(shiny::modalDialog(
            title = "Empty Query",
            "Your query returned zero results. Please adjust your search inputs and try again.
              Remember to update the start and end dates."
          ))
          return()
        }

        # Separate the sites into small and big sites

        # Set the cut point to decide the small or big sites
        maxrecs <- 100000
        pretty_maxrecs <- prettyNum(maxrecs, big.mark = ",", scientific = FALSE)

        smallsites <- tot_sites |> dplyr::filter(tot_n <= maxrecs)
        bigsites <- tot_sites |> dplyr::filter(tot_n > maxrecs)

        # Set other location inputs to be NULL as site ID is available
        args_temp2 <- args_temp

        args_temp2[["statecode"]] <- NULL
        args_temp2[["countycode"]] <- NULL
        args_temp2[["countrycode"]] <- NULL
        args_temp2[["bBox"]] <- NULL

        # Download the data for water quality monitoring locations with less than 'maxrec' records.
        if (nrow(smallsites) > 0) {
          smallsitesgrp <- smallsites |>
            dplyr::mutate(
              group = MESS::cumsumbinning(
                x = tot_n,
                threshold = maxrecs,
                maxgroupsize = 100 # 100 # changed from 300 after Warning: Error in httr2::req_perform: HTTP 414 URI Too Long.
              )
            )

          smallsites_list <- list()

          small_title <- base::paste0(
            "Downloading EPA Water Quality eXchange (WQX) data from ",
            scales::comma(nrow(smallsites)),
            " sites with less than or equal to ",
            pretty_maxrecs,
            " results."
          )

          shiny::withProgress(message = small_title, detail = "0%", value = 0, {
            for (i in 1:max(smallsitesgrp$group)) {
              shiny::incProgress(
                1 / max(smallsitesgrp$group),
                detail = base::paste0(
                  round(i / max(smallsitesgrp$group) * 100),
                  "%"
                )
              )

              small_site_chunk <- subset(
                smallsitesgrp$MonitoringLocationIdentifier,
                smallsitesgrp$group == i
              )

              args_temp_small <- args_temp2

              args_temp_small[["siteid"]] <- small_site_chunk

              TADAprofile_smallsites_temp <- NULL

              ## start of changes for using WQX3
              tryCatch(
                {
                  # Download the WQP data using the WQX3 and the full Physical Chemistry profile
                  TADAprofile_smallsites_temp <- dataRetrieval::readWQPdata(
                    args_temp_small,
                    service = "ResultWQX3",
                    dataProfile = "fullPhysChem",
                    ignore_attributes = TRUE
                  )
                  # revert names to the legacy
                  TADAprofile_smallsites_temp <- EPATADA::TADA_RenametoLegacy(
                    TADAprofile_smallsites_temp
                  )
                },
                error = function(e) {
                  # Error handling: show error message and re-enable harmonize button
                  shinybusy::remove_modal_spinner(
                    session = shiny::getDefaultReactiveDomain()
                  )
                  shiny::showModal(shiny::modalDialog(
                    title = "Error",
                    paste(
                      "An error occurred while querying WQX (EPA):",
                      e$message
                    ),
                    easyClose = TRUE
                  ))
                }
              )
              ## end of changes for using WQX3

              # Assign the data to the list
              if (
                "PreparationStartDate" %in% names(TADAprofile_smallsites_temp)
              ) {
                TADAprofile_smallsites_temp$PreparationStartDate <- as.character(
                  TADAprofile_smallsites_temp$PreparationStartDate
                )
              }
              TADAprofile_smallsites_temp <- EPATADA::TADA_AutoClean(
                TADAprofile_smallsites_temp
              )

              smallsites_list[[i]] <- TADAprofile_smallsites_temp
            }
          })
          # Combine the data
          TADA_smallsites <- dplyr::bind_rows(smallsites_list)

          # Apply TADA_autoclean
          TADA_smallsites_clean <- EPATADA::TADA_AutoClean(TADA_smallsites) |>
            dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
        } else {
          TADA_smallsites_clean <- TADA_download_temp
        }

        # Download the data for water quality monitoring locations with more than 'maxrec' records.
        if (nrow(bigsites) > 0) {
          bigsites_list <- list()

          bsitesvec <- unique(bigsites$MonitoringLocationIdentifier)

          big_title <- base::paste0(
            "Downloading STORET data from sites with greater than ",
            pretty_maxrecs,
            " results."
          )

          shiny::withProgress(message = big_title, detail = "0%", value = 0, {
            for (i in 1:length(bsitesvec)) {
              shiny::incProgress(
                1 / length(bsitesvec),
                detail = base::paste0(round(i / length(bsitesvec) * 100), "%")
              )

              args_temp_big <- args_temp2

              args_temp_big[["siteid"]] <- bsitesvec[i]

              ## start of changes for using WQX3

              # Download the WQP data using the WQX3 and the full Physical Chemistry profile
              bigsites_result_temp <- dataRetrieval::readWQPdata(
                args_temp_big,
                service = "ResultWQX3",
                dataProfile = "fullPhysChem",
                ignore_attributes = TRUE
              )
              # revert names to the legacy
              TADAprofile_bigsites_temp <- EPATADA::TADA_RenametoLegacy(
                bigsites_result_temp
              )

              # Assign the data to the list
              bigsites_list[[i]] <- TADAprofile_bigsites_temp

              ## end of changes for using WQX3
            }
          })

          # Combine the data
          TADA_bigsites <- dplyr::bind_rows(bigsites_list)

          # Apply TADA_autoclean
          TADA_bigsites_clean <- EPATADA::TADA_AutoClean(TADA_bigsites) |>
            dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
        } else {
          TADA_bigsites_clean <- TADA_download_temp
        }

        # Combine the Small and Big sites
        STORET_results <- dplyr::bind_rows(
          TADA_smallsites_clean,
          TADA_bigsites_clean
        )

        # Convert the column types
        type_template <- readRDS(system.file(
          "extdata",
          "TADA_download_temp_type.rds",
          package = "TADAShiny"
        ))
        STORET_results <- STORET_results |>
          dplyr::mutate(dplyr::across(
            tidyselect::everything(),
            ~ {
              col_name <- dplyr::cur_column()
              target_class <- class(type_template[[col_name]])[1]
              switch(
                target_class,
                integer = as.integer(.x),
                numeric = as.numeric(.x),
                logical = as.logical(.x),
                Date = as.Date(.x),
                factor = as.factor(.x),
                as.character(.x)
              )
            }
          ))

        # remove the modal once the dataset has been pulled
        shinybusy::remove_modal_spinner(
          session = shiny::getDefaultReactiveDomain()
        )
      }

      if ("NWIS" %in% providers_arg) {
        # use this to show the user something while they are waiting
        query_text_string <- NULL

        if (input$state == "") {
          state_fips_arg <- NULL
          county_fips_arg <- NULL
        } else if (input$county == "") {
          state <- utils::head(counties[counties$STATE_CD == input$state, ], 1)
          state_fips_arg <- paste(
            "US",
            sprintf("%02d", state$STATE_FIPS),
            sep = ":"
          )
          county_fips_arg <- NULL
          query_text_string <- input$state
        } else {
          county <- counties[
            counties$STATE_CD == input$state &
              counties$COUNTY_NAME == input$county,
          ]
          state_fips_arg <- paste(
            "US",
            sprintf("%02d", county$STATE_FIPS),
            sep = ":"
          )
          county_fips_arg <- paste(
            "US",
            sprintf("%02d", county$STATE_FIPS),
            sprintf("%03d", county$COUNTY_FIPS),
            sep = ":"
          )
          query_text_string <- paste(
            input$state,
            "and",
            county$COUNTY_NAME,
            sep = " "
          )
        }
        # a modal that pops up showing it's working on querying the portal
        shinybusy::show_modal_spinner(
          spin = "double-bounce",
          color = "#0071bc",
          text = tagList(
            shiny::tags$div(
              shiny::tags$p(
                "Querying Data Source",
                shiny::tags$br(),
                "USGS (Samples Data API)"
              ),
              style = "text-align:center; padding: 12px;",
              shiny::tags$p(id = "js_time_display", "00:00:00")
            ),
            # Hidden input to hold elapsed seconds for server (JS updates it)
            shiny::tags$input(
              id = "js_elapsed_seconds",
              type = "hidden",
              value = "0"
            )
          ),
          session = shiny::getDefaultReactiveDomain()
        )

        # Create the list of input arguments for dataRetrieval::read_waterdata_samples
        args_temp <- nwis_args_create(
          stateFips = state_fips_arg,
          countyFips = county_fips_arg,
          # countrycode = tadat$countrycode,
          monitoringLocationIdentifier = tadat$siteid,
          siteTypeName = tadat$siteType,
          # hydrologicUnit = TBD,
          characteristic = tadat$characteristicName,
          characteristicGroup = tadat$characteristicType,
          activityMediaName = tadat$sampleMedia,
          projectIdentifier = tadat$project,
          organizationIdentifier = tadat$organization,
          activityStartDateLower = tadat$startDate,
          activityStartDateUpper = tadat$endDate,
          # providers = tadat$providers,
          dataType = "results",
          dataProfile = "fullphyschem",
          boundingBox = bbox_reactive(),
        )

        NWIS_results <- NULL
        got_NWIS_data <- FALSE
        nwis_error_message_text <- NULL

        tryCatch(
          {
            # stop("random error is NWIS")
            NWIS_results <- do.call(
              dataRetrieval::read_waterdata_samples,
              args_temp
            )
            got_NWIS_data <- TRUE
          },
          error = function(e) {
            # Error handling: show error message and re-enable harmonize button
            nwis_error_message_text <<- paste(
              "An error occurred while querying NWIS (USGS):",
              gsub("\033\\[[0-9;]*m", "", as.character(e$message), perl = TRUE)
            )

            shinybusy::remove_modal_spinner(
              session = shiny::getDefaultReactiveDomain()
            )
          }
        )

        if (got_NWIS_data && nrow(NWIS_results) > 0) {
          NWIS_results_rename <- EPATADA::TADA_RenametoLegacy(NWIS_results)

          # TEMP FIX!!!!!!!!!
          # NWIS uses SampleAquifer and STORET and TADA use AquiferName  Change to AquiferName
          colnames(NWIS_results_rename)[
            colnames(NWIS_results_rename) == "SampleAquifer"
          ] <- "AquiferName"

          # also getting non-fatal error from NWIS only data
          # [1] "Missing the following fields that are in the csv files:"
          # [1] "TADA.QAPPDocAvailable"

          # this will not run if the df is empty
          NWIS_results_clean <- EPATADA::TADA_AutoClean(NWIS_results_rename)

          NWIS_results <- EPATADA::TADA_OrderCols(NWIS_results_clean)

          # this field is all NA but still needs to be recast as date
          # NWIS_results_ordered$Activity_EndDate <- as.Date(NWIS_results_ordered$Activity_EndDate)

          # his this one later
          # Warning: Error in dplyr::bind_rows:
          # Can't combine ..1$ActivityStartDate <character> and ..2$ActivityStartDate <date>.
          NWIS_results$ActivityStartDate <- as.character(
            NWIS_results$ActivityStartDate
          )
          NWIS_results$ActivityStartDateTime <- as.character(
            NWIS_results$ActivityStartDateTime
          )
          NWIS_results$ActivityStartTime.TimeZoneCode_offset <- as.character(
            NWIS_results$ActivityStartTime.TimeZoneCode_offset
          )
        }
      } # end of NWIS query section

      # show a modal dialog box when tadat$raw is empty and the query didn't return any records.
      # but if tadat$raw isn't empty, perform some initial QC of data that aren't media type water
      # or have NA Resultvalue and no detection limit data
      if (
        !is.null(nwis_error_message_text) && nzchar(nwis_error_message_text)
      ) {
        shiny::showModal(shiny::modalDialog(
          title = "NWIS Error",
          shiny::HTML(nwis_error_message_text),
          easyClose = FALSE, # Set to FALSE to force user to use a button to close
          footer = tagList(shiny::modalButton("Dismiss"))
        ))
      } else {
        if (exists("STORET_results") && exists("NWIS_results")) {
          if (got_NWIS_data == TRUE && nrow(NWIS_results) > 0) {
            # merge them together
            All_results <- dplyr::bind_rows(STORET_results, NWIS_results)
          } else {
            # if the NWIS query resulted in no rows, then just include these results
            All_results <- STORET_results
          }

          All_results_clean <- EPATADA::TADA_AutoClean(All_results)

          All_results_clean <- EPATADA::TADA_OrderCols(All_results_clean)
        } else if (exists("NWIS_results")) {
          # && !is.null(NWIS_results())) {
          All_results_clean <- NWIS_results
        } else if (exists("STORET_results")) {
          All_results_clean <- STORET_results
        }

        # using the NWIS retrieval this returns 0 x 181 - a list of the columns.
        if (dim(All_results_clean)[1] <= 0) {
          message_text <- "Your query returned zero results. Please adjust your search inputs and try again.
            Remember to update the start and end dates."

          shiny::showModal(shiny::modalDialog(
            title = "Empty Query",
            shiny::tags$p(message_text),
            HTML(nwis_error_message_text)
          ))
        } else {
          disableLoading(session)
          shinybusy::remove_modal_spinner(
            session = shiny::getDefaultReactiveDomain()
          )

          # Reduce to your keep list and print removed columns
          raw <- restrict_to_keep_cols(
            All_results_clean,
            keep_cols = all.cols,
            verbose = TRUE
          )
          initializeTable(tadat, raw)
        }
      }
    }) # end of observeEvent for querynow button

    # Update the run parameters if example data is selected
    shiny::observeEvent(input$example_data_go, {
      tadat$original_source <- "Example"
    })

    # Populate the boxes if a progress file is loaded
    shiny::observeEvent(tadat$load_progress_file, {
      if (!is.na(tadat$load_progress_file)) {
        if (tadat$original_source == "Example") {
          shiny::updateSelectInput(
            session,
            "example_data",
            selected = tadat$example_data
          )
        } else if (tadat$original_source == "Query") {
          shiny::updateSelectizeInput(
            session,
            "state",
            selected = tadat$statecode
          )
          shiny::updateSelectizeInput(
            session,
            "county",
            selected = tadat$countycode
          )
          shiny::updateSelectizeInput(
            session,
            "siteid",
            selected = tadat$siteid
          )
          shiny::updateSelectizeInput(
            session,
            "type",
            selected = tadat$siteType
          )
          shiny::updateSelectizeInput(
            session,
            "characteristic",
            selected = tadat$characteristicName
          )
          shiny::updateSelectizeInput(
            session,
            "chargroup",
            selected = tadat$characteristicType
          )
          shiny::updateSelectizeInput(
            session,
            "media",
            selected = tadat$sampleMedia
          )
          shiny::updateSelectizeInput(
            session,
            "project",
            selected = tadat$project
          )
          shiny::updateSelectizeInput(
            session,
            "org",
            selected = tadat$organization
          )
          shiny::updateDateInput(session, "startDate", value = tadat$startDate)
          shiny::updateDateInput(session, "endDate", value = tadat$endDate)
        }
        disableLoading(session)
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
    shinyjs::enable(selector = '.nav li a[data-value="Depth"]')
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

disableLoading <- function(session) {
  # disable the button and show text telling the user to reload TADAShiny if they want to restart with new data
  shiny::updateSelectInput(
    session,
    "example_data",
    choices = NULL,
    selected = ""
  )
  shinyjs::disable("example_data_go")
  shinyjs::disable("example_data")
  shinyjs::disable("querynow")
  shinyjs::disable("file")
  shinyjs::disable("progress_file")
  shiny::insertUI(
    selector = "#query_data_1-example_data_go", # Insert relative to the button
    where = "afterEnd", # Place it immediately after the button
    ui = shiny::tags$span(
      "Reload the TADAShiny app to load new data",
      style = "margin-left: 10px; color: red;"
    )
  )
  shiny::insertUI(
    selector = "#query_data_1-querynow", # Insert relative to the button
    where = "afterEnd", # Place it immediately after the button
    ui = shiny::tags$span(
      "Reload the TADAShiny app to query the Water Quality Portal",
      style = "margin-left: 10px; color: red;"
    )
  )
  shiny::insertUI(
    selector = "#file-upload-wrapper", # Use the wrapper div's id
    where = "afterEnd", # Place it immediately after the wrapper div
    ui = shiny::tags$span(
      "Reload the TADAShiny app to upload a new dataset",
      style = "margin-left: 10px; color: red;"
    )
  )
  shiny::insertUI(
    selector = "#progress-file-wrapper", # Insert relative to the button
    where = "afterEnd", # Place it immediately after the button
    ui = shiny::tags$span(
      "Reload the TADAShiny app to upload a new progress file",
      style = "margin-left: 10px; color: red;"
    )
  )
}

## To be copied in the UI
# mod_query_data_ui("query_data_1")

## To be copied in the server
# mod_query_data_server("query_data_1")
