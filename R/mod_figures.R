#' figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_figures_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::h3("1. Determine Characteristic Groups of Interest"),
    htmltools::HTML("First, pick the columns you'd like to use to group your data. You can stick with 'TADA.ComparableDataIdentifier' (where each group of data represents one unique characteristic-unit-fraction-speciation combination), or you can add other columns that will help you divide and review your data most effectively. For example, you may also want to divide up your data based on monitoring location type, or hydrologic condition. Once you've decided on your grouping columns, click 'Generate Groups' below to group data. It is <B>optional</B> to add more grouping columns."),
    htmltools::br(),
    htmltools::br(),
    shiny::fluidRow(
      column(
        10, # column containing drop down and button to select grouping columns
        shiny::uiOutput(ns("groupingcols"))
      ),
      column(
        2, # uiOutput is used when the widget depends upon something the user does/reactivity in the app
        shiny::uiOutput(ns("groupinggo"))
      )
    ),
    # this part contains the grouping field and button that show up after the groups have been established
    shiny::uiOutput(ns("mapplotgroup")),
    # map
    shiny::fluidRow(column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("sites_map"), height = "500px")))),
    htmltools::br(),
    htmltools::br(),
    # site selection field and button
    shiny::uiOutput(ns("selsites")),
    htmltools::br(),
    # tabs with plots in them
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Single Characteristic Figures",
        htmltools::br(),
        htmltools::HTML("Benchmark values appear as horizontal lines on the single-characteristic scatterplot figure, below. Benchmarks may reflect established or proposed water quality criteria, important physical/chemical/biological thresholds, or any other values relevant to your use case."),
        htmltools::br(),
        shiny::uiOutput(ns("benchmarks")),
        htmltools::br(),
        shiny::uiOutput(ns("benchmarktext")),
        htmltools::br(),
        shiny::fluidRow(plotly::plotlyOutput(ns("scatter"))),
        htmltools::br(),
        shiny::fluidRow(plotly::plotlyOutput(ns("histogram"))),
        htmltools::br(),
        shiny::fluidRow(plotly::plotlyOutput(ns("boxplot")))
      ),
      shiny::tabPanel(
        "Two-Characteristic Scatterplot",
        shiny::fluidRow(plotly::plotlyOutput(ns("scatter2")))
      )
    )
  )
}

#' figures Server Functions
#'
#' @noRd
mod_figures_server <- function(id, tadat) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    react <- shiny::reactiveValues() # create a reactive values object to hold vectors, dataframes, etc. that you want to use throughout the app.

    # This grabs the column names of tadat$raw when user first clicks on tadat$tab, so grouping col dropdown doesn't have the 'group' column created in this tab.
    shiny::observeEvent(tadat$tab, {
      if (tadat$tab == "Figures") {
        react$names <- names(tadat$raw)
      }
    })

    # grouping columns widget that depends upon names reactive object
    output$groupingcols <- shiny::renderUI({ # this is the companion to the uiOutput call in the UI of the app.
      shiny::req(react$names) # only shows up when the react$names object is available, with the data's original column names
      shiny::selectInput(ns("groupingcols"), "Select grouping columns", choices = react$names, selected = "TADA.ComparableDataIdentifier", multiple = TRUE, width = "100%")
    })

    # grouping columns GO button
    output$groupinggo <- shiny::renderUI({ # this is the companion to the uiOutput call in the UI of the app.
      shiny::req(react$names)
      shiny::actionButton(ns("groupinggo"), "Generate Groups", shiny::icon("wand-sparkles"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top:30px"
      ) # button shows up when react$dat exists
    })

    # event observer that watches grouping columns GO button and carries out unite function to get unique groups from grouping columns
    shiny::observeEvent(input$groupinggo, { # this event observer carries out its functions whenever the groupinggo button is pushed.
      # this line adds a new column to the dataset of concatenated values of all of the columns selected by the user in the drop down above.
      depthcols <- names(tadat$raw)[grepl("DepthHeightMeasure", names(tadat$raw))]
      depthcols <- depthcols[grepl("TADA.", depthcols)]
      # This must include all columns needed for plots, include those only needed for the hover features
      selcols <- c("TADA.ComparableDataIdentifier",
                   "OrganizationFormalName",
                   "ResultIdentifier",
                   "groupname",
                   "MonitoringLocationIdentifier",
                   "MonitoringLocationName",
                   "MonitoringLocationTypeName", 
                   "TADA.LatitudeMeasure", 
                   "TADA.LongitudeMeasure",
                   "TADA.ResultMeasureValue", 
                   "TADA.ResultMeasure.MeasureUnitCode",
                   "ActivityRelativeDepthName", 
                   "ActivityStartDate",
                   "ActivityStartDateTime", 
                   "TADA.ActivityMediaName", 
                   "ActivityMediaSubdivisionName", 
                   "TADA.ResultSampleFractionText",
                   "TADA.MethodSpecificationName",
                   "TADA.CharacteristicName",
                   depthcols)

      react$dat <- tadat$raw %>%
        dplyr::filter(TADA.Remove == FALSE, !is.na(TADA.ResultMeasureValue)) %>%
        tidyr::unite("group", input$groupingcols, sep = "_", remove = FALSE) %>%
        dplyr::mutate(groupname = gsub("_NA", "", group)) %>%
        dplyr::mutate(groupname = gsub("_", " ", groupname)) %>%
        dplyr::select(selcols)
    })

    # select 1-2 unique groups drop down menu AND button to display on map and plots
    output$mapplotgroup <- shiny::renderUI({ # this companion to the uiOutput in the UI appears when react$done exists
      # req(react$done)
      # this line gets all the unique concatenated group values from react$dat
      if ("groupname" %in% names(react$dat)) {
        choices <- unique(react$dat$groupname)
        shiny::fluidRow(
          htmltools::h3("2. Pick Groups to Map and Plot"),
          htmltools::HTML("Use the drop down to pick the characteristic group(s) (max of 2) you'd like to map/plot and then click 'Generate Map'. If you have <B>one</B> group selected, the map will display site markers with radii that reflect the mean result measure value at each site: larger site markers correspond to higher mean result measure values. If you have <B>two</B> results selected, the map will display which sites have data for one or both of the selected groups."),
          htmltools::br(),
          htmltools::br(), # the object choices, created above, is used as the vector of choices in this final select input
          column(
            6, # column containing drop down menu for all grouping column combinations
            shiny::selectizeInput(ns("mapplotgroup"), "Select up to TWO groups", choices = NULL, multiple = TRUE, options = list(maxItems = 2), width = "100%")
          ),
          column(
            1,
            shiny::actionButton(ns("mapplotgroupgo"), "Generate Map", shiny::icon("wand-sparkles"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top:30px"
            )
          )
        ) # the object choices, created above, is used as the vector of choices in this final select input
      }
    })

    shiny::observe({
      shiny::req(react$dat)
      shiny::updateSelectizeInput(session, "mapplotgroup", choices = unique(react$dat$groupname), selected = unique(react$dat$groupname)[1], server = TRUE)
    })

    # event observer that creates all reactive objects needed for map and plots following button push
    shiny::observeEvent(input$mapplotgroupgo, {
      react$groups <- input$mapplotgroup
      groupdata <- subset(react$dat, react$dat$groupname %in% c(react$groups))
      react$plotdataset <- groupdata
      react$mapdata <- groupdata %>%
        dplyr::group_by(OrganizationFormalName, MonitoringLocationIdentifier, MonitoringLocationName, MonitoringLocationTypeName, TADA.LatitudeMeasure, TADA.LongitudeMeasure) %>%
        dplyr::summarise(Ncount = length(ResultIdentifier), MeanV = mean(TADA.ResultMeasureValue), GroupID = paste0(unique(sort(groupname)), collapse = ";"), DateRange = paste0(min(lubridate::year(as.Date(ActivityStartDate, "%Y-%m-%d"))), " - ", max(lubridate::year(as.Date(ActivityStartDate, "%Y-%m-%d")))))
    })

    # taken from this stackoverflow: https://stackoverflow.com/questions/58505589/circles-in-legend-for-leaflet-map-with-addcirclemarkers-in-r-without-shiny
    addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, title = NULL) {
      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")

      return(leaflet::addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity, title = title))
    }

    # leaflet map, which is dependent upon above event observer's output reactive object (react$mapdata)
    output$sites_map <- leaflet::renderLeaflet({
      shiny::req(react$mapdata)

      # prep for the radius of the points to correspond to the number of measurements and the legend to match
      react$mapdata$radius <- scales::rescale(react$mapdata$Ncount, c(5, 35))
      leg_labs <- c(signif(quantile(react$mapdata$Ncount, 0.10), 3), signif(median(react$mapdata$Ncount), 3), signif(quantile(react$mapdata$Ncount, 0.90), 3))
      leg_sizes <- c(quantile(react$mapdata$radius, 0.10), median(react$mapdata$radius), quantile(react$mapdata$radius, 0.90)) * 2

      # base map
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "World topo", options = leaflet::providerTileOptions(updateWhenZooming = FALSE, updateWhenIdle = TRUE)) %>%
        leaflet::clearMarkers() %>% # get rid of whatever was there before if loading a second dataset
        leaflet::fitBounds(lng1 = min(react$mapdata$TADA.LongitudeMeasure), lat1 = min(react$mapdata$TADA.LatitudeMeasure), lng2 = max(react$mapdata$TADA.LongitudeMeasure), lat2 = max(react$mapdata$TADA.LatitudeMeasure)) # fit to bounds of data in tadat$raw

      # if there's one group, show a map with markers whose color saturation corresponds to mean values at each site: darker = higher
      if (length(react$groups) < 2) {
        pal <- leaflet::colorNumeric(
          palette = "Blues",
          domain = react$mapdata$MeanV
        )
        map %>%
          leaflet::addCircleMarkers(
            data = react$mapdata, lng = ~TADA.LongitudeMeasure, lat = ~TADA.LatitudeMeasure, color = "black", fillColor = ~ pal(react$mapdata$MeanV), fillOpacity = 0.7, stroke = TRUE, weight = 1.5,
            radius = ~radius,
            popup = paste0(
              "Organization: ", react$mapdata$OrganizationFormalName,
              "<br> Site Name: ", react$mapdata$MonitoringLocationName,
              "<br> Site ID: ", react$mapdata$MonitoringLocationIdentifier,
              "<br> Site Type: ", react$mapdata$MonitoringLocationTypeName,
              "<br> Group: ", react$mapdata$GroupID,
              "<br> Date Range: ", react$mapdata$DateRange,
              "<br> Mean Measurement Value: ", react$mapdata$MeanV,
              "<br> Measurement Count: ", react$mapdata$Ncount
            )
          ) %>%
          leaflet::addLegend("bottomright",
            pal = pal, values = react$mapdata$MeanV,
            title = "Mean Result Value",
            opacity = 1
          ) %>%
          addLegendCustom(colors = "black", labels = leg_labs, sizes = leg_sizes, title = "Measurement Count")
      } else { # if there's two groups, show which sites have which groups, plus the ones that have BOTH groups, shown by different colors
        pal <- leaflet::colorFactor("Set2", unique(react$mapdata$GroupID))
        map %>%
          leaflet::addCircleMarkers(
            data = react$mapdata, lng = ~TADA.LongitudeMeasure, lat = ~TADA.LatitudeMeasure, color = "black", fillColor = ~ pal(GroupID), fillOpacity = 0.7, stroke = TRUE, weight = 1.5,
            radius = ~radius,
            popup = paste0(
              "Organization: ", react$mapdata$OrganizationFormalName,
              "<br> Site Name: ", react$mapdata$MonitoringLocationName,
              "<br> Site ID: ", react$mapdata$MonitoringLocationIdentifier,
              "<br> Site Type: ", react$mapdata$MonitoringLocationTypeName,
              "<br> Group(s): ", react$mapdata$GroupID,
              "<br> Date Range: ", react$mapdata$DateRange,
              "<br> Measurement Count: ", react$mapdata$Ncount
            )
          ) %>%
          leaflet::addLegend("bottomright",
            pal = pal, values = react$mapdata$GroupID,
            title = "Data Groups",
            opacity = 1
          ) %>%
          addLegendCustom(colors = "black", labels = leg_labs, sizes = leg_sizes, title = "Measurement Count")
      }
    })

    # select sites whose data to display in plots
    output$selsites <- shiny::renderUI({ # this companion to the uiOutput in the UI appears when react$done exists
      shiny::req(react$mapdata)
      sites <- c("All sites", unique(react$mapdata$MonitoringLocationIdentifier))
      shiny::fluidRow(
        htmltools::h3("3. Select Specific Sites (Optional)"),
        htmltools::HTML(paste0("Use the drop down to pick the sites you'd like to include in the plots below and then click 'Generate Plots'. Defaults to all sites in the dataset. <B>NOTE:</B> Currently, the single-characteristic scatterplot, histogram, and boxplot show the first characteristic from the drop down above the map: <B>", react$groups[1], "</B>.")),
        htmltools::br(),
        column(
          6, # column containing drop down menu for all grouping column combinations
          shiny::selectizeInput(ns("selsites1"), "Select sites", choices = sites, selected = sites[1], multiple = TRUE, width = "100%")
        ),
        column(
          1,
          shiny::actionButton(ns("selsitesgo"), "Generate Plots", shiny::icon("wand-sparkles"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top:30px"
          )
        )
      )
    })

    # when the Go button is pushed to generate plots, this ensures the plot data is filtered to the selected sites (or all sites)
    shiny::observeEvent(input$selsitesgo, {
      if (all(input$selsites1 == "All sites")) {
        react$plotdata <- react$plotdataset
      } else {
        plotdata <- react$plotdataset %>% dplyr::filter(MonitoringLocationIdentifier %in% input$selsites1)
        if (!react$groups[1] %in% plotdata$groupname) {
          shiny::showModal(shiny::modalDialog(
            title = "Whoops!",
            paste0("You selected a site/sites where ", react$groups[1], " was not sampled. Please use the legend in the map above to select site(s) where ", react$groups[1], "was sampled.")
          ))
        } else {
          react$plotdata <- plotdata
        }
      }
    })

    # benchmark widgets
    output$benchmarks <- shiny::renderUI({
      shiny::req(react$plotdata)
      shiny::fluidRow(
        column(3, shiny::numericInput(ns("benchmark1"), "Benchmark 1", value = 0)),
        column(3, shiny::numericInput(ns("benchmark2"), "Benchmark 2", value = 0))
      )
    })

    output$benchmarktext <- shiny::renderUI({
      shiny::req(input$benchmark1)
      vals <- subset(react$plotdata, react$plotdata$groupname == react$groups[1])$TADA.ResultMeasureValue
      exc1 <- length(vals[vals > input$benchmark1])
      exc2 <- length(vals[vals > input$benchmark2])
      tot <- length(vals)
      shiny::wellPanel(htmltools::strong(paste0(exc1, " out of ", tot, " measurements (", round(exc1 / tot * 100, digits = 1), "%) exceed benchmark 1, while ", exc2, " out of ", tot, " measurements (", round(exc2 / tot * 100, digits = 1), "%) exceed benchmark 2.")))
    })

    # for plotting benchmarks
    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color)
      )
    }

    # plotly scatter plot
    output$scatter <- plotly::renderPlotly({
      shiny::req(react$plotdata)
      suppressWarnings(TADA::TADA_Scatterplot(subset(react$plotdata, react$plotdata$groupname == react$groups[1]), id_cols = "groupname")) %>%
        plotly::layout(shapes = list(
          hline(y = input$benchmark1, color = "red"),
          hline(y = input$benchmark2, color = "orange")
        ))
    })

    # plotly boxplot
    output$boxplot <- plotly::renderPlotly({
      shiny::req(react$plotdata)
      suppressWarnings(TADA::TADA_Boxplot(subset(react$plotdata, react$plotdata$groupname == react$groups[1]), id_cols = "groupname"))
    })

    # plotly histogram
    output$histogram <- plotly::renderPlotly({
      shiny::req(react$plotdata)
      suppressWarnings(TADA::TADA_Histogram(subset(react$plotdata, react$plotdata$groupname == react$groups[1]), id_cols = "groupname"))
    })

    # dynamically show/hide two-char scatter
    shiny::observe({
      shiny::req(react$plotdata)
      if (length(react$groups) > 1) {
        shinyjs::enable(selector = '.nav li a[data-value="Two-Characteristic Scatterplot"]')
      } else {
        shinyjs::disable(selector = '.nav li a[data-value="Two-Characteristic Scatterplot"]')
      }
    })

    # plotly two-characteristic scatter plot
    output$scatter2 <- plotly::renderPlotly({
      shiny::req(react$plotdata)
      if (length(unique(react$plotdata$groupname)) > 1) {
      suppressWarnings(TADA::TADA_TwoCharacteristicScatterplot
                       (react$plotdata, 
                        id_cols = "groupname", 
                        groups = unique(react$plotdata$groupname)))
      }
    })
  })
}

## To be copied in the UI
# mod_figures_ui("figures_1")

## To be copied in the server
# mod_figures_server("figures_1")
