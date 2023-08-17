#' figures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_figures_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::h3("Select Data Subset"),
    htmltools::HTML("Click 'Go' below to group data by the default comparable data identifier (characteristic name-fraction-speciation-unit). It is <B>optional</B> to add more grouping columns."),
    htmltools::br(),
    shiny::fluidRow(column(11, # column containing drop down and button to select grouping columns 
                           shiny::uiOutput(ns("groupingcols"))),
                    column(1, # uiOutput is used when the widget depends upon something the user does/reactivity in the app
                           shiny::uiOutput(ns("groupinggo")))),
    shiny::uiOutput(ns("plotgroup")),
    # shiny::fluidRow(column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("sites_map"), height = "500px")))),
    shiny::fluidRow(column(6,
                    shiny::fluidRow(plotly::plotlyOutput(ns("boxplot"))),
                    shiny::fluidRow(plotly::plotlyOutput(ns("histogram"))),
                    shiny::fluidRow(plotly::plotlyOutput(ns("scatter")))))
 
  )
}
    
#' figures Server Functions
#'
#' @noRd 
mod_figures_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    react = shiny::reactiveValues() # create a reactive values object to hold vectors, dataframes, etc. that you want to use throughout the app.
    
    # output$cdi_drop <- shiny::renderUI({
    #   shiny::req(tadat$raw)
    #   choices = subset(tadat$raw, tadat$raw$TADA.Remove==FALSE)
    #   shiny::updateSelectizeInput(ns("cdi_drop"), "Select comparable data group(s)", choices = c("All groups", choices), multiple = TRUE, server = TRUE)
    # })

    shiny::observeEvent(tadat$tab,{
      if(tadat$tab=="Figures"){
        react$names = names(tadat$raw)
      }
    })
    
    output$groupingcols <- shiny::renderUI({ # this is the companion to the uiOutput call in the UI of the app.
      shiny::req(react$names) # only shows up when the react$names object is available, with the data's original column names
      shiny::selectInput(ns("groupingcols"), "Select grouping columns", choices = react$names, selected = "TADA.ComparableDataIdentifier", multiple = TRUE, width = "100%")
    })
    
    output$groupinggo <- shiny::renderUI({ # this is the companion to the uiOutput call in the UI of the app.
      shiny::req(react$names)
      shiny::actionButton(ns("groupinggo"),"Go", shiny::icon("wand-sparkles"),
                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-top:30px") # button shows up when react$dat exists
    })
    
    shiny::observeEvent(input$groupinggo,{ # this event observer carries out its functions whenever the groupinggo button is pushed.
      # this line adds a new column to the dataset of concatenated values of all of the columns selected by the user in the drop down above.
      react$dat = tadat$raw %>% dplyr::filter(TADA.Remove == FALSE) %>% tidyr::unite("group", input$groupingcols, sep = "_", remove = FALSE)
    })
    
    output$plotgroup <- shiny::renderUI({ # this companion to the uiOutput in the UI appears when react$done exists
      # req(react$done) 
      # this line gets all the unique concatenated group values from react$dat
      if("group"%in%names(react$dat)){
        choices <- unique(react$dat$group)
        shiny::fluidRow(htmltools::HTML("Use the drop down to pick the characteristic group(s) (max of 2) you'd like to map/plot. Please note that selecting more than one comparable data identifier group will reflect on the map as ONLY monitoring locations with at least one measurement for ALL comparable data groups selected."),
                        htmltools::br(),
                        column(3, # column containing drop down menu for all grouping column combinations
                               shiny::selectizeInput(ns("plotgroup"),"Select group",choices = choices, multiple = TRUE, options = list(maxItems = 2)))) # the object choices, created above, is used as the vector of choices in this final select input
      }
      
    })
    
    shiny::observeEvent(input$plotgroup,{
      react$plotdata = subset(react$dat, react$dat$group==input$plotgroup[1])
      react$boxp = TADA::TADA_Boxplot(react$plotdata, id_cols = "group")
      react$hist = TADA::TADA_Histogram(react$plotdata, id_cols = "group")
      react$scatter = TADA::TADA_Scatterplot(react$plotdata, id_cols = "group")
    })
    
    output$boxplot <- plotly::renderPlotly({
      shiny::req(react$boxp)
      react$boxp
    })
    
    output$histogram <- plotly::renderPlotly({
      shiny::req(react$hist)
      react$hist
    })
    
    output$scatter <- plotly::renderPlotly({
      shiny::req(react$scatter)
      react$scatter
    })
    
 
  })
}
    
## To be copied in the UI
# mod_figures_ui("figures_1")
    
## To be copied in the server
# mod_figures_server("figures_1")
