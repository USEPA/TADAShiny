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
    # Application title
    shiny::titlePanel("TADA Figures"),
    shiny::fluidRow(column(4, # column containing drop down and button to select grouping columns 
                    shiny::uiOutput(ns("groupingcols")), # uiOutput is used when the widget depends upon something the user does/reactivity in the app
                    shiny::uiOutput(ns("groupinggo")))),
    shiny::fluidRow(column(3, # column containing drop down menu for all grouping column combinations
                    shiny::uiOutput(ns("plotgroup")))),
    shiny::fluidRow(column(6,
                    plotly::plotlyOutput(ns("boxplot"))),
             column(6, 
                    plotly::plotlyOutput(ns("histogram"))))
 
  )
}
    
#' figures Server Functions
#'
#' @noRd 
mod_figures_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    react = shiny::reactiveValues() # create a reactive values object to hold vectors, dataframes, etc. that you want to use throughout the app.
    
    shiny::observeEvent(tadat$tab,{
      if(tadat$tab=="Figures"){
        react$names = names(tadat$raw)
      }
      
    })
    
    # shiny::observeEvent(input$godata,{ # this event observer carries out the code below whenever the godata button is pushed.
    #   if(input$choosedata=="Shepherdstown"){
    #     react$dat <- TADA::Data_NCTCShepherdstown_HUC12 # adding a data frame as an object in the react list
    #   }
    #   if(input$choosedata=="Tribal"){
    #     react$dat <- TADA::Data_6Tribes_5y
    #   }
    #   react$names = names(react$dat) # here, we are adding the dataframe's original column names for use in the column grouping widget
    # })
    # 
    output$groupingcols <- shiny::renderUI({ # this is the companion to the uiOutput call in the UI of the app.
      shiny::req(react$names) # only shows up when the react$names object is available, with the data's original column names
      shiny::selectInput(ns("groupingcols"), "Select grouping columns", choices = react$names, selected = "TADA.ComparableDataIdentifier", multiple = TRUE)
    })
    
    output$groupinggo <- shiny::renderUI({ # this is the companion to the uiOutput call in the UI of the app.
      shiny::req(react$names)
      shiny::actionButton(ns("groupinggo"),"Go") # button shows up when react$dat exists
    })
    
    shiny::observeEvent(input$groupinggo,{ # this event observer carries out its functions whenever the groupinggo button is pushed.
      # this line adds a new column to the dataset of concatenated values of all of the columns selected by the user in the drop down above.
      # print(input$groupingcols)
      react$dat = tadat$raw %>% tidyr::unite("group", input$groupingcols, sep = "_", remove = FALSE)
      # this line saves the react$dat to a dataframe called "dat" that is committed to the global environment when the app closes - NOT IDEAL, BUT HELPFUL WITH TESTING
      # dat <<- react$dat
      # react$done = "Y"
    })
    
    output$plotgroup <- shiny::renderUI({ # this companion to the uiOutput in the UI appears when react$done exists
      # req(react$done) 
      # this line gets all the unique concatenated group values from react$dat
      if("group"%in%names(react$dat)){
        choices <- unique(react$dat$group)
        shiny::selectInput(ns("plotgroup"),"Select group to plot",choices = choices) # the object choices, created above, is used as the vector of choices in this final select input
      }
      
    })
    
    shiny::observeEvent(input$plotgroup,{
      react$plotdata = subset(react$dat, react$dat$group==input$plotgroup)
      react$boxp = TADA::TADA_Boxplot(react$plotdata, id_cols = "group")
      react$hist = TADA::TADA_Histogram(react$plotdata, id_cols = "group")
    })
    
    output$boxplot <- plotly::renderPlotly({
      shiny::req(react$boxp)
      react$boxp
    })
    
    output$histogram <- plotly::renderPlotly({
      shiny::req(react$hist)
      react$hist
    })
    
 
  })
}
    
## To be copied in the UI
# mod_figures_ui("figures_1")
    
## To be copied in the server
# mod_figures_server("figures_1")
