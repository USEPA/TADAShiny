library(shiny)
library(shinybusy)

ui <- fluidPage(actionButton("start", "Start Task & Show Timer"))

server <- function(input, output, session) {
  # Reactive values to hold start time and elapsed time
  timer_data <- reactiveValues(start = NULL, elapsed = 0)

  # Timer that triggers every 1 second (1000 milliseconds)
  autoInvalidate <- reactiveTimer(1000)

  observeEvent(input$start, {
    # Record the start time
    timer_data$start <- Sys.time()
    timer_data$elapsed <- 0

    # Show the modal dialog
    # showModal(modalDialog(
    #   title = "Processing Time", h3(textOutput("clock")), footer = modalButton("Close"), easyClose = FALSE
    # ))
    shinybusy::show_modal_spinner(
      spin = "double-bounce",
      color = "#0071bc",
      text = HTML(paste(
        'Querying Data Source<br>NWIS (USGS)<br>',
        textOutput("clock")
      )),
      session = shiny::getDefaultReactiveDomain()
    )
  })

  # Update the elapsed time
  observe({
    autoInvalidate()
    if (!is.null(timer_data$start)) {
      timer_data$elapsed <- round(difftime(
        Sys.time(),
        timer_data$start,
        units = "secs"
      ))
    }
  })

  # Output the formatted time
  output$clock <- renderText({
    paste0("Elapsed time: ", timer_data$elapsed, " seconds")
  })
}

shinyApp(ui, server)
