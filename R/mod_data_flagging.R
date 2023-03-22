library(shiny)
library(shinyWidgets)
library(DT)
print(getwd())
# Read in the tables
prompt_table = read.csv("inst\\flag_prompts.csv")
prompt_table <- prompt_table[order(prompt_table$Order), ]
prompts <- prompt_table$Prompt
n_switches <- length(prompts)
flag_types <- prompt_table$flagType

mod_data_flagging_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(style = "display: none;",
             prettySwitch("dummy", label = NULL)),
    DT::dataTableOutput(ns('x1')),
    verbatimTextOutput(ns('x2'))
  )
}

mod_data_flagging_server <- function(id, TADA_Profile) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    flagSwitch = function(len, id, value) {
      if (length(value) == 1)
        value <- rep(value, len)
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(prettySwitch(
          ns(paste0(id, i)),
          label = NULL,
          value = value[i],
          status = "primary",
          fill = TRUE
        ))
      }
      inputs
    }
    
    shinyValue = function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value))
          TRUE
        else
          value
      }))
    }
    
    loopData = reactive({
      df = data.frame(Prompt = prompts,
                      Remove = flagSwitch(n_switches, 'switch_', FALSE))
    })
    
    output$x1 = DT::renderDataTable(
      isolate(loopData()),
      escape = FALSE,
      selection = 'none',
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        ordering = FALSE,
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
        ))


    output$x2 = renderPrint({
      data.frame(Selected = flag_types[shinyValue('switch_', n_switches)])})
})}
