#' review_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_review_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::h3("Data Review"),
    htmltools::HTML("Use this tab to review flagging and filter decisions and explore the filtered dataset (still under development). Click the button below to begin."),
    shiny::fluidRow(column(4, shiny::actionButton(ns("review_go"),"Load Review Data",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
    htmltools::br(),
    shiny::fluidRow(column(8, shiny::plotOutput(ns("review_barchar"),height="500px"))),
    shiny::fluidRow(column(8, shiny::plotOutput(ns("reason_pie"),height="500px")))
  )
}
    
#' review_data Server Functions
#'
#' @noRd 
mod_review_data_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    review_things <- shiny::reactiveValues()
    
    shiny::observeEvent(input$review_go,{
      removals <- tadat$removals
      sel <- which(removals == TRUE, arr.ind = TRUE)
      removals[sel] <- names(removals)[sel[, "col"]]
      removals[removals == FALSE] = ""
      tadat$raw$TADA.RemovalReason = apply(removals, 1,
                                           function(row)
                                             paste(row[nzchar(row)], collapse = ", "))
      
      # data for bar chart - this is real rough
      step_rems = sort_removals(tadat$removals)
      total = dim(tadat$raw)[1]
      flag = ifelse(length(step_rems$Count[step_rems$Reason%in%"Flag only"])>0,step_rems$Count[step_rems$Reason%in%"Flag only"],0)
      filtflag = ifelse(length(step_rems$Count[step_rems$Reason%in%"Flag and Filter"])>0,step_rems$Count[step_rems$Reason%in%"Flag and Filter"],0)
      filter = ifelse(length(step_rems$Count[step_rems$Reason%in%"Filter only"])>0,step_rems$Count[step_rems$Reason%in%"Filter only"],0)
      mrfl = total - flag - filtflag
      mrfi = mrfl - filter
      
      step_rems_plot = data.frame(Step = c("Starting Total","Measurements Retained After Flagging","Measurements Retained After Filtering"), Count = c(total, mrfl, mrfi))
      step_rems_plot$Step = factor(step_rems_plot$Step, levels = c("Starting Total","Measurements Retained After Flagging","Measurements Retained After Filtering"))
      review_things$step_rems_plot = step_rems_plot
      
      # data for pie chart
      rem_reas = data.frame(Reason = names(tadat$removals), Count = apply(tadat$removals, 2, sum))
      rem_reas = subset(rem_reas, rem_reas$Count>0)
      review_things$rem_reas = rem_reas
    })
    
    # characteristics bar chart showing top characteristics by result number in dataset
    output$review_barchar = shiny::renderPlot({
      shiny::req(review_things$step_rems_plot)
      ggplot2::ggplot(review_things$step_rems_plot, ggplot2::aes(x=Step, y=Count)) +
        ggplot2::geom_bar(stat = "identity", fill = "#005ea2", color = "black") +
        ggplot2::labs(title="Results Retained Following Flagging/Filtering Steps",x="", y = "Results Count")+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::geom_text(ggplot2::aes(x = Step, y = Count+(0.07*max(Count)), label = Count), size = 5, color="black") #+
    })
    
    output$reason_pie = shiny::renderPlot({
      shiny::req(review_things$rem_reas)
      dat = review_things$rem_reas
      dat$Legend = paste0(dat$Reason, " - ", dat$Count, " results")
      
      # define number of colors required for pie chart
      colorCount <- length(unique(dat$Legend))
      
      # define color palette
      getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
      
      # create pie chart
        ggplot2::ggplot(dat, ggplot2::aes(x = "", y = Count, fill = Legend)) +
        ggplot2::scale_fill_manual(values = getPalette(colorCount), name = "Removal Reasons Pie Chart") +
        ggplot2::geom_bar(stat = "identity", width = 1) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::theme_void() +
        ggplot2::labs(caption = "Note that some results may be removed for multiple reasons, so the numbers in this plot add up to greater than or equal to the total results removed.")
    })
 
  })
}
    
## To be copied in the UI
# mod_review_data_ui("review_data_1")
    
## To be copied in the server
# mod_review_data_server("review_data_1")
