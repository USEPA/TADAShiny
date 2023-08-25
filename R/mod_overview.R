#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'

mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    htmltools::h3("Data Overview"),
    htmltools::HTML("<B>Note:</B> This page shows maps and figures using the <B>original</B> dataset uploaded to this TADAShiny session. If you'd like to see updated figures after working in other tabs, please press the 'Refresh' button."),
    htmltools::div(style="margin-bottom:10px"),
    shiny::fluidRow(column(3, shiny::actionButton(ns("refresh_overview"),"Refresh",shiny::icon("arrows-rotate"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
    htmltools::hr(),
    shiny::fluidRow(column(12, shiny::wellPanel(shiny::htmlOutput(ns("overview_totals"))))),
    htmltools::HTML("<B>Your dataset, mapped:</B> Zoom in and click on sites of interest. A pop up will appear that shows the number of measurements, characteristics, and visits at each site."),
    htmltools::br(),
    shiny::fluidRow(column(12, shinycssloaders::withSpinner(leaflet::leafletOutput(ns("overview_map"), height = "500px")))),# "Larger point sizes represent more samples collected at a site; darker points represent more characteristics collected at a site. Click on a point to see the site ID, name, and sample/visit/parameter counts."
    htmltools::br(),
    shiny::fluidRow(column(6,shiny::plotOutput(ns("overview_hist"), height="500px")),#"This histogram shows sample collection frequency for all sites over the time period queried.",
             column(6, shiny::plotOutput(ns("overview_barchar"), height="600px"))),
    htmltools::h3("Organizations in dataset"),
    htmltools::HTML("The table below shows the organizations that collected data in your dataset and the number of measurements collected by each. Notice the third column, 'Rank'. This editable column is present because sometimes organizations unintentionally upload the same dataset multiple times to the WQP. For example, USGS will collect data at the request of state agencies. The USGS 'copy' of the results is uploaded to NWIS and made available in the portal, and the state agency's 'copy' of the results is uploaded to WQX. This rank provides the necessary info needed to flag and select one representative result from groups of duplicative uploads based on date, characteristic and result value/unit, and proximity to other sites. Double click in a cell in the 'Rank' column to edit the hierarchy of organizations and Ctrl-Enter to save those changes in the table: the default ranks organizations by the number of measurements in the dataset. Using the state vs USGS data example, if the state agency's organization name has a higher rank (ex. ranked #1) than USGS (ex. ranked #2), its result will be selected over the USGS upload of the sample, and the USGS version will be flagged for removal."),
    htmltools::div(style="margin-bottom:10px"),
    shiny::fluidRow(column(12, DT::DTOutput(ns("overview_orgtable"), height = "500px")))
    )

}

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id, tadat){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # this a reactive list created to hold all the reactive objects specific to this module.
    mapdat = shiny::reactiveValues()

    # create dataset for map and histogram using raw data
    shiny::observeEvent(tadat$ovgo, {
      req(tadat$raw)
      # create gray text tile info
      mapdat$text = tadat$raw%>%dplyr::filter(TADA.Remove==FALSE)%>%dplyr::select(ResultIdentifier,MonitoringLocationIdentifier,OrganizationFormalName,ActivityStartDate)
      # create summary info and binning for map
      orgs = tadat$raw%>%dplyr::filter(TADA.Remove==FALSE)%>%dplyr::group_by(OrganizationFormalName, OrganizationIdentifier)%>%dplyr::summarise("Result_Count" = length(unique(ResultIdentifier))) %>% dplyr::ungroup()
      mapdat$orgs = orgs %>% dplyr::arrange(-Result_Count) %>% dplyr::mutate("Rank" = 1:length(Result_Count))
      # get top 10 characteristics by result number in the dataset and place the rest in a group called "all others"
      chars = tadat$raw%>%dplyr::filter(TADA.Remove==FALSE)%>%dplyr::group_by(TADA.CharacteristicName)%>%dplyr::summarise("Result_Count" = length(unique(ResultIdentifier)))
      topslice = chars%>%dplyr::slice_max(order_by = Result_Count, n = 10)
      bottomslice = chars%>%dplyr::ungroup()%>%dplyr::filter(!TADA.CharacteristicName%in%topslice$TADA.CharacteristicName)%>%dplyr::select("Result_Count")%>%dplyr::summarise("Result_Count" = sum(Result_Count))%>%dplyr::mutate("TADA.CharacteristicName" = "ALL OTHERS")
      chars = plyr::rbind.fill(topslice, bottomslice)%>%dplyr::filter(Result_Count>0)
      chars = chars%>%dplyr::mutate(TADA.Chars = substr(TADA.CharacteristicName, 1,22))
      chars$TADA.Chars = ifelse(nchar(chars$TADA.CharacteristicName)>22,paste0(chars$TADA.Chars, "..."),chars$TADA.Chars)
      chars = chars%>%dplyr::mutate(TADA.Chars = forcats::fct_reorder(TADA.Chars, Result_Count, .desc=TRUE))
      mapdat$chars = chars
      tadat$ovgo = NULL
      if(is.null(tadat$orgs)){
        tadat$orgs = mapdat$orgs$OrganizationIdentifier
      }
      })
    
    # this widget produces the text at the top of the page describing record, site, and org numbers in dataset
    output$overview_totals = shiny::renderText({
      shiny::req(mapdat$text)
      paste0("Your dataset contains <B>",scales::comma(length(unique(mapdat$text$ResultIdentifier))),"</B> unique results from <B>",scales::comma(length(unique(mapdat$text$MonitoringLocationIdentifier))),"</B> monitoring location(s) and <B>", scales::comma(length(unique(mapdat$text$OrganizationFormalName))),"</B> unique organization(s).")
    })

    # the leaflet map
    output$overview_map = leaflet::renderLeaflet({
      shiny::req(mapdat$text)
      TADA::TADA_OverviewMap(tadat$raw[tadat$raw$TADA.Remove==FALSE,])
    })

    # histogram showing results collected over time.
    output$overview_hist = shiny::renderPlot({
      shiny::req(mapdat$text)
      ggplot2::ggplot(data = mapdat$text, ggplot2::aes(x = as.Date(ActivityStartDate, format = "%Y-%m-%d")))+ggplot2::geom_histogram(color = "black", fill = "#005ea2", binwidth = 7)+ggplot2::labs(title="Results collected per week over date range queried",x="Time", y = "Result Count")+ggplot2::theme_classic(base_size = 16)
    })
    
    # organization numbers table, the editable part allows user to change only the third column (rankings)
    # https://yihui.shinyapps.io/DT-edit/
    output$overview_orgtable = DT::renderDT(
      mapdat$orgs[,!names(mapdat$orgs)%in%c("OrganizationIdentifier")],
      editable = list(target = "column", disable = list(columns = c(0, 1))),
      colnames = c("Organization Name","Results Count","Rank - Double Click to Edit, Ctrl-Enter to Save"),
      options = list(pageLength=length(unique(mapdat$orgs$OrganizationFormalName)), searching = FALSE, scrollY = TRUE),
      rownames= FALSE,
      selection = 'none'
    )
    
    # observe({
    #   print("What did the two raindrops say to the third one? Two is company, but three is a cloud.")
    #   print(input$overview_orgtable_cell_edit)
    # })
    
    shiny::observeEvent(input$overview_orgtable_cell_edit, {
      org_rank = data.frame(OrganizationIdentifier = mapdat$orgs$OrganizationIdentifier, Rank = as.numeric(input$overview_orgtable_cell_edit$value)) %>% dplyr::arrange(Rank)
      mapdat$orgs = mapdat$orgs %>% dplyr::select(-Rank) %>% dplyr::left_join(org_rank) %>% dplyr::arrange(Rank)
      # mapdat$orgs = orgs %>% dplyr::arrange(-Result_Count) %>% dplyr::mutate("Rank" = 1:length(Result_Count))
      tadat$orgs = org_rank$OrganizationIdentifier
    })
    
    # characteristics bar chart showing top characteristics by result number in dataset
    output$overview_barchar = shiny::renderPlot({
      shiny::req(mapdat$chars)
      ggplot2::ggplot(mapdat$chars, ggplot2::aes(x=TADA.Chars, y=Result_Count)) +
        ggplot2::geom_bar(stat = "identity", fill = "#005ea2", color = "black") +
        ggplot2::labs(title="Results per Characteristic",x="", y = "Results Count")+
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::geom_text(ggplot2::aes(x = TADA.Chars, y = Result_Count+(0.07*max(Result_Count)), label = Result_Count), size = 5, color="black") #+
   
          })
    
    shiny::observeEvent(input$refresh_overview,{
      shiny::req(mapdat$text)
      tadat$ovgo = TRUE
    })

  })
}

## To be copied in the UI
# mod_overview_ui("overview_1")

## To be copied in the server
# mod_overview_server("overview_1")
