list.of.packages <- c("shiny", "shinydashboard", "raster", "rgdal", "sp", "leaflet", "leaflet.extras", "geojsonio", "shinythemes","markdown", "ggplot2", "DT", "shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(raster)
library(rgdal)
library(sp)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(markdown)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyWidgets)

fm <- geojsonio::geojson_read("data/global_guides_r.geojson", what = "sp")
fg.data <- read.csv("data/FG_Category_Counry_cleaned_3.12.2019.csv")

ui <- dashboardPage(#skin = "black",
                    
  dashboardHeader(titleWidth = 400, title = "Field Museum Field Guides"),
                    
  dashboardSidebar(
    sidebarMenu(style = "position: fixed; overflow: visible;"),
    width = 400,
    br(),
    helpText("Click a desired country on the map for more information, or choose a country from the dropdown menu below", align = "center"),
    
    selectInput(inputId = "variableselected", label = "Select country", choices = fm$admin),
                      
    plotOutput("PieChart"),
                      
    h2(align = "center",
      img(src = "logo.png", height = 70, width = 70),
      img(src = "shiny.png", height = 70, width = 120))
  ),
                    
  dashboardBody(
    
    tags$head(tags$style(HTML('
                                
                              /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #DCDCDC;
                                color: #000000
                                }

                              /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #FFFFFF;
                                color: #000000;
                                }
                                
                            /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0000FF;
                                }
                                
                              /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #FFFAFA;
                                }
                                
                              /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #FFFFFF;
                                }
                              
                              /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFAFA;
                                }
                              
                              '))),
    fluidPage(
      tags$style(type = "text/css", "#map {height: calc(100vh - 420px) !important;}"),
      leafletOutput("map"),
      DT::dataTableOutput("link_list")
    )
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    bins <- c(1, 5, 100, 300)
    #pal <- colorBin("Blues", domain = fm$guides_no, bins = bins) #YlGrn
    pal <- colorBin(c("lightskyblue1","lightskyblue3", "dodgerblue3"), domain = fm$guides_no, bins = bins) #YlGrn
    
    #labels <- sprintf(
    #  "<strong>%s</strong>", fm$admin) %>% lapply(htmltools::HTML)
    #"<strong>%s</strong><br/>No. of Guides: <strong>%g</strong><sup></sup><br>Link:<strong><a href = %f></a>GO</strong></br>",
    #     fm$admin, fm$guides_no, fm$guide_link) %>% lapply(htmltools::HTML)
    
    m <- leaflet(fm, options = leafletOptions(minZoom = 3, maxZoom = 4)) %>%
      setView(7, 17, 2.5) %>%
      addTiles(group = "OSM") %>%
      #addProviderTiles(providers$Stamen.TonerBackground, group = "Black & White") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topography") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")
    
    m %>% addPolygons(
      fillColor = ~pal(guides_no),
      weight = 1,
      opacity = 0.8,
      color = "black",
      fillOpacity = 0.8,
      group = "Field Guides",
      
      popup = paste("<h4>", "<strong>", fm$admin, "</strong> <br> No. of Guides: <strong>", fm$guides_no, 
                    "</strong> <br> Link: <a href = ", fm$guide_link, "> Go to guides </a>"),
      
      highlight = highlightOptions(
        weight = 2.5,
        color = "Yellow",
        fillOpacity = 1,
        bringToFront = TRUE)) %>%
      
      #label = labels,
      
      #labelOptions = labelOptions(
      #  style = list("font-weight" = "normal", padding = "3px 6px"),
      #  textsize = "12px",
      #  direction = "auto")) %>% 
      
      addLayersControl(
        baseGroups = c("OSM", "Topography", "Satellite"),
        overlayGroups = c("Field Guides"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      addLegend(pal = pal, values = ~guides_no, title = "Number of Field Guides",
                position = "bottomright", opacity = 0.9) %>%
      
      addResetMapButton() %>%
      
      addSearchFeatures(
        targetGroups = c("Field Guides"),
        options = searchFeaturesOptions(
          zoom=18, openPopup = FALSE, firstTipSubmit = FALSE,
          autoCollapse = TRUE, hideMarkerOnCollapse = FALSE ))
  })
  
  output$PieChart<-renderPlot({
    fg.data %>%subset(Countries == input$variableselected) %>%
      select(Countries, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))%>%
      ggplot(aes(x="", y=Proportion, fill=Category))+ geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
      #scale_fill_manual(values = c("Plants" ="seagreen2", "Birds" = "steelblue4", "Fishes"="steelblue", 
      #                             "Herp" = "powerderblue", "Insects" = "darkslategray3","Mammals" = "cornsilk4",
      #                             "Fungi"= "steelblue1", "Other" = "palevioletred2")) +
      scale_fill_manual(values = c("Plants" ="cornsilk4", "Birds" = "steelblue4", "Fishes"= "steelblue", 
                                   "Herp" = "powderblue", "Insects" = "darkslategray3","Mammals" = "dodgerblue",
                                   "Fungi"= "steelblue1", "Other" = "cornsilk2")) +
      theme_bw() + theme(legend.text=element_text( colour = "black", size=rel(1.2)))
  })
  
  output$link_list <- DT::renderDataTable({
    fg.data %>%subset(Countries == input$variableselected) %>%
      dplyr::select(-category,-country,-state, -language, -date_created) %>%
      dplyr::mutate(URL = paste0("https://fieldguides.fieldmuseum.org/guides/guide/", guide_no)) %>%
      dplyr::mutate(title = paste0("<a href='", URL, "'>",guide_title,"</a>")) %>% 
      dplyr::select(title, guide_no, page_no,Category) %>% 
      dplyr :: rename(Title = title, Guide_No = guide_no,Page_Number = page_no)
  }, escape = FALSE)
  
}

shinyApp(ui = ui, server = server)