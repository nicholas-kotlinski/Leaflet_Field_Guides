list.of.packages <- c("shiny", "shinydashboard", "raster", "rgdal", "sp", "leaflet", "geojsonio", "markdown", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(raster)
library(rgdal)
library(sp)
library(leaflet)
library(geojsonio)
library(markdown)
library(ggplot2)
library(dplyr)

fm <- geojsonio::geojson_read("data/fm_guides_r.geojson", what = "sp")
fg.data <- read.csv("data/FG_Category_Counry_cleaned_3.12.2019.csv")

ui <- dashboardPage(
  
  dashboardHeader(title = "Field Guides Dashboard"),
  
  dashboardSidebar(title = "Controls",
                   selectInput(inputId = "variableselected", label = "Select country", 
                               choices = fm$admin),
                   textOutput("selected_var"),
                   plotOutput("PieChart")),
                   #selectInput("cInput", "Select an Inventory", choices = unique(data$country)),
                   #selectInput(inputId = "YInput", label = "Select Field Guides",
                   #            choices = data$guide_no),
                   
                   #plotOutput("PieChart"),
  
  dashboardBody(
    fluidPage(
      tags$style(type = "text/css", "#map {height: calc(100vh - 20px) !important;}"),
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
    
  output$selected_var <- renderText({ 
    paste("You have selected", input$variableselected)
  })
  
  output$map <- renderLeaflet({

    bins <- c(1, 5, 50, 250)
    pal <- colorBin("Blues", domain = states$FG_Numb, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/><strong>%g</strong> Field Guides<sup></sup><br>Link:<br/>",
      fm$admin, fm$FG_Numb) %>% lapply(htmltools::HTML)
    
    m <- leaflet(fm, height = "100%", options = leafletOptions(minZoom = 1, maxZoom = 5)) %>%
      setView(7, 17, 1.5) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Black & White")

    m %>% addPolygons(
      fillColor = ~pal(FG_Numb),
      weight = 1,
      opacity = 1,
      color = "black",
      fillOpacity = 1,
      group = "Field Guides",
      
      highlight = highlightOptions(
        weight = 2,
        color = "Yellow",
        fillOpacity = 1,
        bringToFront = TRUE),
      
      label = labels,
      
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 6px"),
        textsize = "12px",
        direction = "auto")) %>% 
      
      addLayersControl(
        baseGroups = c("OSM", "Black & White"),
        overlayGroups = c("Field Guides"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      addLegend(pal = pal, values = ~FG_Numb, title = "Number of Field Guides",
        position = "bottomleft")
  })
  output$PieChart<-renderPlot({
    fg.data %>%subset(Countries == input$variableselected) %>%
      select(Countries, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))%>%
      ggplot(aes(x="", y=Proportion, fill=Category))+ geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
      scale_fill_manual(values = c("Plants" ="seagreen2", "Birds" = "mediumpurple3", "Fishes"="deepskyblue3", 
                                   "Herp" = "cyan3", "Insects" = "goldenrod2","Mammals" = "darkorange2",
                                   "Fungi"= "lightgoldenrod4", "Other" = "palevioletred2")) +
      theme_bw() + theme(legend.text=element_text(size=rel(1.2))) +theme(legend.title=element_text(size=15))
  }) 
  
}

shinyApp(ui = ui, server = server)