# Automatically loads required packages if the user doesn't have it already
list.of.packages <- c("shiny", "raster", "rgdal", "sp", "leaflet", "geojsonio", "markdown")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(raster)
library(rgdal)
library(sp)
library(leaflet)
library(geojsonio)
library(markdown)

######################
### LOAD VARIABLES ###
######################

#wd <- setwd("C:/Shiny/Field_Guides_Leaflet")

# Import shape variables
#countries <- readOGR("C:/Shiny/Field_Guides_Leaflet/data", "global_bounds")
fm <- geojsonio::geojson_read("data/fm_guides_r.geojson", what = "sp")

##########
### UI ###
##########

ui <- fluidPage(

      id = "main_content",
     
      titlePanel("Field Guides Dashboard"),
      
      tags$style(type = "text/css", "#map {height: calc(100vh - 20px) !important;}"),
      leafletOutput("map"),

      absolutePanel(
          top = "auto", left = 20, right = "auto", bottom = 200,
          width = 330, height = "auto", draggable = TRUE, fixed = TRUE,
          wellPanel(
            HTML(markdownToHTML(fragment.only=TRUE, text=c(
              "Search `Field Guides` by country"
            ))),
            selectInput(inputId = "variableselected", label = "Select country", 
                        choices = fm$admin),
            textOutput("selected_var")
            ),
            
          style = "opacity: 0.90"
      )
    )      
      
      #sidebarLayout(
      #  sidebarPanel(
          
      #      selectInput(inputId = "variableselected", label = "Select country", 
      #                  choices = fm$admin)),
        
        #mainPanel(
        #  leafletOutput("map", width = 1000, height = 600)
        #)
      #)

################
#### SERVER ####
################

server <- function(input, output, session) {
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$variableselected)
  })
  
  output$map <- renderLeaflet({
  
    # Global color scheme
    bins <- c(1, 5, 50, 250)
    pal <- colorBin("Blues", domain = states$FG_Numb, bins = bins)
    
    # Add labels
    labels <- sprintf(
      "<strong>%s</strong><br/><strong>%g</strong> Field Guides<sup></sup><br>Link:<br/>",
      fm$admin, fm$FG_Numb) %>% lapply(htmltools::HTML)
    
    m <- leaflet(fm, height = "100%", options = leafletOptions(minZoom = 1, maxZoom = 5)) %>%
      setView(7, 17, 1.5) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Black & White")
      #addProviderTiles("MapBox", options = providerTileOptions(
      #  id = "mapbox.light",
      #  accessToken = Sys.getenv('sk.eyJ1IjoibmtvdGxpbnNraSIsImEiOiJjanRyYnN6NGYwb20zNDBwY3M0Nmtpa2Y4In0.fvRJSU66OfBBQd9aSsZIqQ')))
      #  accessToken = Sys.getenv('pk.eyJ1IjoibmtvdGxpbnNraSIsImEiOiJjanRyYnJteHUwb2JhNDRtdWR4bHRiem9uIn0.wtil2ENruTPGdwliR5p7bg')))
    
    # Run the map
    m %>% addPolygons(
      fillColor = ~pal(FG_Numb),
      weight = 1,
      opacity = 1,
      color = "black",
      #dashArray = "3",
      fillOpacity = 1,
      group = "Field Guides",
      highlight = highlightOptions(
        weight = 2,
        color = "Yellow",
        #dashArray = "",
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
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(pal = pal, values = ~FG_Numb, title = "Number of Field Guides",
                position = "bottomleft")
  })

}

#########################
#### RUN APPLICATION ####
#########################

shinyApp(ui = ui, server = server)

