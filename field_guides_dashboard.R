library(leaflet)
library(rgdal)
library(raster)
library(sp)
library(geojsonio)

# Set working directory
wd <- setwd("//Filer01/SE/Action/Code_Lunch/RShiny_Projects/Nic/Field_Guides_Leaflet/data")
            
# Import shape variables
countries <- readOGR(dsn = wd, "global_bounds")
plot(countries)

fm <- geojsonio::geojson_read("fm_guides_r.geojson", what = "sp")
plot(fm)

# Global color scheme
bins <- c(1, 5, 50, 250)
pal <- colorBin("Blues", domain = states$FG_Numb, bins = bins)

# Add labels
labels <- sprintf(
  "<strong>%s</strong><br/><strong>%g</strong> Field Guides<sup></sup><br>Link:<br/>",
  fm$admin, fm$FG_Numb
) %>% lapply(htmltools::HTML)

leaflet(options = leafletOptions(minZoom = 1, maxZoom = 5))

# Build the initial basemap
m <- leaflet(fm) %>%
  setView(7, 17, 1.5) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Dark Matter") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
  #  accessToken = Sys.getenv('sk.eyJ1IjoibmtvdGxpbnNraSIsImEiOiJjanRyYnN6NGYwb20zNDBwY3M0Nmtpa2Y4In0.fvRJSU66OfBBQd9aSsZIqQ')))
    accessToken = Sys.getenv('pk.eyJ1IjoibmtvdGxpbnNraSIsImEiOiJjanRyYnJteHUwb2JhNDRtdWR4bHRiem9uIn0.wtil2ENruTPGdwliR5p7bg')))

# Run the map
m %>% addPolygons(
  fillColor = ~pal(FG_Numb),
  weight = 1,
  opacity = 0.5,
  color = "black",
  #dashArray = "3",
  fillOpacity = 0.7,
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
    baseGroups = c("OSM", "Dark Matter", "Toner Lite"),
    overlayGroups = c("Field Guides"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(pal = pal, values = ~FG_Numb, opacity = 1, title = "Number of Field Guides",
                position = "bottomright")

