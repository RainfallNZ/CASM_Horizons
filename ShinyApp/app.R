library(shiny)
library(leaflet)
library(htmltools)

#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()
reprojected.data.WGS84 <- readRDS("Data/SpatialData.RDS")
map <- leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap) %>%
  setView(lng=175.5,lat=-40.0,zoom=8) %>%
  addPolygons(data = reprojected.data.WGS84$SubZones, color = "black", weight = 3, fillColor = "transparent", label = ~htmlEscape(Zone_Code)) %>%
  addCircleMarkers(data = reprojected.data.WGS84$MeasurementSites, color = "red",label = ~htmlEscape(sID)) %>%
  addCircleMarkers(data = reprojected.data.WGS84$PointSourceSites, color = "black", label = ~htmlEscape(Site.Name)) %>%
  addPolylines(data = reprojected.data.WGS84$RiverNetwork, color= "blue", label = ~htmlEscape(PrefixedLabel))

ui <- fluidPage(
  titlePanel("LWP"),
  leafletOutput("mymap", height = "100vh"),
  
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({map})
  
}

shinyApp(ui, server)