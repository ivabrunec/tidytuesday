# orca encounters
# turns out you have to comment out setwd statements for shiny apps
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(shiny)
library(leaflet)
library(scico)

tuesdata <- tidytuesdayR::tt_load('2024-10-15')
orcas <- tuesdata$orcas |>
  tidyr::drop_na(year)

# color palette based on year
unique_years <- sort(unique(orcas$year))
colors <- scico(8, palette = "hawaii")
color_vector <- colors[match(orcas$year, unique_years)]  

ui <- fluidPage(
  leafletOutput("map", width = "100%", height = "800px")  
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    
    # initialize map
    map <- leaflet() |>
  #    addTiles()
    addProviderTiles(providers$CartoDB.DarkMatter) 
    
    # circle markers, color based on year of sighting    
    for (i in 1:nrow(orcas)) {
      map <- map |>
        addCircleMarkers(
          lng = orcas$begin_longitude[i],  
          lat = orcas$begin_latitude[i],   
          color = color_vector[i],
          radius = 3,
          fillOpacity = 0.8,
          popup = sprintf(
            '<a href="%s" target="_blank">Encounter details and photos</a><br>%s', 
            orcas$link[i], orcas$duration[i]
          )
        )
    }
    
    # add legend
    map <- map |>
      addLegend(
        position = "bottomright", 
        colors = colors, 
        labels = unique_years,
        title = "Year", 
        opacity = 0.7
      )
    # render map
    map
  })
}

shinyApp(ui, server)
