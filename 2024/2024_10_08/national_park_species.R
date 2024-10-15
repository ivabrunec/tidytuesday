# animal species in most visited national parks
# NOT FINISHED. goal is to return to polish.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)
library(scico)

font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2024-10-08')
nps_data <- tuesdata$most_visited_nps_species_data

nps_coords <- nps_data |>
  filter(Occurrence == "Present" & 
           CategoryName != "Vascular Plant" & 
           CategoryName != "Non-vascular Plant") |>
  group_by(ParkName) |>
  arrange(CategoryName) |>
  mutate(
    x = (row_number() - 1) %% 101,
    y = (row_number() - 1) %/% 101) |>
  ungroup()

# define color palette - 16 categories
col_pal <- scico(16, palette = 'hawaii')



# faceted plot
ggplot(nps_coords, aes(x = x, y = y, label = SciName, color = CategoryName)) +
  facet_wrap(~ParkName, nrow = 3, scales = "free") +
  geom_point(size = 1) +
  scale_color_manual(values = col_pal) +
  theme_minimal() +
  theme(legend.position = 'bottom')

# shiny app
library(plotly)
library(shiny)

ui <- fluidPage(
  titlePanel("Interactive Plot with Group Selection"),
  
  # dropdown with park name selection
  selectInput("ParkName", "Select park:", choices = unique(nps_coords$ParkName)),
  
  plotlyOutput("plot")
)

server <- function(input, output) {
  
  output$plot <- renderPlotly({
    
    # subset based on park name
    df_subset <- nps_coords[nps_coords$ParkName == input$ParkName, ]
    
    p <- ggplot(df_subset, aes(x = x, y = y, label = SciName, color = CategoryName)) +
      geom_point(size = 1) +
      scale_color_manual(values = col_pal) +
      labs(title = input$ParkName) +
      theme_minimal() 
    
    ggplotly(p, tooltip = "label")
  })
}

shinyApp(ui = ui, server = server)
