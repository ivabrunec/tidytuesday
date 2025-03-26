# tidy tuesday - march 18, 2025 - palm trees
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(grid)
library(showtext)

font_add_google(name = 'Orelega One', family = 'orelega')
font_add_google(name = 'Roboto', family = 'roboto')
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
palms <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')

palms_clean <- palms |>
  separate_rows(main_fruit_colors, sep = "; ") |>
  drop_na(main_fruit_colors) |>
  group_by(main_fruit_colors) |>
  mutate(color_count = n()) |>
  ungroup() |>
  arrange(desc(color_count)) |>
  select(-color_count) 

# look at the unique colors
unique(palms_clean$main_fruit_colors)

# replace colors with no value with a corresponding hex code
palms_clean <- palms_clean |>
  mutate(main_fruit_colors = if_else(main_fruit_colors == "straw-coloured", '#d6bf86', main_fruit_colors),
         main_fruit_colors = if_else(main_fruit_colors == "cream", '#FFFDD0', main_fruit_colors)
)

grid_width <- 50
palms_clean <- palms_clean |>
  mutate(
    x = (row_number() - 1) %% grid_width, 
    y = -((row_number() - 1) %/% grid_width)
  )

ggplot(palms_clean, aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1, fill = main_fruit_colors)) +
  geom_rect(color = "white") +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() 

### shapes

# there's only one 'rounded' shape, so let's replace that with 'globose'
palms_clean <- palms_clean |>
  mutate(fruit_shape = if_else(fruit_shape == "rounded", "globose", fruit_shape)
         ) |>
  drop_na(fruit_shape)
# this drops a good number of rows which is unfortunate but it's okay.

# Function to generate shape coordinates
# thank u claude
generate_shape <- function(shape, center_x = 0, center_y = 0, size = 0.8) {
  if (shape == "globose") {
    tibble(
      x = center_x + 0.5 * cos(seq(0, 2 * pi, length.out = 100)),
      y = center_y + 0.5 * sin(seq(0, 2 * pi, length.out = 100)),
      shape = shape
    )
  } else if (shape == "ovoid") {
    tibble(
      x = center_x + c(-0.4, -0.2, 0, 0.2, 0.4, 0.2, 0, -0.2, -0.4) * size,
      y = center_y + c(0, 0.5, 0.7, 0.5, 0, -0.5, -0.7, -0.5, 0) * size,
      shape = shape
    )
  } else if (shape == "elongate") {
    tibble(
      x = center_x + c(-0.2, 0.2, 0.2, -0.2, -0.2) * size,
      y = center_y + c(-0.7, -0.7, 0.7, 0.7, -0.7) * size,
      shape = shape
    )
  } else if (shape == "ellipsoid") {
    tibble(
      x = center_x + c(-0.3, -0.2, 0, 0.2, 0.3, 0.2, 0, -0.2, -0.3) * size,
      y = center_y + c(0, 0.6, 0.8, 0.6, 0, -0.6, -0.8, -0.6, 0) * size,
      shape = shape
    )
  } else if (shape == "pyramidal") {
    tibble(
      x = center_x + c(-0.4, 0.4, 0, -0.4) * size, 
      y = center_y + c(-0.4, -0.4, 0.6, -0.4) * size,
      shape = shape
    )
  } else if (shape == "fusiform") {
    tibble(
      x = center_x + c(-0.2, 0, 0.2, 0, -0.2) * size,
      y = center_y + c(0, 0.7, 0, -0.7, 0) * size,
      shape = shape
    )
  } else {
    stop("Unknown shape")
  }
}

color_map <- c(
  "cream" = '#F3E5AB',      # Soft cream
  "straw-coloured" = '#E6D2B5', # Muted straw
  "black" = '#2C2C2C',      # Soft black
  "blue" = '#4A7C9B',       # Muted slate blue
  "brown" = '#8B4513',      # Saddle brown
  "green" = '#2E8B57',      # Sea green
  "grey" = '#708090',       # Slate grey
  "ivory" = '#FFFFF0',      # Soft ivory
  "orange" = '#D2691E',     # Warm copper
  "pink" = '#DB7093',       # Pale violet red
  "purple" = '#6A5ACD',     # Slate blue
  "red" = '#B22222',        # Firebrick red
  "white" = '#F5F5F5',      # Soft white
  "yellow" = '#DAA520'      # Goldenrod
)

# map these hex codes onto color
palms_clean <- palms_clean |>
  mutate(main_fruit_colors = color_map[main_fruit_colors])

palms_clean <- palms_clean |>
  # arrange by color and shape
  arrange(main_fruit_colors, fruit_shape) |>
  # ensure full grid coverage
  mutate(
    x = (row_number() - 1) %% 50, 
    y = -((row_number() - 1) %/% 50)
  )

# generate shape for each row
shape_data <- palms_clean |> 
  mutate(
    shape_coords = pmap(list(fruit_shape, x + 0.5, y - 0.5, main_fruit_colors), 
                        ~ generate_shape(..1, ..2, ..3, size = 0.8))
  ) |> 
  mutate(shape_coords = map(shape_coords, ~ .x %>% 
                              rename(shape_x = x, shape_y = y))) |> 
  unnest(shape_coords)

# 'waffle' plot
ggplot() +
  geom_rect(data = palms_clean, 
            aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1), 
            fill = NA, color = "grey40") +
  geom_polygon(data = shape_data, 
               aes(x = shape_x, y = shape_y, fill = main_fruit_colors, 
                   group = interaction(x, y)), 
               color = "grey40", linewidth = 0.2) +
  coord_fixed() +
  scale_fill_identity() +
  labs(title = "tropical tessellations",
       subtitle = "1,796 palm tree fruit shapes & colors") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 160, family = 'orelega', color = 'grey90', hjust = 0.5),
        plot.subtitle = element_text(size = 60, family = 'roboto', color = 'grey90', hjust = 0.5),
        plot.background = element_rect(fill = 'grey40', color = NA)) 

ggsave('tidytues_2025_03_18_palms.png', height = 14, width = 14)
