# tidy tuesday - march 18, 2025 - palm trees
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggforce)
library(grid)

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

# function to generate shape coordinates
# generated with chatgpt
generate_shape <- function(shape, center_x = 0, center_y = 0, size = 1) {
  if (shape == "globose") {
    tibble(
      x = center_x + size * cos(seq(0, 2 * pi, length.out = 100)),
      y = center_y + size * sin(seq(0, 2 * pi, length.out = 100)),
      shape = shape
    )
  } else if (shape == "ovoid") {
    tibble(
      x = center_x + c(-0.6, -0.3, 0, 0.3, 0.6, 0.3, 0, -0.3, -0.6) * 1.2,
      y = center_y + c(0, 0.5, 0.7, 0.5, 0, -0.5, -0.7, -0.5, 0) * 1.2,
      shape = shape
    )
  } else if (shape == "elongate") {
    tibble(
      x = center_x + c(-0.3, 0.3, 0.3, -0.3, -0.3) * size,
      y = center_y + c(-1, -1, 1, 1, -1) * size,
      shape = shape
    )
  } else if (shape == "ellipsoid") {
    tibble(
      x = center_x + c(-0.5, -0.3, 0, 0.3, 0.5, 0.3, 0, -0.3, -0.5) * size,
      y = center_y + c(0, 0.8, 1, 0.8, 0, -0.8, -1, -0.8, 0) * size,
      shape = shape
    )
  } else if (shape == "pyramidal") {
    tibble(
      x = center_x + c(-0.5, 0.5, 0, -0.5) * 1.5, # size = 1 was a bit small
      y = center_y + c(-0.5, -0.5, 0.7, -0.5) * 1.5,
      shape = shape
    )
  } else if (shape == "fusiform") {
    tibble(
      x = center_x + c(-0.3, 0, 0.3, 0, -0.3) * 1.2,
      y = center_y + c(0, 1, 0, -1, 0) * 1.2,
      shape = shape
    )
  } else {
    stop("Unknown shape")
  }
}

# Define shapes and their positions
shapes <- c("globose", "ovoid", "elongate", "ellipsoid", "pyramidal", "fusiform")

# Generate shape coordinates
shape_data <- bind_rows(
  lapply(seq_along(shapes), function(i) {
    generate_shape(shapes[i], center_x = (i - 1) %% 3 * 3, center_y = -((i - 1) %/% 3) * 3)
  })
)

for (shape in shapes) {
  
  shape_subset <- shape_data %>% filter(shape == !!shape)
  
  angled_gradient <- linearGradient(
    c("red", "orange"),
    x1 = min(shape_subset$x), y1 = min(shape_subset$y), 
    x2 = max(shape_subset$x), y2 = max(shape_subset$y),
    default.units = "snpc")
  
  ggplot(shape_subset, aes(x = x, y = y, group = shape)) +
    geom_polygon(fill = angled_gradient) +
    #geom_shape(radius = unit(1, 'cm'),
    #           fill = NA) +
    coord_equal()
}




# Plot using ggplot
ggplot(shape_data, aes(x, y, group = shape, fill = fill_color)) +
  geom_polygon(color = "black") +
  scale_fill_manual(values = c("globose" = "red", "ovoid" = "blue", "elongate" = "green",
                               "ellipsoid" = "purple", "pyramidal" = "orange",
                               "fusiform" = "yellow")) +
  coord_fixed() +
  theme_void() +
  labs(title = "Different Shape Representations")
