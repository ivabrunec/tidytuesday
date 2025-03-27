# tidy tuesday - march 11, 2025 - pixar films

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(ggradar)
library(patchwork)
library(MetBrewer)
library(magick)

# tt load not working for some reason?
pixar_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv')
public_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv')

films_plus_response <- left_join(pixar_films, public_response, join_by(film))
films_plus_response <- tidyr::drop_na(films_plus_response, film) 

films_plus_response <- films_plus_response %>%
  mutate(run_time = if_else(film == "Turning Red" & is.na(run_time), 100, run_time),
         run_time = if_else(film == "Lightyear" & is.na(run_time), 105, run_time),
         rotten_tomatoes = if_else(film == "Luca" & is.na(rotten_tomatoes), 91, rotten_tomatoes),
         rotten_tomatoes = if_else(film == "Turning Red" & is.na(rotten_tomatoes), 95, rotten_tomatoes),
         rotten_tomatoes = if_else(film == "Lightyear" & is.na(rotten_tomatoes), 74, rotten_tomatoes),
         metacritic = if_else(film == "Luca" & is.na(metacritic), 71, metacritic),
         metacritic = if_else(film == "Turning Red" & is.na(metacritic), 83, metacritic),
         metacritic = if_else(film == "Lightyear" & is.na(metacritic), 60, metacritic)
  )

films_plus_response$release_date <- as.Date(films_plus_response$release_date)

films_plus_response$score_diff <- films_plus_response$rotten_tomatoes - films_plus_response$metacritic

ggplot(data = films_plus_response,
       aes(x = release_date, y = score_diff)) +
  geom_point() +
  geom_text_repel(aes(label = film)) 


rescale_custom <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

films_plus_response <- films_plus_response |>
  mutate(across(c(rotten_tomatoes, metacritic, run_time), rescale_custom))

# specify color palette
col_pal <- met.brewer("Cross", n=nrow(films_radar))

# sort films by balanced-ness
films_radar <- films_plus_response |>
  mutate(
    avg_score = rowMeans(across(c(rotten_tomatoes, metacritic, run_time))), 
    balance = -apply(across(c(rotten_tomatoes, metacritic, run_time)), 1, sd)  
  ) |>
  arrange(desc(avg_score), balance) |> # sort: highest score, then best balance
  select(film, run_time, rotten_tomatoes, metacritic)

films_radar <- films_radar |>
  rename(
    "Runtime" = run_time,
    "Metacritic" = metacritic, 
    "Tomatoes" = rotten_tomatoes
  )

# create plots, put in list
plots <- list()

for (i in 1:nrow(films_radar)) {
  print(i)
  movie_name <- films_radar$film[i]
  film_single <- films_radar[i, , drop = FALSE]
  line_color <- col_pal[i]
  
  # create plot
  p <- ggradar(film_single, 
               background.circle.colour = "white",
               axis.line.colour = "gray60",
               gridline.min.colour = "gray60",
               gridline.mid.colour = "gray60",
               gridline.max.colour = "gray60",
               grid.min = 0, grid.mid = 0.5, grid.max = 1,
               grid.label.size = 0,
               group.point.size = 0,
               group.colours = line_color,
               fill = T,
               fill.alpha = 0.3,
               axis.label.size = 2) +
      ggtitle(movie_name) + 
      theme_void() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            legend.position = 'none',
            plot.margin = margin(10, 10, 10, 10)  
            )
  
  plots[[i]] <- p
}

final_plot <- wrap_plots(plots, ncol = 4) +
  plot_annotation(
    title = 'Pixar films: A 3-D story',
    theme = theme(
    plot.background = element_rect(fill = "grey92"),
    plot.title = element_text(size = 25, face = 'bold', hjust = 0)))
final_plot

ggsave('pixar_composite.png', height = 14, width = 8, dpi = 300)

# annotate
img <- image_read("pixar_composite.png")
img_drawn <- image_draw(img)

text(x = 600, y = 1000, labels = "Short and beloved", col = "black", cex = 3, font = 2)
text(x = 2200, y = 2600, labels = "Most unbalanced", col = "black", cex = 3, font = 2)
text(x = 1300, y = 3700, labels = "Falling short across the board", col = "black", cex = 3, font = 2)

arrows(x0 = 600, y0 = 1020, x1 = 680, y1 = 1070, col = "black", lwd = 5, length = 0.3)
arrows(x0 = 600, y0 = 1020, x1 = 520, y1 = 1070, col = "black", lwd = 5, length = 0.3)
arrows(x0 = 2250, y0 = 2620, x1 = 2180, y1 = 2670, col = "black", lwd = 5, length = 0.3)
arrows(x0 = 1120, y0 = 3720, x1 = 1050, y1 = 3780, col = "black", lwd = 5, length = 0.3)

# Close the drawing device
dev.off()

image_write(img_drawn, "tidy_tues_2025_03_11_pixar.png")
