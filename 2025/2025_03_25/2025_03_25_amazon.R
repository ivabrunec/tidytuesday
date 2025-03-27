# tidy tuesday - march 25, 2025 - amazon reports
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(ggbump)
library(ggrepel)
library(ggtext)

font_add_google(name = 'Orelega One', family = 'orelega')
font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

report_words_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-25/report_words_clean.csv')

df_counts <- report_words_clean |>
  count(year, word, sort = TRUE)

top_words_2023 <- df_counts |>
  filter(year == 2023) |>
  slice_max(n, n = 10) |>
  pull(word)

# rank all word counts per year
df_ranked <- df_counts |>
  group_by(year) |>
  mutate(rank = rank(-n, ties.method = "first")) |>
  ungroup()

# we only want the top words in 2023
df_top_tracked <- df_ranked |>
  filter(word %in% top_words_2023)

amazon_orange <- "#FF9900"
grayscale <- gray.colors(9, start = 0.85, end = 0.2)
color_palette <- c(amazon_orange, grayscale)

ggplot(df_top_tracked, aes(x = year, y = rank, color = word, group = word)) +
  geom_bump(size = 1, smooth = 10) +
  geom_point(size = 2) +  
  #geom_text(data = df_top_tracked |> filter(year == min(year)),
  #          aes(x = year - 0.5, label = word),
  #          size = 14, hjust = 1) +
  geom_text_repel(data = df_top_tracked |> filter(year == min(year)),
                  aes(x = year - 0.5, label = word),
                  size = 14, hjust = 1, direction = "y",
                  nudge_x = -1, box.padding = 0.1, segment.color = "grey80") +
  #geom_text(data = df_top_tracked |> filter(year == max(year)),
  #          aes(x = year + 0.5, label = word),
  #          size = 12, hjust = 0) +
  geom_text_repel(data = df_top_tracked |> filter(year == max(year)),
                  aes(x = year + 0.5, label = word),
                  size = 14, hjust = 0, direction = "y",
                  nudge_x = 1, box.padding = 0.1, segment.color = "grey80") +
  geom_richtext(
    aes(x = 2014, y = 120, 
        label = "<span style='color:grey40;'>The word <span style='color:#FF9900; font-weight:bold;'>billion</span><br> was ranked 212th in 2005, <br> and 1st in 2022.</span>"),
    fill = NA, label.color = "white", size = 16, hjust = 0, lineheight = 0.3
  ) +
  theme_minimal() +
  scale_y_reverse() +
  scale_color_manual(values = color_palette) +
  labs(title = "rise of the billion",
       y = "Word frequency rank") +
  xlim(c(2001, 2028)) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = 'grey98', color = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 50),
        plot.title = element_text(size = 60, hjust = 0.5),
        axis.text = element_text(size = 40),
        text = element_text(family = 'roboto')) 

ggsave('tidytues_03_25_2025_amazon.png', height = 14, width = 6)
