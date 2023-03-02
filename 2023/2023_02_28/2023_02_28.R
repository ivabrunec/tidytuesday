# 2023-02-28
# afrisenti

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(showtext)

font_add_google(name = "Nunito", family = "Nunito")
font_add_google(name = "Advent Pro", family = "Advent Pro")
showtext_auto(enable = T)

tuesdata <- tidytuesdayR::tt_load('2023-02-28')
afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages

afrisenti <- merge(afrisenti, languages)

# calculate total positives + negatives, calculate proportions
afrisenti_sum <- afrisenti |>
  group_by(language) |>
  add_count() |>
  group_by(language, label, n) |>
  tally(name = 'n_by_sent') |>
  mutate(prop_sent = n_by_sent / n)

font_name = 'Advent Pro'

ggplot() +
  ggforce::geom_circle(data = afrisenti_sum, 
                       aes(x0 = 1, y0 = 0+(prop_sent/2), r = prop_sent/2,
                           color = label
                           )) +
  facet_wrap(~language, nrow = 2) +
  scale_color_manual(values = c('#FF8552','#A9ADB3','#297373')) +
  #scale_color_manual(values=met.brewer("Peru1", 3))+
  labs(title = 'African Language Sentiments, Proportionally') +
  theme_minimal() +
  theme(legend.position = 'bottom',
        strip.text.x = element_text(family=font_name, size = 20),
        plot.background = element_rect(fill = 'grey96', color = NA),
        legend.key.size = unit(.5,"line"),
        plot.title = element_text(family=font_name, size = 50),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family='Nunito', size = 15),
        axis.title.y = element_text(family = 'Nunito', size = 18),
        legend.title = element_blank(),
        legend.text = element_text(family = 'Nunito', size = 20),
        panel.grid.minor = element_line(size = 0.2),
        panel.grid.major = element_line(size = 0.2)
        ) +
  ylab('Proportion of sentiment')

ggsave('african_sentiments.png', width = 7, height = 3)
