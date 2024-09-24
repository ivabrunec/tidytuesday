
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2024-09-24')

country_results_df <- tuesdata$country_results_df
individual_results_df <- tuesdata$individual_results_df
timeline_df <- tuesdata$timeline_df

# assuming the NA means 0 in this case
timeline_df$female_contestant[is.na(timeline_df$female_contestant)] <- 0

timeline_df$ratio <- timeline_df$female_contestant / timeline_df$male_contestant
# create segment lines
timeline_df$segment_male_start <- (1 - timeline_df$ratio) * -1

ggplot(data = timeline_df) +
  geom_rect(aes(ymin = segment_male_start, xmin = year-.4,
                 ymax = 0, xmax = year+.4),
            fill = 'grey75') +
  geom_rect(aes(ymin = 0, xmin = year-.4,
                ymax = ratio, xmax = year+.4),
            fill='coral') +
  geom_point(aes(y = segment_male_start, x = year),
             size = 2, color = 'grey75') +
  geom_point(aes(y = ratio, x = year),
             size = 2, color = 'coral') +
  #geom_text(aes(y = 0, x = year,
  #              label = (round(ratio,2))*100),
  #          vjust = 1.5, size = 8, color = 'black', family = 'roboto') +
  scale_y_continuous(breaks = c(0, .05, .1, .15, .2)) + 
  theme_minimal() +
  theme(axis.text.y = element_text(size = 20, family = 'roboto', color = 'grey20'),
        axis.text.x = element_text(size = 30, family = 'roboto'),
        axis.title = element_blank(),
        plot.background = element_rect(fill = 'grey90', color = NA),
        plot.title = element_text(color = 'black', 
                                  size = 35, 
                                  family = 'roboto',
                                  hjust = .5)) +
  labs(title = 'Share of female contestants in the International Mathematical Olympiad, 1959-2024')

ggsave('math_olympiad.png', height = 6, width = 8)

