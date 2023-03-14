# european drug development

library(ggplot2)
library(dplyr)
library(ggmosaic)
library(showtext)
library(cowplot)

font1 = 'Work Sans'
font_add_google(name = "DM Serif Display", family = "DM")
font_add_google(name = font1, family = font1, regular.wt = 300)
showtext_auto(enable = T)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

tuesdata <- tidytuesdayR::tt_load('2023-03-14')

# panel 1 
drugs0 <- tuesdata$drugs |>
  filter(!is.na(category), !is.na(authorisation_status))

p1 <- ggplot() +
  geom_mosaic(data = drugs0,
              aes(x = product(category), fill=authorisation_status)) +
  theme_mosaic() +
  scale_x_productlist(position = "bottom") +
  scale_fill_manual(values = c('#118767','orangered','#68b1fc'))+
  theme(legend.position = '',
        axis.text.y = element_text(family = font1, size = 50, color='black',angle = 50),
        axis.title = element_blank(),
        axis.ticks.length.x=unit(.6, "cm"),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(family = font1, color='black', size = 50),
        plot.background = element_rect(fill='grey96', color=NA),
        plot.title = element_text(family = 'DM', size = 90,hjust = -.05),
        plot.subtitle = element_text(family = 'DM', size = 45, hjust=.9)
        ) +
  labs(title = 'Authorised, Refused, Withdrawn',
       subtitle = 'The landscape of European drug development')

# panel 2
drugs1 <- tuesdata$drugs |>
  filter(!is.na(therapeutic_area)) |>
  filter(category == 'human') |>
  group_by(therapeutic_area)|>
  add_tally() |>
  ungroup() |>
  top_frac(.3) |>
  mutate(therapeutic_area = forcats::fct_reorder(therapeutic_area, n))


p2 <- ggplot() +
  geom_mosaic(data = drugs1,
              aes(x = product(therapeutic_area), 
                  fill=authorisation_status)) +
  theme_mosaic() +
  coord_flip() + 
  scale_x_productlist(position = "top") +
  scale_fill_manual(values = c('#118767','orangered','#68b1fc'))+
  theme(legend.position = '',
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(family = font1,color='black', size = 30),
        plot.background = element_rect(fill='grey96', color=NA),
        plot.title = element_text(family ='DM',size = 50, hjust=.25)) +
  labs(title = 'Most frequent therapeutic areas')


# panel 3
drugs2 <- tuesdata$drugs |>
  filter(category == 'human') |>
  group_by(marketing_authorisation_holder_company_name)|>
  add_tally() |>
  ungroup() |>
  top_frac(.3) |>
  mutate(marketing_authorisation_holder_company_name = forcats::fct_reorder(marketing_authorisation_holder_company_name, n))


p3 <- ggplot() +
  geom_mosaic(data = drugs2,
              aes(x = product(marketing_authorisation_holder_company_name), fill=authorisation_status)) +
  theme_mosaic() +
  coord_flip() +
  scale_x_productlist(position = "top") +
  scale_fill_manual(values = c('#118767','orangered','#68b1fc'))+
  theme(legend.position = '',
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(family = font1,color='black', size = 30),
        plot.background = element_rect(fill='grey96', color=NA),
        plot.title = element_text(family ='DM',size = 50, hjust=.5)) +
  labs(title = 'Most active pharmaceutical companies')

# combine
right_side <- plot_grid(p2, p3, align = 'v', nrow=2)
plot_grid(p1, right_side, ncol=2, rel_heights = c(2,1)) +
  theme(plot.background = element_rect(fill = "grey96", colour = NA))

ggsave('pharma_final.png',width = 16,height=10)
