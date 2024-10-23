# cia factbook data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(ggradar)
library(scales)
library(showtext)

font_add_google(name = 'Roboto Condensed', family = 'roboto')
font_add_google(name = 'Abel', family = 'abel')
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2024-10-22')
cia_dat <- tuesdata$cia_factbook

## attempted a geofacet plot
# don't love it but it was an attempt
rescale_to_100 <- function(x){
  rescale(x, to = c(0, 100))
}

cia_dat <- cia_dat |>
  mutate(birth_z = rescale_to_100(birth_rate),
         death_z = rescale_to_100(death_rate),
         infant_z = rescale_to_100(infant_mortality_rate),
         internet_z = rescale_to_100(internet_users),
         maternal_z = rescale_to_100(maternal_mortality_rate),
         migration_z = rescale_to_100(net_migration_rate)
  )

cia_z <- cia_dat |>
  select(country, birth_z, death_z, infant_z, internet_z, maternal_z, migration_z)

# to check the labels
test <- geofacet::world_countries_grid1

cia_df <- tidyr::pivot_longer(cia_z, -country)

replacement_list <- c("United States" = "United States of America", 
                      "Russia" = "Russian Federation",
                      "Congo, Democratic Republic of the" = "Congo (Democratic Republic of the)",
                      "Congo, Republic of the" = "Congo",
                      "Iran" = "Iran (Islamic Republic of)",
                      "Burma" = "Myanmar",
                      "Vietnam" = "Viet Nam",
                      "Cote d'Ivoire" = "Côte d'Ivoire",
                      "United Kingdom" = "Great Britain and Northern Ireland",
                      "Laos" = "Lao People's Democratic Republic",
                      "Korea, North" = "North Korea",
                      "Korea, South" = "South Korea",
                      "Bosnia and Herzegovina" = "Bosnia & Herzegovina",
                      "Moldova" = "Moldova (Republic of)",
                      "Bahamas, The" = "Bahamas",
                      "Gambia, The" = "Gambia",
                      "Brunei" = "Brunei Darussalam",
                      "Trinidad and Tobago" = "Trinidad & Tobago",
                      "Micronesia, Federated States of" = "Micronesia",
                      "Saint Lucia" = "St. Lucia",
                      "Antigua and Barbuda" = "Antigua & Barbuda",
                      "Saint Vincent and the Grenadines" = "St. Vincent & the Grenadines",
                      "Saint Kitts and Nevis" = "St. Kitts & Nevis"
                      )
cia_df <- cia_df %>%
  mutate(country = recode(country, !!!replacement_list))

col_pal <- MetBrewer::met.brewer("Cross", n=6)

# create radar plot with geofacet
ggplot(cia_df, aes(x = name, y = value, group = country)) +
  geom_point(aes(group = country, color = name), 
             alpha = 0.4) +
  geom_segment(aes(x = name, y = 0, xend = name, yend = value, color = name)) +
  scale_color_manual(values = col_pal) +
  coord_polar() +
  facet_geo(~country, grid = "world_countries_grid1") +
  theme_void() +
  theme(legend.position = 'none',
        strip.text = element_text(size = 4, color = 'white'),
        plot.background = element_rect(color = NA, fill = 'black'),
        panel.background = element_rect(color = 'grey20', fill = 'black'),
        plot.title = element_text(size = 50)) +
  labs(title = '') # hack to add more space

ggsave('map_geofacet.png', width = 14, height = 12)


## create example with equidistant lines for inset
example_df <- data.frame(country = rep('A', 6),
                         name = c('Birth rate','Death rate','Infant mortality','Internet users','Maternal mortality','Net migration'),
                         value = rep(100, 6))

ggplot(example_df, aes(x = name, y = value, group = country)) +
  geom_point(aes(group = country, color = name), 
             alpha = 0.4, size = 8) +
  geom_segment(aes(x = name, y = 0, xend = name, yend = value, color = name)) +
  geom_text(aes(x = name, y = value+4, label = name, color = name), nudge_x = .2, size = 5) +
  ylim(c(0,150)) +
  coord_polar() +
  scale_color_manual(values = col_pal) +
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(color = NA, fill = 'black'),
        panel.background = element_rect(color = NA, fill = 'black'),
        panel.grid = element_blank(),
        )

ggsave('legend_inset.png', height = 4, width = 4)

## combine
library(magick)

map_main <- image_read('map_geofacet.png')
inset_legend <- image_read("legend_inset.png")

test <- image_composite(map_main, image_scale(inset_legend, "x700"), offset='+200+2800')
test
image_write(test, 'geofacet_map_legend.png')
