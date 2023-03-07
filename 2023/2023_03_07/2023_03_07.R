# March 7 2023: Numbats

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(sf)
library(ggimage)
library(showtext)

font_add_google(name = "Righteous", family = "Righteous")
font_add_google(name = "Overpass", family = "Overpass")
showtext_auto(enable = T)

# image from
# https://cdn.britannica.com/87/81287-050-4A1A5B72/numbat-anteater.jpg


tuesdata <- tidytuesdayR::tt_load('2023-03-07')
numbats <- tuesdata$numbats

numbats$eventDate <- as.Date(numbats$eventDate)
# remove entries with no date
numbats <- numbats[!is.na(numbats$eventDate),]

# too few to count
numbats <- filter(numbats, year > 2000)

numbats$lat <- round(numbats$decimalLatitude, digits = 3)
numbats$long <- round(numbats$decimalLongitude, digits = 3)

numbats <- numbats |>
  mutate(cuts = cut(year, c(2005, 2010, 2015, 2020, 2023)))

# create year ranges
numbats_sum <- numbats |>
  group_by(cuts, lat, long) |>
  tally()

aus_res10 <-
  giscoR::gisco_get_countries(resolution = "10", country = "AUS") %>%
  mutate(res = "10M")

numbats_sum$image <- 'numbat-removebg-preview.png'
numbats_sum$cuts <- as.factor(numbats_sum$cuts)
numbats_sum$size2 <- numbats_sum$n / 54 # total number is 538

numbat_counts <- numbats_sum |>
  group_by(cuts) |>
  summarise(total_sum = sum(n))

numbat_facets <- c(
  `(2005,2010]`="2005-2010",
  `(2010,2015]`="2010-2015",
  `(2015,2020]`="2015-2020",
  `(2020,2023]`="2020-2023 (to date)"
)

ggplot() +
  geom_sf(data = aus_res10, color=NA, fill = 'grey40') +
  geom_point(data = numbats_sum, aes(long, lat, size=size2), color='white') +
  facet_wrap(cuts ~ ., labeller=as_labeller(numbat_facets),nrow = 1) +
  theme_void() +
  theme(legend.position = '',
        plot.background = element_rect(fill = '#062405', color=NA),
        strip.text = element_text(color='white', family = 'Overpass', size = 40),
        plot.title = element_text(color = 'white', family = 'Righteous', size = 70),
        plot.caption = element_text(color = 'white', family = 'Overpass', size = 20)) +
  labs(title = 'numbats in numbers',
       caption = 'Locations where numbats were spotted, weighted by the proportion of observations')

# save first panel
ggsave('panel1.png', width = 6, height = 2, dpi = 400)

# bottom panel
numbats_count <- numbats_sum |>
  group_by(cuts) |>
  summarise(total_num = sum(n))

numbats_count$size <- numbats_count$total_num / 200
numbats_count$image <- 'numbat-removebg-preview.png'

numbat_facets2 <- c(
  `(2005,2010]`="68",
  `(2010,2015]`="182",
  `(2015,2020]`="206",
  `(2020,2023]`="82"
)

ggplot() +
  ggforce::geom_circle(data=numbats_count, aes(x0=1,y0=1, r = size), color = NA, fill = '#b080d6') +
  geom_image(data = numbats_count, aes(x=1, y=1, image=image), size=I(numbats_count$size))+
  facet_wrap(cuts ~ ., labeller=as_labeller(numbat_facets2),nrow = 1) +
  theme_void() +
  theme(legend.position = '',
        plot.background = element_rect(fill = '#062405', color=NA),
        strip.text = element_text(color='white', family = 'Righteous', size = 40),
        plot.caption = element_text(color = 'white', family = 'Overpass', size = 20)) +
  labs(caption = 'Total number of numbats spotted, per 5-year range')

# save second panel
ggsave('panel2.png', width = 6, height = 1.5, dpi = 400)

# combine with magick
library(magick)
img1 <- image_read('panel1.png')
img2 <- image_read('panel2.png')
img <- c(img1, img2)

img_final <- image_append(img, stack = TRUE)

image_write(img_final, 'numbats_final.png')
