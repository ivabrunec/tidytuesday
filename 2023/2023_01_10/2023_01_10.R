# Jan 10, 2023
# bird feeder data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
tuesdata <- tidytuesdayR::tt_load('2023-01-10')

data_pub <- tuesdata$PFW_2021_public
usa <- map_data('usa')
canada <- map_data('world') |>
  filter(region == 'Canada' | subregion=='Alaska') |>
  filter(long < -50)

data_pub <- data_pub |>
  filter(longitude > -150 & longitude < -50 & latitude > 20)

col_pal <- colorspace::sequential_hcl(100, palette = "Dark Mint")

g <- ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group ), color='grey20', fill= NA, size = .2) + 
  geom_polygon(data = canada, aes(x=long, y = lat, group = group ), color='grey20', fill= NA, size = .2) + 
  geom_hex(data = data_pub, aes(x = longitude, y = latitude, fill= stat(log(count))), binwidth = c(1.2,1)) +
  scale_fill_gradientn(colours = col_pal) +
  theme_void() +
  theme(legend.position = '',
        plot.background = element_rect(fill='grey96', color=NA)) +
  coord_fixed(1.3)

g

library(rayshader)
plot_gg(g, scale = 20, raytrace = T, background='grey96',
        windowsize=c(800,800))

render_camera(zoom=.5, theta=0, phi=90)
render_highquality(filename = 'birds_test.png', samples = 360)


# month-by-month
library(poissoned)

month_data <- data_pub |>
  group_by(Month, species_code) |>
  summarise(total_birds=sum(how_many))

# match species code with descriptive name
# downloaded from: https://feederwatch.org/explore/raw-dataset-requests/
data_dict <- read.csv('feederwatch_datadict.csv') |>
  select(species_code, name)

month_data <- merge(month_data, data_dict, by=c('species_code'))
# assign 'other' if fewer than 100 birds of that species were observed
month_data$name[month_data$total_birds < 100] <- "Other"

# poissoned plot
# helpful code snippet from Nicola Rennie: 
# https://github.com/nrennie/tidytuesday/blob/main/2022/2022-01-11/20220111.R
month_grid <- month_data %>% 
  rowwise() %>% 
  mutate(
    t = sqrt(total_birds / 10),
    pnts = list(poisson_disc(ncols = t, nrows = t, cell_size = 1 / t))
  ) %>% 
  ungroup() %>% 
  unnest(pnts)


month_grid$Month <- as.factor(month_grid$Month)
month_grid$Month <- recode_factor(month_grid$Month, '1'='JAN','2'='FEB','3'='MAR','4'='APR','11'='NOV','12'='DEC')

ggplot() +
  geom_tile(data = month_grid, aes(0.5, 0.5, width = 1, height = 1), fill=NA, stat = "unique") +
  geom_point(data = month_grid, aes(x, y), color='#fffda0', size=0.002) +
  facet_wrap(~ Month, ncol=6, strip.position='top') +
  theme_void() +
  theme(legend.position='',
        plot.background = element_rect(fill=NA, color=NA),
        panel.background = element_rect(fill=NA, color='black'),
        strip.text.x = element_text(size=50, family='mono')) 

ggsave('poisson_temp.png', height = 1.2, width=6)

# combine into full 'poster'
library(magick)

map <- image_read('birds_test.png')
buzz <- image_read("poisson_temp.png")

buzz1 <- image_scale(buzz,'400')

temp <- image_composite(map, buzz1, offset = "+200+600") |>
  image_annotate("birds of a feather", size = 50, location = "+10+10",
                 font='mono sans') |>
  image_annotate("Bird sightings reported on FeederWatch between 2020 and 2021", size = 20, location = "+10+70",
                 font='mono sans') 
temp

image_write(temp, 'combined_test.png')
