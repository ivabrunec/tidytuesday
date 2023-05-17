# tornadoes
# tidy tuesday: may 16, 2023
# rayshader

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(elevatr)
library(raster)
library(sf)
library(rayshader)
library(rayrender)

tuesdata <- tidytuesdayR::tt_load('2023-05-16')
tornados <- tuesdata$tornados |>
  tidyr::drop_na(mag) |>
  mutate(decade =  cut_interval(yr, n = 8)) 

# get state info 
states <- spData::us_states

# get centroid of each state so we can plot the symbol
states$centroid <- st_centroid(states$geometry) 
states <- cbind(states, st_coordinates(states$centroid))

# get state abbreviation
state_df <- data.frame(state = state.name, 
                       st = state.abb, stringsAsFactors = FALSE)

# merge
states <- merge(states, state_df, by.x = "NAME", by.y = "state" )

# get raster of US
us_raster <-
  giscoR::gisco_get_countries(country = "USA", 
                              resolution = '03', epsg = "4326") %>%
  get_elev_raster(z = 3, clip = 'location')

# sanity check
raster::plot(us_raster)

# clip contiguous only (sorry alaska)
e <- as(extent(-125, -70, 25, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
us_cropped <- crop(us_raster, e)
plot(us_cropped)

# convert to matrix & plot
us_mat <- raster_to_matrix(us_cropped)  

us_mat |>
  sphere_shade(texture = 'bw', sunangle = 90) |>
  #sphere_shade(texture = create_texture('#723d46', '#695958', '#6D7789','#cfffe5',  '#A2C6AD'), sunangle = 90) |>
  #add_shadow(ray_shade(ukr_mat,sunaltitude=30, sunangle = 0),max_darken=0.65) |>
  plot_3d(us_mat, zscale = 300, windowsize = (c(1000,800)),
          theta = 12, phi = 60, zoom = .8,
          background = 'black')

tornados2 <- filter(tornados, decade == '(2013,2022]') |>
  filter(st != 'HI' & st != 'AL' & st != 'PR' & st != 'DC') |>
  group_by(st) |>
  summarise(mean_mag = mean(mag))
  
for (i in 1:nrow(states)){
  
  # rayshader has flipped x & y
  # so to avoid confusion, I'm coding this here
  coord_1 = states$Y[i]
  coord_2 = states$X[i]
  
  tornado_coords <- c(coord_1, coord_2)
  
  cur_state <- states$st[i]
  
  # get current state
  cur_tornados <- filter(tornados2, st == cur_state)
  
  if (nrow(cur_tornados) == 0){
    next
  }
  
  # size of tornado
  size <- cur_tornados$mean_mag

  t = seq(0,2*pi,length.out=1000)
  circle_coords_lat = tornado_coords[1] + (size*2) * t/8 * sin(t*6)
  circle_coords_long = tornado_coords[2] + (size*2) * t/8 *  cos(t*6)
  circle_coords_alt = seq(from = 10, to = size*1000, length.out = 1000)
  
  
  render_path(extent = attr(us_cropped,"extent"), heightmap = us_mat, clear_previous = F,
              lat = unlist(circle_coords_lat), long = unlist(circle_coords_long),
              altitude = circle_coords_alt, zscale = 30, color="#cf6101", offset=200, linewidth=5)
  
}

render_highquality(filename='tornados_light.png', samples = 360,
                   width = 1200, height = 800,
                   path_material = rayrender::light,
                   ground_material = rayrender::generate_ground(material = metal(color="grey20", fuzz = .05)))

# the following are experimental
# save scene in cache
scene = render_highquality(cache_filename = "tornado4",
                           return_scene = T)

render_scene(scene, width = 800, height = 800,)

rayrender::render_scene(scene,lookfrom=c(0,8000,5),
             width=800, height=800)


motion = generate_camera_motion(positions = get_saved_keyframes(), 
                                frames=80, type = "cubic")


render_animation(scene=scene,
                 camera_motion = motion, 
                 samples=100,width = 800,height=800, 
                 filename = 'tornado_temp')

av::av_encode_video(glue::glue("tornado_temp{1:80}.png"), 
                    framerate=20, output = "tornado_video2.mp4")

# or a 360 deg loop
for(i in 1:360) {
  render_camera(theta=135-i,phi=30,zoom=0.6,fov = 90)
  render_highquality(line_radius = .8,
                     filename=sprintf("tornado%i.png",i))
}

av::av_encode_video(glue::glue("tornado{1:360}.png"), 
                    framerate=20, output = "tornado_video.mp4")
file.remove(glue::glue("tornado{1:360}.png"))
