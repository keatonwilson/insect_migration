#Danaus plexippus mapping and biolim data
#Keaton Wilson
#keatonwilson@me.com
#2019-05-28


# Packages ----------------------------------------------------------------
library(tidyverse)
library(ggmap)
library(mapr)
library(lubridate)
library(maptools)
library(dismo)
library(lubridate)
library(rgdal)
library(sp)
library(raster)
library(viridis)
library(ggthemes)
library(rgeos)
library(maps)
library(viridis)
library(gganimate)
library(transformr)
library(dismo)

#Map API
#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

# Reading in the data -----------------------------------------------------
monarch_winter = read_csv("./data/monarch_winter.csv")
monarch_occ = read_csv("./data/monarch_gbif.csv")

#Bioclim data
bioclim.data <- raster::getData(name = "worldclim",
                                var = "bio",
                                res = 2.5,
                                path = "./data/")
#Cropping
max_lat_monarch <- ceiling(max(monarch_occ$latitude))
min_lat_monarch <- floor(min(monarch_occ$latitude))
max_lon_monarch <- ceiling(max(monarch_occ$longitude))
min_lon_monarch <- floor(min(monarch_occ$longitude))
geographic.extent <- extent(x = c(min_lon_monarch, max_lon_monarch, min_lat_monarch, max_lat_monarch))

bioclim_data = crop(bioclim.data, geographic.extent)

# Mapping ------------------------------------------------------------
#Quick map
monarch_occ %>%
  map_ggmap()

#Real map - polygon building
#Getting map data
usa = getData(country = 'USA', level = 1)

#extract states (need to uppercase everything)
to_remove = c("Alaska", "Hawaii")

#filtering
mapping = usa[-match(toupper(to_remove), toupper(usa$NAME_1)),]

#simplying polygons
simple_map_US = gSimplify(mapping, tol = 0.01, topologyPreserve = TRUE)

#Great lakes issues
lakes <- rgdal::readOGR("./data/10m_physical/ne_10m_lakes.shp")
lakes = lakes[lakes$scalerank==0,]
lakes = crop(lakes, geographic.extent)

#Testing geographic polygons
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA)

#Filtered US data for maps
monarch_occ_maps = monarch_occ %>%
  filter(longitude > -135 & longitude < -60) %>%
  filter(latitude > 15 & latitude < 47) %>%
  filter(!is.na(month)) %>%
  mutate(month = month(month, label = TRUE, abbr = TRUE))

#Occurence maps
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), #USA State filled polygons
               color=NA, size=0.25, fill = "grey10") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), #Outlines
               color="grey75", size=0.25, fill = NA) +
  geom_polygon(data = lakes, aes(x = long, y = lat, group = group), fill = "white", size = 0.25) + #Great lakes
  geom_point(data = monarch_occ_maps, 
             aes(x = longitude, y = latitude, color = as.factor(month)), 
             size = 1,
             alpha = 0.4) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm")) +
  theme_nothing(legend = TRUE) +
  # ggtitle("2000-2019") +
  coord_quickmap() +
  facet_wrap(~ month)

#Copied code below - not for monarchs yet
anim = ggplot(data = n_californica) +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_point(data = n_californica, aes(x = longitude, y = latitude), color = "yellow", alpha = 0.3, size = 3, shape = 3) +
  stat_density_2d(data = n_californica, aes(x = longitude, y = latitude, fill = stat(nlevel)), geom = "polygon", alpha = 0.5) +
  scale_fill_distiller(palette = 'Spectral') +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  coord_quickmap() +
  transition_states(month, transition_length = 2, state_length = 1) +
  labs(title = 'Month: {closest_state}') +
  enter_fade() +
  exit_fade()