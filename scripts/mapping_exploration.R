#Migration exploration script
#Keaton Wilson
#keatonwilson@me.com
#2019-05-09

#packages
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
#Map API
#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#data read in
n_californica = read_csv("./data/n_californica_west.csv")
v_annabella = read_csv("./data/v_annabella_west.csv")
v_atalanta = read_csv("./data/v_atalanta_west.csv")

#Creating month features for mapping later on
n_californica = n_californica %>%
  mutate(month = month(date, label = TRUE)) %>%
  drop_na()

v_annabella = v_annabella %>%
  mutate(month = month(date, label = TRUE)) %>%
  drop_na()

v_atalanta = v_atalanta %>%
  mutate(month = month(date, label = TRUE)) %>%
  drop_na()

#Building map polygon of western US

# Building Predictions and Plotting ---------------------------------------

#Pulling in polygons for states and provinces
#Getting map data
usa = getData(country = 'USA', level = 1)

#extract states (need to uppercase everything)
to_keep = c("Washington", "Oregon", "California", "Idaho", "Wyoming",
            "Montana", "Utah", "Nevada", "Arizona", "New Mexico", "Colorado")

#filtering
mapping = usa[match(toupper(to_keep), toupper(usa$NAME_1)),]

#simplying polygons
simple_map_US = gSimplify(mapping, tol = 0.01, topologyPreserve = TRUE)

#Pulling Canada Province data
can = getData(country = 'CAN', level = 1)
province = c("British Columbia", "Alberta", "Saskatchewan")
can_mapping = can[match(toupper(c(province)), toupper(can$NAME_1)),]
simple_map_can = gSimplify(can_mapping, tol = 0.01, topologyPreserve = TRUE)


#Testing geographic polygons
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) 

#n_californica total map
ggplot() +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_point(data = n_californica, aes(x = longitude, y = latitude), color = "yellow", alpha = 0.6) +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  coord_quickmap()

#Let's facet by month
n_californica_by_month = ggplot(data = n_californica) +  
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_US, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color=NA, size=0.25, fill = "#440154FF") +
  geom_polygon(data=simple_map_can, aes(x=long, y=lat, group=group), 
               color="grey50", size=0.25, fill = NA) +
  geom_point(data = n_californica, aes(x = longitude, y = latitude), color = "yellow", alpha = 0.2, size = 1, shape = 3) +
  stat_density_2d(data = n_californica, aes(x = longitude, y = latitude, fill = stat(nlevel)), geom = "polygon", alpha = 0.5) +
  scale_fill_distiller(palette = 'Spectral') +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(2, "cm"),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  theme_nothing(legend = TRUE) +
  coord_quickmap() +
  facet_wrap(~ month)

ggsave(plot = n_californica_by_month, filename = "./output/n_californica_by_month.png", device = "png")

#Animation
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

animate(anim)
