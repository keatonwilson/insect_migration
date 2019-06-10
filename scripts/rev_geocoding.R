#Revgeocoding script
#Keaton Wilson
#keatonwilson@me.com
#2019-06-10


#packages
library(tidyverse)
library(lubridate)
library(ggmap)
library(stringr)
library(purrr)
library(revgeo)

register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#Reading in the data
monarch = read_csv("./data/monarch_gbif_inat_full.csv")
monarch_winter = read_csv("./data/monarch_winter.csv")

#Adding some geocode information to the monarch data
monarch_loc = monarch %>%
  select(longitude, latitude) 


#Feeding data to revgeo so avoid throttling and memory issues
locs = c()
start = Sys.time()
start_index = 1
start = Sys.time()

for (i in 1:nrow(monarch_loc)) {
  rec = monarch_loc[i,]
  latlon = c(rec$longitude, rec$latitude)
  loc = revgeocode(latlon)
  locs[i]  = loc
  Sys.sleep(sample(seq(0.5:1, by = 0.1), size = 1))
}
end = Sys.time()

#Damn - 21 hours

#binding it back on
monarch$locs = locs

#rebinding and saving
write_csv(monarch, "./data/monarch_w_locs.csv")