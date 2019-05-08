#Occurence data from iNat and GBIF
#Keaton Wilson
#keatonwilson@me.com
#2019-05-08

#packages
library(tidyverse)
library(spocc)
library(ggmap)
library(mapr)
library(lubridate)

#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#pulling species data
n_californica = occ("Nymphalis californica", from = c("inat", "gbif"), has_coords = TRUE, 
                    limit = 3000)
v_atalanta_inat = occ("Vanessa atalanta", from = c("inat"), has_coords = TRUE, 
                 limit = 790000)
# v_atalanta_gbif = occ("Vanessa atalanta", from = c("gbif"), has_coords = TRUE, 
#                  limit = 790000)
#There is a shitload of these - I ended up downloading the gbif data separately. Will combine below. 
v_annabella = occ("Vanessa annabella", from = c("inat", "gbif"), has_coords = TRUE, 
                  limit = 4000)


#Filtering out anything from inat that isn't research grade
n_californica$inat$data$`Nymphalis_californica` = n_californica$inat$data$`Nymphalis_californica` %>%
  filter(quality_grade == "research")
v_atalanta$inat$data$`Vanessa_atalanta` = v_atalanta$inat$data$`Vanessa_atalanta` %>%
  filter(quality_grade == "research")
v_annabella$inat$data$`Vanessa_annabella` = v_annabella$inat$data$`Vanessa_annabella` %>%
  filter(quality_grade == "research")


#converting to df
n_californica = occ2df(n_californica)
v_atalanta = occ2df(v_atalanta)
v_annabella= occ2df(v_annabella)

#filtering for just the western US
n_californica_west = n_californica %>%
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>%
  filter(longitude < -100 & longitude > -125) %>%
  select(-key, -prov)

unique(n_californica_west$name)  

#More cleaning
n_californica_west = n_californica_west %>%
  filter(str_detect(name, 'Nymphalis californica')) %>%
  distinct()

#map
n_californica_west %>%
  map_ggmap()

#atalanta
#importing gbif megabase
v_atalanta_gbif = read_tsv("./data/v_atalanta_gbif.csv") 
v_atalanta = v_atalanta %>%
  select(species, latitude, longitude, date) %>%
  mutate(latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude)) %>%
  filter(longitude < -100 & longitude > -125)

#filtering and selecting
v_atalanta_master = v_atalanta_gbif %>%
  select(species, latitude = decimalLatitude, longitude = decimalLongitude, date = eventDate) %>%
  filter(longitude < -100 & longitude > -125) %>%
  mutate(date = date(date)) %>% 
  bind_rows(v_atalanta) %>%
  distinct() %>%
  rename(name = species)

#map
v_atalanta_master %>%
  map_ggmap()

  
#v annabella
v_annabella_west = v_annabella %>%
  select(name, longitude, latitude, date) %>%
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude)) %>%
  filter(longitude < -100 & longitude > -125) %>%
  filter(str_detect(name, "Vanessa annabella")) %>%
  distinct()

v_annabella_west %>%
  map_ggmap()

#Writing data to csv
write_csv(v_annabella_west, "./data/v_annabella_west.csv")
write_csv(v_atalanta_master, "./data/v_atalanta_west.csv")
write_csv(n_californica_west, "./data/n_californica_west.csv")

