#Function for querying and cleaning data from inat and gbif using the spocc package
#Keaton Wilson
#keatonwilson@me.com
#2019-05-23

# Packages ----------------------------------------------------------------
library(tidyverse)
library(spocc)
library(ggmap)
library(mapr)
library(lubridate)
library(stringr)


# get_clean_obs function --------------------------------------------------

get_clean_obs = function(species = NULL) {
  occ_obs_1 = occ(species, from = c("gbif", "inat"), limit = 1, has_coords = TRUE) #querying with small number (1) to get total number of occurence records
  gbif_obs_num = occ_obs_1$gbif$meta$found #number of obs in gbif
  inat_obs_num = occ_obs_1$inat$meta$found #number of obs in inat
  
  occ_obs_full = occ(species, from = c("gbif", "inat"), #Getting the full set of records
                     has_coords = TRUE,
                     inatopts = list(limit = inat_obs_num),
                     gbifopts = list(limit = gbif_obs_num))
  
  occ_obs_full$inat$data[[1]] = occ_obs_full$inat$data[[1]] %>% #Filtering quality grade to research for inat data
    filter(quality_grade == "research")
  
  occ_df = occ2df(occ_obs_full) #Making a dataframe out of it
  
  occ_df = occ_df %>%
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude)) %>%
    filter(longitude > -165 & longitude < -45 & latitude > 15 & latitude < 70) %>% #Filtering for North America
    filter(str_detect(name, species)) %>% #Only including names that match original species name
    distinct() %>% #Filtering out duplicates between inat and GBIF
    as_tibble()
  
  print(occ_df %>%
          group_by(prov) %>%
          summarize(n()))
}

#Be wary of species name - difference here between the GBIF and iNat designations. If you don't get any results from both... something probably is amiss. i.e. WTF - why is GBIF calling quinqs "quinquemaculatus"
get_clean_obs(species = "Manduca quinquemaculata")

#An example of both
get_clean_obs(species = "Bison bison")


