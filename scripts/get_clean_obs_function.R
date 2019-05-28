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
  
  if (gbif_obs_num < 10000 & inat_obs_num < 10000) { #Conditional - we can do single runs if it's less than 10000, but otherwise need to combine
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
    distinct(longitude, latitude, date, .keep_all = TRUE) %>% #Filtering out duplicates between inat and GBIF
    as_tibble()
  
  print(occ_df %>%
          group_by(prov) %>%
          summarize(n()))
  } else { #Chunk of code for if there are more than 10k observations
    biggest = max(gbif_obs_num, inat_obs_num) #determining the max number of records between gbif and inat
    number_of_runs = ceiling(biggest/10000) #figuring out how many interations we'll need to do
    
    occ_obs_list = list() #initalizing a list to hold all of the occ objects
    
    for (i in 1:number_of_runs) {
      start = 1 #defining starting record
      occ_obs_list[[i]] = occ(species, from = c("gbif", "inat"), #Getting the full set of records and binding to list
                              has_coords = TRUE,
                              limit = 10000,
                              start = start)
      start = start + 10000
    }
    
    print(occ_obs_list) #print off to see if the list is working
  }
}


# Some warnings and examples ----------------------------------------------
# 
#Be wary of species name - difference here between the GBIF and iNat designations. If you don't get any results from both... something probably is amiss. i.e. WTF - why is GBIF calling quinqs "quinquemaculatus"
mq = get_clean_obs(species = "Manduca quinquemaculata")

#An example of both
get_clean_obs(species = "Taricha granulosa")


get_clean_obs(species = "Lithobates catesbeianus")

#replicating the above manually to make sure it works
occ_obs_1 = occ("Lithobates catesbeianus", from = c("gbif", "inat"), limit = 1, has_coords = TRUE)
gbif_obs_num = occ_obs_1$gbif$meta$found #number of obs in gbif
inat_obs_num = occ_obs_1$inat$meta$found #number of obs in inat
biggest = max(gbif_obs_num, inat_obs_num) #determining the max number of records between gbif and inat
number_of_runs = ceiling(biggest/10000)

occ_obs_list = list() #initalizing a list to hold all of the occ objects
start = 1 #defining starting record

for (i in 1:number_of_runs) {
  occ_obs_list[[i]] = occ("Lithobates catesbeianus", from = c("gbif", "inat"), #Getting the full set of records and binding to list
                          has_coords = TRUE,
                          limit = 10000,
                          inatopts = list(page = start),
                          gbifopts = list(start = start))
  start = start + 10000
}

print(occ_obs_list)
df_1  = occ2df(occ_obs_list[[1]])
df_2 = occ2df(occ_obs_list[[2]])
df_3 = occ2df(occ_obs_list[[3]])

test_occ_2 = occ("Lithobates catesbeianus", from = c("gbif", "inat"), #Getting the full set of records and binding to list
               has_coords = TRUE,
               limit = 10000,
               start = 10001)

test_df_2 = occ2df(test_occ_2)

test_df_2 == df_2

big_df = bind_rows(df_1, df_2, df_3)

big_df %>%
  filter(prov == "inat") %>%
  distinct()

big_df %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  group_by(prov) %>%
  summarize(n())


one = occ("Danaus plexippus", from = "inat", limit = 5, page = 1)
two = occ("Danaus plexippus", from = "inat", limit = 5, start = 2)

one = occ2df(one)
two = occ2df(two)

one == two
