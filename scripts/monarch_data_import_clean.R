#Monarch Data Import
#Keaton Wilson
#keatonwilson@me.com
#2019-05-21
#
#remotes::install_github("ropensci/spocc", dependencies = TRUE, force = TRUE)

# Packages ----------------------------------------------------------------
library(tidyverse)
library(spocc)
library(ggmap)
library(mapr)
library(lubridate)

#register google api for mapping stuff
register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

# Data Import -------------------------------------------------------------
# Just GBIF for now - iNat is broken for grabbing this many records

occ_obs_1 = occ("Danaus plexippus", from = "gbif", limit = 1, has_coords = TRUE, gbifopts = list(country='US'))
gbif_obs_num = occ_obs_1$gbif$meta$found #number of obs in gbif
number_of_runs = ceiling(gbif_obs_num/10000)

monarch_gbif_list = list()
start = 0

for (i in 1:number_of_runs) {
  monarch_gbif_list[[i]] = occ("Danaus plexippus", from = c("gbif"), #Getting the full set of records and binding to list
                          has_coords = TRUE,
                          limit = 10000,
                          start = start, 
                          gbifopts = list(country = 'US'))
  start = start + 10000
  print(monarch_gbif_list[[i]])
}

#Ok, so we have a big list (20 items) that we need to turn into dataframes and bind together
monarch_gbif_list[[1]][[1]][[2]]

monarch_gbif_df = occ2df(monarch_gbif_list[[1]])
for (j in 2:length(monarch_gbif_list)) {
  df = occ2df(monarch_gbif_list[[j]])
  monarch_gbif_df = bind_rows(df, monarch_gbif_df)
}
# Cleaning and Filtering -------------------------------------------------

monarch_gbif_df = monarch_gbif_df %>%
  distinct(key, .keep_all = TRUE) %>%
  mutate(month = month(date), 
         year = year(date))
# Manually Adding Mexican Winter Hectare Data -----------------------------

#Might need to modify 2004 - apparently the data was collected early in the season, and may closer to 8-9 hectares. 
winter = data.frame(year = seq(from = 1995, to = 2019),
                    hectares = c(7.81, 12.61, 18.19, 
                                 5.77, 5.56, 9.05,
                                 2.83, 9.35, 7.54, 
                                 11.52, 2.19, 5.92, 
                                 6.87, 4.61, 5.06, 
                                 1.92, 4.02, 2.89, 
                                 1.19, 0.67, 1.13,
                                 4.01, 2.91, 2.48, 
                                 6.05))

#Saving the data
write_csv(winter, "./data/monarch_winter.csv")
write_csv(monarch_gbif_df, "./data/monarch_gbif.csv")
