#Mini-script to get Danaus plexippus obs
#Keaton Wilson
#keatonwilson@me.com
#2019-06-04

#packages
library(tidyverse)
library(spocc)
library(ggmap)
library(mapr)
library(lubridate)
library(rgbif)

#Sourcing the get_clean_obs function 
source("./scripts/get_clean_obs_function.R")

#Searching monarch records (takes a long time)
monarch_full = get_clean_obs(genus = "Danaus", species = "plexippus")

#Writing to csv
write_csv(monarch_full, "./data/monarch_gbif_inat_full.csv")
