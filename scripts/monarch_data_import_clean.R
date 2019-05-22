#Monarch Data Import
#Keaton Wilson
#keatonwilson@me.com
#2019-05-21
#
# remotes::install_github("ropensci/spocc", force = TRUE)

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
monarch_occ = occ("Danaus plexippus", from = c("gbif"), limit = 216465)

# Cleaning and Filtering -------------------------------------------------
# Filtering for Research Grade in inat
# monarch_occ$inat$data$`Danaus_plexippus` = monarch_occ$inat$data$`Danaus_plexippus` %>%
#   filter(quality_grade == "research")

# Turning into a data frame for further processing
monarch_df = oc2df(monarch_occ)

monarch_df %>%
  


# Manually Adding Mexican Winter Hectare Data -----------------------------

#Might need to modify 2004 - apparently the data was collected early in the season, and may closer to 8-9 hectares. 

winter = data.frame(year = seq(1995:2018),
                    hectares = c(7.81, 12.61, 18.19, 
                                 5.77, 5.56, 9.05,
                                 2.83, 9.35, 7.54, 
                                 11.52, 2.19, 5.92, 
                                 6.87, 4.61, 5.06, 
                                 1.92, 4.02, 2.89, 
                                 1.19, 0.67, 1.13,
                                 4.01, 2.91, 2.48, 
                                 6.05))


