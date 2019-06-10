#Feature generation for Monarch ML
#Keaton Wilson
#keatonwilson@me.com
#2019-06-05
#
#packages
library(tidyverse)
library(lubridate)
library(ggmap)
library(stringr)
library(purrr)
library(revgeo)

register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#Reading in the data
monarch = read_csv("./data/monarch_w_locs.csv")
monarch_winter = read_csv("./data/monarch_winter.csv")

#So the general idea here is to start to build the feature set to feed into a ML model. 
# Feature list: 
# 1. Date of first sighting in the US (previous year)
# 2. Number of sightings N of 37 º
# 3. Total # of sightings between March and October
# 4. Active time environmental conditions (March-October) (precip, avg temp, avg max temp)
# 5. Others (start with what is above)

#First step is setting up the dataframe that all of this will populate
monarch_ml_df = monarch_winter

#inspecting
print(monarch_ml_df, n=30)

#Changing dates to December dates - i.e. the year reflects the year in December, not in January when the records were collected. 
monarch_ml_df$year = monarch_ml_df$year - 1

#Finding the date of first sighting by year
monarch %>%
  mutate(year = year(date)) %>% 
  filter()
  group_by(year) %>%
  summarize(first_sighting = min(date))
