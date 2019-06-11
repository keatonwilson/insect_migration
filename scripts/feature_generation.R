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
library(RNetCDF)
library(daymetr)

register_google(key = "AIzaSyDyAqUc4o9p_DOBSF_JOXH5c_JXPqoU4Yw")

#Reading in the data
monarch = read_csv("./data/monarch_w_locs.csv")
monarch_winter = read_csv("./data/monarch_winter.csv")

#quickmap
monarch %>%
  map_ggmap()

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

#extracting location info
monarch$country = str_extract(monarch$locs, '\\b[^,]+$')
unique(monarch$country)

na_country_list = c("USA", "Mexico", "Puerto Rico", "Canada", "United States")

monarch_country_sub = monarch %>%
  filter(country %in% na_country_list) %>%
  mutate(country = str_replace(country, "United States", "USA"), 
         country = str_replace(country, "Peurto Rico", "USA"))


#Finding the date of first sighting by year
first_us_sighting = monarch_country_sub %>%
  mutate(year = year(date)) %>% 
  filter(country == "USA") %>%
  group_by(year) %>%
  summarize(first_sighting = min(date))

#binding
monarch_ml_df = monarch_ml_df %>%
  left_join(first_us_sighting, by = "year") %>%
  mutate(day_first_sighting = lubridate::yday((first_sighting))) %>%
  select(-first_sighting)

#Feature 2 - Number of sightings north of 37º (Have this scaled to the number of sightings for a given year - trying to control for effort)
monarch_ml_df = monarch %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(n_obs_total = n()) %>%
  right_join(monarch_ml_df, by = "year") %>%
  left_join(monarch %>%
               mutate(year = year(date)) %>%
               filter(latitude > 37) %>%
               group_by(year) %>%
               summarize(n_obs_37 = n()), by = "year") %>%
  mutate(obs_37_norm = n_obs_37/n_obs_total) %>%
  select(year, hectares, day_first_sighting, obs_37_norm) %>%
  print(n = 50)

#Feature 3 - Total number of sightings between March and October (again, normalized for the total number of sightings per a given year)
monarch_ml_df = monarch %>%
  mutate(year = year(date), 
         month = month(date)) %>%
  filter(month %in% 3:10) %>%
  group_by(year) %>%
  summarize(active_months_obs = n()) %>% 
  right_join(monarch_ml_df, by = "year") %>%
  left_join(monarch %>%
              mutate(year = year(date)) %>%
              group_by(year) %>%
              summarize(n_obs_total = n()), by = "year") %>%
  mutate(active_months_obs_norm = active_months_obs/n_obs_total) %>%
  select(year, hectares, day_first_sighting, obs_37_norm, active_months_obs_norm, n_obs_total) %>%
  print(n = 50)

#Active environmental conditions (March-October)
 
#Need to get terraclim data

lon_min = -110
lon_max = -80
lat_min = 25
lat_max = 50

#Enter lat and lon ranges
lat.range=c(25, 50)        #! Ranges instead of point values. Order does not matter
lon.range=c(-110, -80)


#Pulling data
env_data = download_daymet_ncss(location = c(50, -110, 25, -80),
                                start = 1994,
                                end = 2018,
                                frequency = "monthly",
                                param = c("tmin","tmax", "prcp"),
                                path = tempdir(),
                                silent = TRUE)

