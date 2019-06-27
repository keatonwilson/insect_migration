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
library(mapr)
library(revgeo)
library(RNetCDF)
library(daymetr)
library(ncdf4)
library(dismo)

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


#Finding the date of first sighting by year (filtering for texas - some weird sightings in CA)
first_us_sighting = monarch_country_sub %>%
  mutate(year = year(date)) %>% 
  filter(country == "USA") %>%
  filter(str_detect(locs, "TX|Texas|NM|New Mexico|OK|Oklahoma|AZ|Arizona")) %>%
  group_by(year) %>%
  summarize(first_sighting = min(date), 
            n = n()) %>%
  print(n = 50)

#binding
monarch_ml_df = monarch_ml_df %>%
  left_join(first_us_sighting, by = "year") %>%
  mutate(day_first_sighting = lubridate::yday((first_sighting))) %>%
  dplyr::select(-first_sighting)

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
  dplyr::select(year, hectares, day_first_sighting, obs_37_norm) %>%
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
  dplyr::select(year, hectares, day_first_sighting, obs_37_norm, active_months_obs_norm, n_obs_total) %>%
  print(n = 50)

#Active environmental conditions (March-October)
 


#Workflow:
#1. Pull monthly data for the region for each year. 
#2. Calculate biovars for each year (19)
#3. Delete massive ncss files
#3. Bind biovars onto the data

#Need to get terraclim data

lon_min = -110
lon_max = -80
lat_min = 25
lat_max = 50

#Enter lat and lon ranges
lat.range=c(25, 50)        #! Ranges instead of point values. Order does not matter
lon.range=c(-110, -80)

library(prism)

# get_prism_monthlys(type = "tmax", years = 1999:2019, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "tmin", years = 1994:2019, mon = 1:12, keepZip = FALSE)
# get_prism_monthlys(type = "ppt", years = 1994:2019, mon = 1:12, keepZip = FALSE)

#Combining lots of data into raster bricks (12 layers per year for each variables (ppt, tmin, tmax))
# biovar_list = list()
# year_seq = as.character(seq(from = 1994, to = 2018,  by = 1))
# 
# for(i in 1:length(year_seq)) {
#   year_char = year_seq[i]
#   ppt_names = ls_prism_data(absPath = TRUE) %>%
#   filter(str_detect((ls_prism_data(absPath = TRUE))[,2], year_char)) %>%
#   filter(str_detect(abs_path, "ppt"))
# 
#   tmax_names = ls_prism_data(absPath = TRUE) %>%
#   filter(str_detect((ls_prism_data(absPath = TRUE))[,2], year_char)) %>%
#   filter(str_detect(abs_path, "tmax"))
# 
#   tmin_names = ls_prism_data(absPath = TRUE) %>%
#   filter(str_detect((ls_prism_data(absPath = TRUE))[,2], year_char)) %>%
#   filter(str_detect(abs_path, "tmin"))
# 
#   ppt_list = list()
#   for (j in 1:length(ppt_names$abs_path)) {
#   ppt_list[[j]]= raster(ppt_names$abs_path[j])
#   }
# 
#   tmax_list = list()
#   for (j in 1:length(tmax_names$abs_path)) {
#   tmax_list[[j]]= raster(tmax_names$abs_path[j])
#   }
# 
#   tmin_list = list()
#   for (j in 1:length(tmin_names$abs_path)) {
#   tmin_list[[j]]= raster(tmin_names$abs_path[j])
#   }
# 
#   ppt_brick = brick(ppt_list)
#   tmin_brick = brick(tmin_list)
#   tmax_brick = brick(tmax_list)
# 
#   biovars = biovars(prec = ppt_brick, 
#                   tmin = tmin_brick,
#                   tmax = tmax_brick)
#   biovar_list[[i]] = biovars
# }
# 
# #Saving list of biovars for each year
# saveRDS(biovar_list, "./data/biovar.rds")
biovar_list = readRDS("./data/biovar.rds")
#Cropping - Feature #1 Total Area
lon_min = -110
lon_max = -80
lat_min = 25
lat_max = 50

#Enter lat and lon ranges
lat.range=c(25, 50)        #! Ranges instead of point values. Order does not matter
lon.range=c(-110, -80)

geographic.extent <- extent(x = c(lon_min, lon_max, lat_min, lat_max))

cropped_biovars = lapply(biovar_list, function(x) crop(x, geographic.extent))

#Making a list of data frames
cropped_biovars_df = lapply(cropped_biovars, as.data.frame)
means_list = lapply(cropped_biovars_df, function(x) apply(x, 2, function(x) mean(x, na.rm = TRUE)))

means_list = lapply(means_list, function(x) t(as.data.frame(x)))
names(means_list) = seq(from = 1994, to = 2018, by = 1)
test = lapply(means_list, as.data.frame)
summary_features = bind_rows(test, .id = 'year') %>%
  mutate(year = as.numeric(year))
names(summary_features) = c("year", 
                            "bio_1_whole_range", "bio_2_whole_range", 
                            "bio_3_whole_range", "bio_4_whole_range", 
                            "bio_5_whole_range", "bio_6_whole_range", 
                            "bio_7_whole_range", "bio_8_whole_range", 
                            "bio_9_whole_range", "bio_10_whole_range", 
                            "bio_11_whole_range", "bio_12_whole_range", 
                            "bio_13_whole_range", "bio_14_whole_range", 
                            "bio_15_whole_range", "bio_16_whole_range", 
                            "bio_17_whole_range", "bio_18_whole_range", 
                            "bio_19_whole_range")
monarch_ml_df = left_join(monarch_ml_df, summary_features, by = "year")

monarch_ml_df = monarch_ml_df %>%
  dplyr::select(year:n_obs_total, bio_1_whole_range:bio_19_whole_range)

#Bioclim for smaller northern range (abvoe 37º)

#Cropping - Feature #1 Total Area
lon_min = -110
lon_max = -80
lat_min = 37
lat_max = 50

#Enter lat and lon ranges
lat.range=c(37, 50)        #! Ranges instead of point values. Order does not matter
lon.range=c(-110, -80)

geographic.extent <- extent(x = c(lon_min, lon_max, lat_min, lat_max))

cropped_biovars = lapply(biovar_list, function(x) crop(x, geographic.extent))

#Making a list of data frames
cropped_biovars_df = lapply(cropped_biovars, as.data.frame)
means_list = lapply(cropped_biovars_df, function(x) apply(x, 2, function(x) mean(x, na.rm = TRUE)))

means_list = lapply(means_list, function(x) t(as.data.frame(x)))
names(means_list) = seq(from = 1994, to = 2018, by = 1)
test = lapply(means_list, as.data.frame)
summary_features = bind_rows(test, .id = 'year') %>%
  mutate(year = as.numeric(year))
names(summary_features) = c("year", 
                            "bio_1_nrange", "bio_2_nrange", 
                            "bio_3_nrange", "bio_4_nrange", 
                            "bio_5_nrange", "bio_6_nrange", 
                            "bio_7_nrange", "bio_8_nrange", 
                            "bio_9_nrange", "bio_10_nrange", 
                            "bio_11_nrange", "bio_12_nrange", 
                            "bio_13_nrange", "bio_14_nrange", 
                            "bio_15_nrange", "bio_16_nrange", 
                            "bio_17_nange", "bio_18_nrange", 
                            "bio_19_nrange")
monarch_ml_df = left_join(monarch_ml_df, summary_features, by = "year")

monarch_ml_df = monarch_ml_df %>%
  dplyr::select(year:n_obs_total, bio_1_whole_range:bio_19_whole_range)

#Saving data set
write_csv(monarch_ml_df, "./data/monarch_data_real.csv")
