#Function for querying and cleaning data from inat and gbif using the spocc package
#Keaton Wilson
#keatonwilson@me.com
#2019-05-23
#
# Packages ----------------------------------------------------------------
library(tidyverse)
library(spocc)
library(ggmap)
library(mapr)
library(lubridate)
library(stringr)
library(rgbif)


# get_clean_obs function --------------------------------------------------

get_clean_obs = function(genus = NULL, species = NULL) {
  
  species_name = paste(genus, species, sep = " ") #Generating the full species name for searching
  occ_obs_1 = occ(species_name, from = c("gbif", "inat"), limit = 1, has_coords = TRUE) #querying with small number (1) to get total number of occurence records
  gbif_obs_num = occ_obs_1$gbif$meta$found #number of obs in gbif
  inat_obs_num = occ_obs_1$inat$meta$found #number of obs in inat
  
  #Conditional - we can do single runs if it's less than 10000, but otherwise need to combine
  if (gbif_obs_num < 10000 & inat_obs_num < 10000) { 
  
  ## FOR RECORDS WITH LESS THAN 10k RECORDS
  occ_obs_full = occ(species_name, from = c("gbif", "inat"), #Getting the full set of records
                     has_coords = TRUE,
                     inatopts = list(limit = inat_obs_num),
                     gbifopts = list(limit = gbif_obs_num))
  
  occ_obs_full$inat$data[[1]] = occ_obs_full$inat$data[[1]] %>% #Filtering quality grade to research for inat data
    filter(quality_grade == "research")
  
  occ_df = occ2df(occ_obs_full) #Making a dataframe out of it
  
  #Cleaning
  occ_df = occ_df %>%
    mutate(longitude = as.numeric(longitude),
           latitude = as.numeric(latitude)) %>%
    filter(longitude > -165 & longitude < -45 & latitude > 15 & latitude < 70) %>% #Filtering for North America
    filter(str_detect(name, species_name)) %>% #Only including names that match original species name 
    distinct(key) %>% #Filtering out duplicates between inat and GBIF
    as_tibble()
  
  #Print off number from each group
  print(occ_df %>%
          group_by(prov) %>%
          summarize(n()))
  
  ## MORE THAN 10k OBSERVATIONS BELOW
  } else {
  
    ## GBIF DATA
    gbif_occ = occ_search(scientificName = species_name, #getting gbif records from rgbif function
                          hasCoordinate = TRUE, 
                          continent = "north_america", 
                          limit = gbif_obs_num) 
    
    gbif_df = gbif_occ$data #pulling out just the data
    
    #Cleaning
    gbif_df = gbif_df %>%
      select(longitude = decimalLongitude, 
             latitude = decimalLatitude,
             name = scientificName,
             date = eventDate) %>%
      mutate(date = date(date)) %>%
      filter(longitude > -165 & longitude < -45 & latitude > 15 & latitude < 70) %>% #Filtering for North America
      filter(str_detect(name, species_name)) %>% #Only including names that match original species name 
      distinct(longitude, latitude, date, .keep_all = TRUE) %>% #Filtering out duplicates between inat and GBIF
      as_tibble()
    
    
    #iNat Data
    #The trick was to sequence through year by year. It reduces the pagination for each year, which gets around the hard limit of 10 pages (or 10k records)
    
    #Inat Loop
    inat_df <- NULL #Initializing the data frame
    year_sequence = 1975:(year(Sys.Date()))#Setting the years to sequence through
    
    #Start of the outer for loop to cycle through years
    for (i in year_sequence) {
      
      page = 1
      finished <- FALSE
      
      while (!finished & page < 100) { #Jeff's while loop (modified a bit)
        obs.url <- paste0("http://inaturalist.org/observations.csv?&taxon_name=", 
                          genus,
                          "%20",
                          species, 
                          "&page=",
                          page,
                          "&year=",
                          i,
                          "&quality_grade=research&has[]=geo&per_page=200") #200 max per page, only research grade
        
        temp.data <- read.csv(obs.url) #storing the output csv as a temporary data frame
        
        if (nrow(temp.data) > 0) {
          if (is.null(inat_df)) {
            inat_df <- temp.data
          } else {
            inat_df <- bind_rows(inat_df, temp.data) #bind the new data onto obs.data
          }
        } else {
          finished <- TRUE #Finish when there aren't any records in a data frame
        }
        page = page + 1 #Iterate to the next page
        
        if (nrow(temp.data) == 0) {
          print("This year has no records from iNaturalist", quote = FALSE) 
        } else {
          print("Working...", quote = FALSE)
        }
        rm(temp.data)
      }
      if (is.null(inat_df)){
        print(paste("Moving on to year", i+1), quote = FALSE)
      } else {
        print(paste("Your data set now has", inat_df %>%
                      n_distinct(), "distinct record(s)."), quote = FALSE)
        print(paste("Moving on to year", i), quote = FALSE)
      }
    }
    print(paste("Your data set now has", inat_df %>%
                  n_distinct(), "distinct record(s)"), quote = FALSE)
    
    print("Finished searching iNat", quote = FALSE)
    
    #Cleaning inat Data
    inat_df = inat_df %>%
      select(name = scientific_name,
             date = datetime, 
             latitude, 
             longitude, 
             key = id) %>%
      mutate(date = date(date)) %>%
      filter(longitude > -165 & longitude < -45 & latitude > 15 & latitude < 70) %>% #Filtering for North America
      filter(str_detect(name, species_name)) %>% #Only including names that match original species name 
      distinct(key, .keep_all = TRUE) %>%#Filtering out duplicates within inat
      select(-key) %>%
      as_tibble()
    
    #Combining GBIF and iNat into single dataframe
    occ_df = bind_rows(gbif_df, inat_df, .id = "prov")
    occ_df = occ_df %>%
      mutate(prov = factor(prov, labels = c("gbif", "inat"))) %>%
      distinct(date, latitude, longitude, .keep_all = TRUE) #Filtering out duplicates among the data set
    
    print(occ_df %>%
            group_by(prov) %>%
            summarize(n()))
  }
  #Printing summary
  print(paste("Search returned", nrow(occ_df), "records"), quotes = FALSE)
  occ_df
}


# Some warnings and examples ----------------------------------------------
# 
#Be wary of species name - difference here between the GBIF and iNat designations. If you don't get any results from both... something probably is amiss. i.e. WTF - why is GBIF calling quinqs "quinquemaculatus"
#
#Tests
#Low records
df_test = get_clean_obs(genus = "Manduca", species = "rustica")

##Bigger records
df_test_2 = get_clean_obs(genus = "Lithobates", species = "catesbeianus")




