#Reproducible Example of occ with iNat failing at high limits
#Keaton Wilson
#keatonwilson@me.com
#2019-05-21

#fresh install of spocc (as per last fix suggested on this thread)
remotes::install_github("ropensci/spocc", force = TRUE)

#Restart your R session here

#loading spocc
library(spocc)

#Successful query with small limits
monarch_500 = occ("Danaus plexippus", from = "inat", limit = 3200)
monarch_500

#Can we pull the total number (53,066) - keep in mind, this takes a while.
monarch_full = occ("Danaus plexippus", from = "inat", limit = 53066)
monarch_full

#Nothing there - let's see if gbif works.
monarch_full_gbif = occ("Danaus plexippus", from = "gbif", limit = 50000)
monarch_full_gbif

#Quering GBIF seems to be functional - so it's an inat problem. 