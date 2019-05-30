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
monarch_bigger = occ("Danaus plexippus", from = "inat", limit = 18020)
monarch_bigger

#This is particularly strange, because it pulls less than the limit (limit = 18020, returned = 10041), but still works? What happens if we
#pull even more?
#
#
monarch_bigger_still = occ("Danaus plexippus", from = "inat", limit = 20000)
monarch_bigger_still

#And this is even weirder - now it pulls less than the total number, but slightly more than when the limit is set at 18020. 