#Occurence data from iNat and GBIF
#Keaton Wilson
#keatonwilson@me.com
#2019-05-08

#packages
library(tidyverse)
library(spocc)

#pulling species data
n_californica = occ("Nymphalis californica", from = c("inat", "gbif"), has_coords = TRUE, 
                    limit = 3000)
v_atalanta = occ("Vanessa atalanta", from = c("inat", "gbif"), has_coords = TRUE, 
                 limit = 790000)
v_annabella = occ("Vanessa annabella", from = c("inat", "gbif"), has_coords = TRUE, 
                  limit = 4000)

#Filtering out anything from inat that isn't research grade
n_californica$inat$data$`Nymphalis_californica*` = swallowtail$inat$data$`Nymphalis_californica*` %>%
  filter(quality_grade == "research")
v_atalanta$inat$data$`Vanessa_atalanta*` = v_atalanta$inat$data$`Vanessa_atalanta*` %>%
  filter(quality_grade == "research")
v_annabella$inat$data$`Vanessa_annabella*` = v_annabella$inat$data$`Vanessa_annabella*` %>%
  filter(quality_grade == "research")

#converting to df
n_californica = occ2df(n_californica)
v_atelanta = occ2df(v_atalanta)
v_annabella= occ2df(v_annabella)
