#Making synthetic data set
#Keaton Wilson
#keatonwilson@me.com
#2019-06-27

#packages
library(tidyverse)
library(synthpop)


#reading in real data
monarch_data = read_csv("./data/monarch_data_real.csv")
test = synthpop::syn(monarch_data, k = 1000)
