# load packages ---------------
 
library(tidyverse)
library(here)
library(skimr)
 
 # read in data -------------
 
beaches <- read.csv(here("data", "sydneybeaches.csv"))


# exploring the data --------------

View(beaches)

dim(beaches)

str(beaches)

glimpse(beaches)

head(beaches)

tail(beaches)

summary(beaches)


skim(beaches)
