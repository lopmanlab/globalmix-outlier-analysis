####################################################
#Exploring Place Use Data for Airborne Transmission#
####################################################

rm(list=ls())
pacman::p_load(here,
               tidyverse,
               scales)


# Reading and cleaning data ----------------------------------------------------

placeuse_raw <- readRDS(paste0(here(),"/../","globalmix-mozambique/data/clean/locations_visited_aim1.RDS"))

table(placeuse_raw$place_visited, placeuse_raw$num_pax_place)

