rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(MASS)

df_contact <- readRDS(here("data/df_contact.RDS"))

contact_unique_resp <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, respiratory, contact_age,
                  study_site, participant_age, participant_sex, 
                  age, occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n()) %>%
  filter(respiratory == 1) %>%
  tidyr::pivot_wider(., 
                     names_from = fromdayone, 
                     values_from=num_contacts)

contact_unique_resp$`Both Days`[which(is.na(contact_unique_resp$`Both Days`))] <- 0
contact_unique_resp$`Day1 Only`[which(is.na(contact_unique_resp$`Day1 Only`))] <- 0
contact_unique_resp$`Day2 Only`[which(is.na(contact_unique_resp$`Day2 Only`))] <- 0
contact_unique_resp$`NA`[which(is.na(contact_unique_resp$`NA`))] <- 0
contact_unique_resp$avg_unique_resp_contacts <- (round(contact_unique_resp$`Both Days` / 2)+
                                                   contact_unique_resp$`Day1 Only` + 
                                                   contact_unique_resp$`Day2 Only`+
                                                   contact_unique_resp$`NA`)/2

contact_daily_resp <- df_contact %>%
  dplyr::group_by(rec_id, study_day, participant_age, contact_age,
                  study_site, participant_sex, age, respiratory, 
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n()) %>%
  filter(respiratory == 1) %>%
  dplyr::group_by(rec_id, participant_age, contact_age,
                  participant_sex, age, study_site,
                  occupation, hh_occupants) %>%
  summarise(avg_daily_resp_contacts = (mean(num_contacts)))




