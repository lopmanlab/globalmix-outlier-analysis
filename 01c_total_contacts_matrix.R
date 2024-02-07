rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)
library(scales)
library(ggpubr)

# Read in Data -----------------------------------------------------------------

df_contact <- read.csv(here("data/df_contact.csv"))

# Create unique matrix ---------------------------------------------------------

contacts_unique <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, contact_age,
                  study_site, participant_age, participant_sex, 
                  age, occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n()) %>%
  tidyr::pivot_wider(., 
                     names_from = fromdayone, 
                     values_from=num_contacts)

contacts_unique$`Both Days`[which(is.na(contacts_unique$`Both Days`))] <- 0
contacts_unique$`Day1 Only`[which(is.na(contacts_unique$`Day1 Only`))] <- 0
contacts_unique$`Day2 Only`[which(is.na(contacts_unique$`Day2 Only`))] <- 0
contacts_unique$`NA`[which(is.na(contacts_unique$`NA`))] <- 0
contacts_unique$avg_unique_contacts <- (round(contacts_unique$`Both Days` / 2)+
                                          contacts_unique$`Day1 Only` + 
                                          contacts_unique$`Day2 Only`+
                                          contacts_unique$`NA`)/2

unique_matrix <- contacts_unique %>% filter(!is.na(contact_age))%>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_unique_contacts = round(mean(avg_unique_contacts),1)) %>%
  pivot_wider(names_from = c("participant_age"), values_from = avg_unique_contacts) 

unique_matrix$contact_age <- NULL

rownames(unique_matrix) <- colnames(unique_matrix)

write.csv(unique_matrix, "data/unique_matrix.csv")
