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

contact_unique_resp %>% filter(!is.na(contact_age))%>%
  filter(respiratory == 1) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_unique_resp_contacts = round(mean(avg_unique_resp_contacts),1)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill=avg_unique_resp_contacts)) +
  geom_raster() +
  geom_text(aes(participant_age, contact_age, label = avg_unique_resp_contacts),
            color = "black", size = 4) +
  theme_classic() +
  scale_fill_gradient2(low="#0571b0", mid="#92c5de", high="#ca0020", 
                       limits=c(0,3), breaks=(c(0,1,2,3))) +
  labs(x ="Participant age", 
       y = "Contact age",
       title = "Average Unique Respiratory Contacts",
       fill = "Average\ncontacts") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "right") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size= 10))

contact_daily_resp <- df_contact %>%
  dplyr::group_by(rec_id, study_day, participant_age, contact_age,
                  study_site, participant_sex, age, respiratory, 
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n()) %>%
  filter(respiratory == 1) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_daily_resp_contacts = round(mean(num_contacts),1))

contact_daily_resp %>% filter(!is.na(contact_age))%>%
  ggplot(aes(x = participant_age, y = contact_age, fill=avg_daily_resp_contacts)) +
  geom_raster() +
  geom_text(aes(participant_age, contact_age, label = avg_daily_resp_contacts),
            color = "black", size = 4) +
  theme_classic() +
  scale_fill_gradient2(low="#0571b0", mid="#92c5de", high="#ca0020", 
                       limits=c(0,5), breaks=(c(0,1,2,3,4,5))) +
  labs(x ="Participant age", 
       y = "Contact age",
       title = "Average Daily Respiratory Contacts",
       fill = "Average\ncontacts") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "right") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size= 10))

# Enteric ----------------------------------------------------------------------

contact_unique_ent <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, enteric, contact_age,
                  study_site, participant_age, participant_sex, 
                  age, occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n()) %>%
  filter(enteric == 1) %>%
  tidyr::pivot_wider(., 
                     names_from = fromdayone, 
                     values_from=num_contacts)

contact_unique_ent$`Both Days`[which(is.na(contact_unique_ent$`Both Days`))] <- 0
contact_unique_ent$`Day1 Only`[which(is.na(contact_unique_ent$`Day1 Only`))] <- 0
contact_unique_ent$`Day2 Only`[which(is.na(contact_unique_ent$`Day2 Only`))] <- 0
contact_unique_ent$`NA`[which(is.na(contact_unique_ent$`NA`))] <- 0
contact_unique_ent$avg_unique_ent_contacts <- (round(contact_unique_ent$`Both Days` / 2)+
                                                   contact_unique_ent$`Day1 Only` + 
                                                   contact_unique_ent$`Day2 Only`+
                                                   contact_unique_ent$`NA`)/2

contact_unique_ent %>% filter(!is.na(contact_age))%>%
  filter(enteric == 1) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_unique_ent_contacts = round(mean(avg_unique_ent_contacts),1)) %>%
  ggplot(aes(x = participant_age, y = contact_age, fill=avg_unique_ent_contacts)) +
  geom_raster() +
  geom_text(aes(participant_age, contact_age, label = avg_unique_ent_contacts),
            color = "black", size = 4) +
  theme_classic() +
  scale_fill_gradient2(low="#0571b0", mid="#92c5de", high="#ca0020", 
                       limits=c(0,3.1), breaks=(c(0,1,2,3))) +
  labs(x ="Participant age", 
       y = "Contact age",
       title = "Average Unique Enteric Contacts",
       fill = "Average\ncontacts") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "right") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size= 10))

contact_daily_ent <- df_contact %>%
  dplyr::group_by(rec_id, study_day, participant_age, contact_age,
                  study_site, participant_sex, age, enteric, 
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n()) %>%
  filter(enteric == 1) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_daily_ent_contacts = round(mean(num_contacts),1))

contact_daily_ent %>% filter(!is.na(contact_age))%>%
  ggplot(aes(x = participant_age, y = contact_age, fill=avg_daily_ent_contacts)) +
  geom_raster() +
  geom_text(aes(participant_age, contact_age, label = avg_daily_ent_contacts),
            color = "black", size = 4) +
  theme_classic() +
  scale_fill_gradient2(low="#0571b0", mid="#92c5de", high="#ca0020", 
                       limits=c(0,5), breaks=(c(0,1,2,3,4,5))) +
  labs(x ="Participant age", 
       y = "Contact age",
       title = "Average Daily Enteric Contacts",
       fill = "Average\ncontacts") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "right") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size= 10))

# Index-Q ----------------------------------------------------------------------

respiratory_matrix <- contact_unique_resp %>% filter(!is.na(contact_age))%>%
  filter(respiratory == 1) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_unique_resp_contacts = round(mean(avg_unique_resp_contacts),1)) %>%
  pivot_wider(names_from = c("participant_age"), values_from = avg_unique_resp_contacts) 

respiratory_matrix$contact_age <- NULL

rownames(respiratory_matrix) <- colnames(respiratory_matrix)


enteric_matrix <- contact_unique_ent %>% filter(!is.na(contact_age))%>%
  filter(enteric == 1) %>%
  dplyr::group_by(participant_age, contact_age) %>%
  summarise(avg_unique_ent_contacts = round(mean(avg_unique_ent_contacts),1)) %>%
  pivot_wider(names_from = c("participant_age"), values_from = avg_unique_ent_contacts) 

enteric_matrix$contact_age <- NULL

rownames(enteric_matrix) <- colnames(enteric_matrix)

index_q <- function(m){
  m = m/sum(m)
  colsum <- colSums(m)
  diagsum = 0
  colsumsq = 0
  for(i in 1:nrow(m)){
    diagsum <- diagsum + m[i,i]
    colsumsq <- colsumsq + (colsum[i]^2)
  }
  
  r <- (diagsum - colsumsq) / (1 - colsumsq)
  return(as.numeric(r))
}

sam_index_q <- function(m){
  m = m/sum(m)
  colsum <- colSums(m)
  diagsum = 0
  colsumsq = 0
  for(i in 1:nrow(m)){
    diagsum <- diagsum + m[i,i]
    colsumsq <- colsumsq + (colsum[i]^2)
  }
  
  r <- (diagsum - 1) / (nrow(m) - 1)
  return(as.numeric(r))
}

index_q(respiratory_matrix)

index_q(enteric_matrix)

sam_index_q(respiratory_matrix)

sam_index_q(enteric_matrix)
