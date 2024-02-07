rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

participants <- readRDS(paste0(here(),"/../","globalmix-mozambique/data/clean/participant_data_aim1.RDS"))
contacts <- readRDS(paste0(here(),"/../", "globalmix-mozambique/data/clean/contact_data_aim1.RDS"))
households <- readRDS(paste0(here(), "/../", "globalmix-mozambique/data/clean/household_survey_aim1.RDS"))

## Subset contacts to the IDs in participant list only
contacts <- contacts %>%
  dplyr::filter(rec_id %in% unlist(participants$rec_id))

# Filter contact relationship == self (you cannot have a contact with yourself)
contacts <- contacts %>%
  dplyr::filter(hh_member_relationship != "Self")

# relationship
contacts$hh_membership <- factor(contacts$hh_membership,
                                 levels = c("Member", "Non-member"))


# recategorize contact locations
contacts <- contacts %>%
  mutate(cnt_home = ifelse(location_contact___0==1, 1,0),
         cnt_school = ifelse(location_contact___2==1, 1,0),
         cnt_work = ifelse(location_contact___3==1, 1,0),
         cnt_otherplace = ifelse(location_contact___1==1 | 
                                   location_contact___4==1 | 
                                   location_contact___5==1 | 
                                   location_contact___6==1 | 
                                   location_contact___7==1 |
                                   location_contact___8==1 | 
                                   location_contact___9==1 |
                                   location_contact___10==1 | 
                                   location_contact___11==1,1,0))
# masking
contacts <- contacts %>%
  mutate(contact_mask2 = case_when(contact_mask == "Yes, for the entire encounter" ~ "Yes",
                                   contact_mask == "Yes, during parts of encounter" ~ "Yes",
                                   contact_mask == "No mask was worn during the encounter" ~ "No",
                                   TRUE ~ "Can't recall"))
contacts$contact_mask2 <- factor(contacts$contact_mask2, levels = c("Yes", "No",
                                                                    "Can't recall"))

contacts$cnt_home <- factor(contacts$cnt_home, levels = c(1, 0),
                            labels = c("Yes", "No"))
contacts$cnt_work <- factor(contacts$cnt_work, levels = c(1, 0),
                            labels = c("Yes", "No"))
contacts$cnt_school <- factor(contacts$cnt_school, levels = c(1, 0),
                              labels = c("Yes", "No"))
contacts$cnt_otherplace <- factor(contacts$cnt_otherplace, levels = c(1, 0),
                                  labels = c("Yes", "No"))

df_contact <- contacts %>%
  dplyr::select(-study_site) %>%
  left_join(participants, by=("rec_id")) %>%
  left_join( dplyr::select(households, "rec_id", "hh_occupants"), by=("rec_id"))


png("figs/contacts_duration.png", height = 750, width = 1250, res = 200)
ggplot(df_contact %>% 
         drop_na(participant_age, duration_contact) %>%
         mutate(duration_contact = factor(duration_contact, levels=c("<5 mins", "5-15 mins", "16-30 mins", "31 mins-1 hr", "1-4 hrs", ">4 hrs"))), 
       aes(fill=duration_contact, y=participant_age)) + 
  geom_bar(position="fill", stat="count")
dev.off()

df_contact <- df_contact %>%
  mutate(duration = case_when(duration_contact == "<5 mins" ~ 0,
                              duration_contact == "5-15 mins" ~ 1,
                              duration_contact == "16-30 mins" ~ 2,
                              duration_contact == "31 mins-1 hr" ~ 3,
                              duration_contact == "1-4 hrs" ~ 4,
                              duration_contact  == ">4 hrs" ~ 5))
df_contact <- df_contact %>%
  mutate(respiratory = ifelse(cnt_home == "Yes", 1,
                              ifelse(where_contact != "Outdoors" & duration >= 2, 1, 
                                     ifelse(where_contact == "Outdoors" & duration >= 4, 1, 0)
                                     )
                              )
         ) %>%
  mutate(enteric = ifelse(cnt_home == "Yes", 1,
                          ifelse(where_contact != "Outdoors" & touch_contact == "Yes", 1,
                                 ifelse(where_contact != "Outdoors" & touch_contact == "No" & duration >= 3, 1,
                                        ifelse(where_contact == "Outdoors" & touch_contact == "Yes", 1, 
                                               0 )
                                        )
                                 )
                          )
         )


png("figs/duration_by_where.png", height = 500, width = 1250, res = 200)
ggplot(df_contact %>% 
         drop_na(where_contact, duration_contact) %>%
         mutate(duration_contact = factor(duration_contact, levels=c("<5 mins", "5-15 mins", "16-30 mins", "31 mins-1 hr", "1-4 hrs", ">4 hrs"))), 
       aes(fill=duration_contact, y=where_contact)) + 
  geom_bar(position="fill", stat="count")
dev.off()

png("figs/duration_by_household.png", height = 500, width = 1250, res = 200)
ggplot(df_contact %>% 
         drop_na(cnt_home, duration_contact) %>%
         mutate(duration_contact = factor(duration_contact, levels=c("<5 mins", "5-15 mins", "16-30 mins", "31 mins-1 hr", "1-4 hrs", ">4 hrs"))), 
       aes(fill=duration_contact, y=cnt_home)) + 
  geom_bar(position="fill", stat="count")
dev.off()

contacts_resp <- df_contact %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, 
                  participant_sex, age, respiratory, occupation,
                  hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contact_daily_resp <- contacts_resp %>%
  filter(respiratory == 1) %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, age, 
                  occupation, hh_occupants) %>%
  summarise(avg_daily_resp_contacts = (mean(num_contacts)))

contacts_resp <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, respiratory, study_site, 
                  participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contacts_unique_resp <- tidyr::pivot_wider(contacts_resp %>% filter(respiratory == 1), 
                                     names_from = fromdayone, values_from=num_contacts)
contacts_unique_resp$`Both Days`[which(is.na(contacts_unique_resp$`Both Days`))] <- 0
contacts_unique_resp$`Day1 Only`[which(is.na(contacts_unique_resp$`Day1 Only`))] <- 0
contacts_unique_resp$`Day2 Only`[which(is.na(contacts_unique_resp$`Day2 Only`))] <- 0
contacts_unique_resp$`NA`[which(is.na(contacts_unique_resp$`NA`))] <- 0
contacts_unique_resp$avg_unique_resp_contacts <- (round(contacts_unique_resp$`Both Days` / 2)+
                                                  contacts_unique_resp$`Day1 Only` + 
                                                  contacts_unique_resp$`Day2 Only`+
                                                  contacts_unique_resp$`NA`)/2


contacts_ent <- df_contact %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, 
                  participant_sex, age, enteric, occupation,
                  hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contact_daily_ent <- contacts_ent %>%
  filter(enteric == 1) %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  summarise(avg_daily_ent_contacts = (mean(num_contacts)))

contacts_ent <- df_contact %>%
  dplyr::group_by(rec_id, fromdayone, enteric, study_site, 
                  participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contacts_unique_ent <- tidyr::pivot_wider(contacts_ent %>% filter(enteric == 1), 
                                     names_from = fromdayone, values_from=num_contacts)
contacts_unique_ent$`Both Days`[which(is.na(contacts_unique_ent$`Both Days`))] <- 0
contacts_unique_ent$`Day1 Only`[which(is.na(contacts_unique_ent$`Day1 Only`))] <- 0
contacts_unique_ent$`Day2 Only`[which(is.na(contacts_unique_ent$`Day2 Only`))] <- 0
contacts_unique_ent$`NA`[which(is.na(contacts_unique_ent$`NA`))] <- 0
contacts_unique_ent$avg_unique_ent_contacts <- (round(contacts_unique_ent$`Both Days` / 2)+
                                                contacts_unique_ent$`Day1 Only` + 
                                                contacts_unique_ent$`Day2 Only`+
                                                contacts_unique_ent$`NA`)/2

# One main dataset for all outcomes ---------------------------------------------

contacts_resp_ent <- left_join(contact_daily_resp, 
                               contact_daily_ent %>% 
                                 ungroup() %>%
                                 dplyr::select(rec_id, avg_daily_ent_contacts),
                               by = c("rec_id" = "rec_id")) %>%
  left_join(., contacts_unique_resp %>% ungroup() %>%
              dplyr::select(rec_id, avg_unique_resp_contacts),
            by = c("rec_id" = "rec_id")) %>%
  left_join(., contacts_unique_ent %>% ungroup() %>%
              dplyr::select(rec_id, avg_unique_ent_contacts),
            by = c("rec_id" = "rec_id"))

# Outlier variables ------------------------------------------------------------
contact_summaries <- contacts_resp_ent %>%
  distinct() %>%
  ungroup() %>%
  dplyr::summarize(daily_resp_q50 = quantile(avg_daily_resp_contacts, probs = 0.50, na.rm=T),
                   daily_resp_q75 = quantile(avg_daily_resp_contacts, probs = 0.75, na.rm=T),
                   daily_resp_q90 = quantile(avg_daily_resp_contacts, probs = 0.90, na.rm=T),
                   daily_resp_mean = mean(avg_daily_resp_contacts, na.rm=T),
                   daily_ent_q50 = quantile(avg_daily_ent_contacts, probs = 0.50, na.rm=T),
                   daily_ent_q75 = quantile(avg_daily_ent_contacts, probs = 0.75, na.rm=T),
                   daily_ent_q90 = quantile(avg_daily_ent_contacts, probs = 0.90, na.rm=T),
                   daily_ent_mean = mean(avg_daily_ent_contacts, na.rm=T),
                   unique_resp_q50 = quantile(avg_unique_resp_contacts, probs = 0.50, na.rm=T),
                   unique_resp_q75 = quantile(avg_unique_resp_contacts, probs = 0.75, na.rm=T),
                   unique_resp_q90 = quantile(avg_unique_resp_contacts, probs = 0.90, na.rm=T),
                   unique_resp_mean = mean(avg_unique_resp_contacts, na.rm=T),
                   unique_ent_q50 = quantile(avg_unique_ent_contacts, probs = 0.50, na.rm=T),
                   unique_ent_q75 = quantile(avg_unique_ent_contacts, probs = 0.75, na.rm=T),
                   unique_ent_q90 = quantile(avg_unique_ent_contacts, probs = 0.90, na.rm=T),
                   unique_ent_mean = mean(avg_unique_ent_contacts, na.rm=T))

contacts_resp_ent <- contacts_resp_ent %>%
  mutate(daily_resp_mean_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_mean, 1, 0),
         daily_resp_q75_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q75, 1, 0),
         daily_resp_q90_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q90, 1, 0),
         daily_ent_mean_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_mean, 1, 0),
         daily_ent_q75_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q75, 1, 0),
         daily_ent_q90_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q90, 1, 0),
         unique_resp_mean_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_mean, 1, 0),
         unique_resp_q75_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_q75, 1, 0),
         unique_resp_q90_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_q90, 1, 0),
         unique_ent_mean_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_mean, 1, 0),
         unique_ent_q75_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_q75, 1, 0),
         unique_ent_q90_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_q90, 1, 0))

# Clean up hh_occupants and occupation -----------------------------------------
# table(contacts_resp_ent$age, contacts_resp_ent$occupation, useNA = "always")
# table(contacts_resp_ent$age, is.na(contacts_resp_ent$hh_occupants))
# those with occupation = NA are all <= 7 years old

contacts_resp_ent$occupation <- if_else(is.na(contacts_resp_ent$occupation),
                                        "Child", contacts_resp_ent$occupation)

contacts_resp_ent$hh_occupants <- as.numeric(contacts_resp_ent$hh_occupants)

df_contact$occupation <- if_else(is.na(df_contact$occupation),
                                        "Child", df_contact$occupation)

df_contact$hh_occupants <- as.numeric(df_contact$hh_occupants)


contacts_resp_ent$occupation <- factor(contacts_resp_ent$occupation,
                                       levels = c("Unemployed", "Child", 
                                                  "Student", "Farmer", 
                                                  "Business person", "Office worker", 
                                                  "Casual laboror", "Fisherman", 
                                                  "Homemaker", "Retired",
                                                  "Other"))

hist(contacts_resp_ent$hh_occupants)
write.csv(table(contacts_resp_ent$occupation) %>% as.data.frame(), "data/occupation_freq.csv")

contacts_resp_ent$hhsize <- if_else(contacts_resp_ent$hh_occupants > 6, "7+",
                                    if_else(contacts_resp_ent$hh_occupants > 3, "4-6",
                                            "0-3"))
contacts_resp_ent$hhsize <- factor(contacts_resp_ent$hhsize, 
                                   levels = c("0-3", "4-6", "7+"))

barplot(prop.table(table(contacts_resp_ent$hhsize)))

contacts_resp_ent$age = as.numeric(contacts_resp_ent$age)
adults_contacts_resp_ent <- contacts_resp_ent %>% filter(age >= 20, participant_age != "<6mo")
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Child", "Fisherman"))] <- "Other"
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Retired"))] <- "Unemployed"

table(adults_contacts_resp_ent$occupation)
table(adults_contacts_resp_ent$participant_age)

contacts_resp_ent %>% 
  group_by(hhsize) %>% 
  summarise(medianr = median(avg_unique_resp_contacts, na.rm = T),
            meanr = mean(avg_unique_resp_contacts, na.rm = T),
            mediane = median(avg_unique_ent_contacts, na.rm = T),
            meane = mean(avg_unique_ent_contacts, na.rm = T))

adults_contacts_resp_ent %>% 
  group_by(occupation) %>% 
  summarise(medianr = median(avg_unique_resp_contacts, na.rm = T),
            meanr = mean(avg_unique_resp_contacts, na.rm = T),
            mediane = median(avg_unique_ent_contacts, na.rm = T),
            meane = mean(avg_unique_ent_contacts, na.rm = T))

ggplot(data = contacts_resp_ent)+
  geom_histogram(aes(avg_unique_resp_contacts))+
  geom_vline(aes(xintercept = contact_summaries$unique_resp_q75), color = "blue")+
  geom_vline(aes(xintercept = contact_summaries$unique_resp_q90), color = "red")+
  ggtitle("Unique Respiratory, Blue=Q75 and Red=Q90")

ggplot(data = contacts_resp_ent)+
  geom_histogram(aes(avg_unique_ent_contacts))+
  geom_vline(aes(xintercept = contact_summaries$unique_ent_q75), color = "blue")+
  geom_vline(aes(xintercept = contact_summaries$unique_ent_q90), color = "red")+
  ggtitle("Unique Enteric, Blue=Q75 and Red=Q90")


saveRDS(contacts_resp_ent, here("data/contacts_resp_ent.RDS"))
saveRDS(contact_summaries, here("data/contact_summaries.RDS"))
saveRDS(df_contact, here("data/df_contact.RDS"))


# Non-household contacts -------------------------------------------------------

contacts_resp <- df_contact %>%
  filter(hh_membership == "Non-member") %>%
  dplyr::group_by(rec_id, fromdayone, respiratory, study_site, 
                  participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contacts_unique_resp <- tidyr::pivot_wider(contacts_resp %>% filter(respiratory == 1), 
                                           names_from = fromdayone, values_from=num_contacts)
contacts_unique_resp$`Both Days`[which(is.na(contacts_unique_resp$`Both Days`))] <- 0
contacts_unique_resp$`Day1 Only`[which(is.na(contacts_unique_resp$`Day1 Only`))] <- 0
contacts_unique_resp$`Day2 Only`[which(is.na(contacts_unique_resp$`Day2 Only`))] <- 0
contacts_unique_resp$`NA`[which(is.na(contacts_unique_resp$`NA`))] <- 0
contacts_unique_resp$avg_unique_resp_contacts <- (round(contacts_unique_resp$`Both Days` / 2)+
                                                    contacts_unique_resp$`Day1 Only` + 
                                                    contacts_unique_resp$`Day2 Only`+
                                                    contacts_unique_resp$`NA`)/2

contacts_ent <- df_contact %>%
  filter(hh_membership == "Non-member") %>%
  dplyr::group_by(rec_id, fromdayone, enteric, study_site, 
                  participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contacts_unique_ent <- tidyr::pivot_wider(contacts_ent %>% filter(enteric == 1), 
                                          names_from = fromdayone, values_from=num_contacts)
contacts_unique_ent$`Both Days`[which(is.na(contacts_unique_ent$`Both Days`))] <- 0
contacts_unique_ent$`Day1 Only`[which(is.na(contacts_unique_ent$`Day1 Only`))] <- 0
contacts_unique_ent$`Day2 Only`[which(is.na(contacts_unique_ent$`Day2 Only`))] <- 0
contacts_unique_ent$`NA`[which(is.na(contacts_unique_ent$`NA`))] <- 0
contacts_unique_ent$avg_unique_ent_contacts <- (round(contacts_unique_ent$`Both Days` / 2)+
                                                  contacts_unique_ent$`Day1 Only` + 
                                                  contacts_unique_ent$`Day2 Only`+
                                                  contacts_unique_ent$`NA`)/2
# One main dataset for all outcomes

contacts_resp_ent <- left_join(contact_daily_resp, 
                               contact_daily_ent %>% 
                                 ungroup() %>%
                                 dplyr::select(rec_id, avg_daily_ent_contacts),
                               by = c("rec_id" = "rec_id")) %>%
  left_join(., contacts_unique_resp %>% ungroup() %>%
              dplyr::select(rec_id, avg_unique_resp_contacts),
            by = c("rec_id" = "rec_id")) %>%
  left_join(., contacts_unique_ent %>% ungroup() %>%
              dplyr::select(rec_id, avg_unique_ent_contacts),
            by = c("rec_id" = "rec_id"))

# Outlier variables
contact_summaries <- contacts_resp_ent %>%
  distinct() %>%
  ungroup() %>%
  dplyr::summarize(daily_resp_q50 = quantile(avg_daily_resp_contacts, probs = 0.50, na.rm=T),
                   daily_resp_q75 = quantile(avg_daily_resp_contacts, probs = 0.75, na.rm=T),
                   daily_resp_q90 = quantile(avg_daily_resp_contacts, probs = 0.90, na.rm=T),
                   daily_resp_mean = mean(avg_daily_resp_contacts, na.rm=T),
                   daily_ent_q50 = quantile(avg_daily_ent_contacts, probs = 0.50, na.rm=T),
                   daily_ent_q75 = quantile(avg_daily_ent_contacts, probs = 0.75, na.rm=T),
                   daily_ent_q90 = quantile(avg_daily_ent_contacts, probs = 0.90, na.rm=T),
                   daily_ent_mean = mean(avg_daily_ent_contacts, na.rm=T),
                   unique_resp_q50 = quantile(avg_unique_resp_contacts, probs = 0.50, na.rm=T),
                   unique_resp_q75 = quantile(avg_unique_resp_contacts, probs = 0.75, na.rm=T),
                   unique_resp_q90 = quantile(avg_unique_resp_contacts, probs = 0.90, na.rm=T),
                   unique_resp_mean = mean(avg_unique_resp_contacts, na.rm=T),
                   unique_ent_q50 = quantile(avg_unique_ent_contacts, probs = 0.50, na.rm=T),
                   unique_ent_q75 = quantile(avg_unique_ent_contacts, probs = 0.75, na.rm=T),
                   unique_ent_q90 = quantile(avg_unique_ent_contacts, probs = 0.90, na.rm=T),
                   unique_ent_mean = mean(avg_unique_ent_contacts, na.rm=T))

contacts_resp_ent <- contacts_resp_ent %>%
  mutate(daily_resp_mean_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_mean, 1, 0),
         daily_resp_q75_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q75, 1, 0),
         daily_resp_q90_outlier = 
           ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q90, 1, 0),
         daily_ent_mean_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_mean, 1, 0),
         daily_ent_q75_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q75, 1, 0),
         daily_ent_q90_outlier = 
           ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q90, 1, 0),
         unique_resp_mean_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_mean, 1, 0),
         unique_resp_q75_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_q75, 1, 0),
         unique_resp_q90_outlier = 
           ifelse(avg_unique_resp_contacts > contact_summaries$unique_resp_q90, 1, 0),
         unique_ent_mean_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_mean, 1, 0),
         unique_ent_q75_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_q75, 1, 0),
         unique_ent_q90_outlier = 
           ifelse(avg_unique_ent_contacts > contact_summaries$unique_ent_q90, 1, 0))

# Clean up hh_occupants and occupation 
contacts_resp_ent$occupation <- if_else(is.na(contacts_resp_ent$occupation),
                                        "Child", contacts_resp_ent$occupation)

contacts_resp_ent$hh_occupants <- as.numeric(contacts_resp_ent$hh_occupants)

df_contact$occupation <- if_else(is.na(df_contact$occupation),
                                 "Child", df_contact$occupation)

df_contact$hh_occupants <- as.numeric(df_contact$hh_occupants)


contacts_resp_ent$occupation <- factor(contacts_resp_ent$occupation,
                                       levels = c("Unemployed", "Child", 
                                                  "Student", "Farmer", 
                                                  "Business person", "Office worker", 
                                                  "Casual laboror", "Fisherman", 
                                                  "Homemaker", "Retired",
                                                  "Other"))

hist(contacts_resp_ent$hh_occupants)
# write.csv(table(contacts_resp_ent$occupation) %>% as.data.frame(), "data/occupation_freq.csv")

contacts_resp_ent$hhsize <- if_else(contacts_resp_ent$hh_occupants > 6, "7+",
                                    if_else(contacts_resp_ent$hh_occupants > 3, "4-6",
                                            "0-3"))
contacts_resp_ent$hhsize <- factor(contacts_resp_ent$hhsize, 
                                   levels = c("0-3", "4-6", "7+"))

barplot(prop.table(table(contacts_resp_ent$hhsize)))

contacts_resp_ent$age = as.numeric(contacts_resp_ent$age)
adults_contacts_resp_ent <- contacts_resp_ent %>% filter(age >= 20, participant_age != "<6mo")
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Child", "Fisherman"))] <- "Other"
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Retired"))] <- "Unemployed"

table(adults_contacts_resp_ent$occupation)
table(adults_contacts_resp_ent$participant_age)

contacts_resp_ent %>% 
  group_by(hhsize) %>% 
  summarise(medianr = median(avg_unique_resp_contacts, na.rm = T),
            meanr = mean(avg_unique_resp_contacts, na.rm = T),
            mediane = median(avg_unique_ent_contacts, na.rm = T),
            meane = mean(avg_unique_ent_contacts, na.rm = T))

adults_contacts_resp_ent %>% 
  group_by(occupation) %>% 
  summarise(medianr = median(avg_unique_resp_contacts, na.rm = T),
            meanr = mean(avg_unique_resp_contacts, na.rm = T),
            mediane = median(avg_unique_ent_contacts, na.rm = T),
            meane = mean(avg_unique_ent_contacts, na.rm = T))

saveRDS(contacts_resp_ent, here("data/contacts_resp_ent_nonHHcontacts.RDS"))
