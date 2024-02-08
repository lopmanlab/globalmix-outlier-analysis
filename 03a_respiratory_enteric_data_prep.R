rm(list=ls())
pacman::p_load(here,
               dplyr,
               ggplot2, 
               plotly, 
               lme4, 
               MASS,
               lmerTest)

participants <- read.csv(paste0(here(),"/data/raw/prasad_india_individual_21jan2024.csv"))
contacts <- read.csv(paste0(here(),"/data/raw/prasad_india_contact_21jan2024.csv"))
households <- read.csv(paste0(here(), "/data/raw/prasad_india_household_21jan2024.csv"))

## Subset contacts to the IDs in participant list only
contacts <- contacts %>%
  dplyr::filter(rec_id %in% unlist(participants$rec_id)) #%>%
  # mutate(fromdayone = ifelse(is.na(fromdayone), study_day, fromdayone))

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
  # dplyr::select(-study_site) %>%
  left_join(participants %>% mutate(age = round(age_months/12)), by=("rec_id")) %>%
  left_join( dplyr::select(households, "rec_id", "hh_occupants"), by=("rec_id"))

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
contacts_unique_resp$`Both days`[which(is.na(contacts_unique_resp$`Both days`))] <- 0
contacts_unique_resp$`Day 1`[which(is.na(contacts_unique_resp$`Day 1`))] <- 0
contacts_unique_resp$`Day 2`[which(is.na(contacts_unique_resp$`Day 2`))] <- 0
contacts_unique_resp$`NA`[which(is.na(contacts_unique_resp$`NA`))] <- 0
contacts_unique_resp$avg_unique_resp_contacts <- (round(contacts_unique_resp$`Both days` / 2)+
                                                  contacts_unique_resp$`Day 1` +
                                                  contacts_unique_resp$`Day 2`+
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
contacts_unique_ent$`Both days`[which(is.na(contacts_unique_ent$`Both days`))] <- 0
contacts_unique_ent$`Day 1`[which(is.na(contacts_unique_ent$`Day 1`))] <- 0
contacts_unique_ent$`Day 2`[which(is.na(contacts_unique_ent$`Day 2`))] <- 0
contacts_unique_ent$`NA`[which(is.na(contacts_unique_ent$`NA`))] <- 0
contacts_unique_ent$avg_unique_ent_contacts <- (round(contacts_unique_ent$`Both days` / 2)+
                                                contacts_unique_ent$`Day 1` +
                                                contacts_unique_ent$`Day 2`+
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
table(contacts_resp_ent$hh_occupants)
write.csv(table(contacts_resp_ent$occupation) %>% as.data.frame(), "data/occupation_freq.csv")

contacts_resp_ent$hhsize <- ifelse(contacts_resp_ent$hh_occupants > 6, "7+",
                                    ifelse(contacts_resp_ent$hh_occupants > 4, "5-6",
                                            ifelse(contacts_resp_ent$hh_occupants > 2, "3-4",
                                                    2)))
contacts_resp_ent$hhsize <- factor(contacts_resp_ent$hhsize, 
                                   levels = c("2", "3-4", "5-6", "7+"))

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

contacts_resp_ent <- contacts_resp_ent %>%
  mutate(p_age = case_when(
    participant_age == "60+y" ~ "60+y",
    age == 60 & participant_age == "40-59y" ~ "50-59y",
    age >= 50 & age <= 59 ~ "50-59y",
    participant_age == "30-39y" ~ "30-39y",
    age >= 40 & age <= 49 ~ "40-49y",
    participant_age == "20-29y" ~ "20-29y",
    participant_age %in% c("10-14y", "15-19y") ~ "10-19y",
    participant_age == "5-9y" ~ "5-9y",
    .default = "<4y")) %>%
  mutate(p_age = factor(p_age, c("<4y", "5-9y", "10-19y", "20-29y", "30-39y",
                                 "40-49y", "50-59y", "60+y")))


write.csv(contacts_resp_ent, here("data/contacts_resp_ent.csv"))
write.csv(contact_summaries, here("data/contact_summaries.csv"))
write.csv(df_contact, here("data/df_contact.csv"))


# Non-household contacts -------------------------------------------------------

contacts_resp <- df_contact %>%
  filter(hh_membership == "Non-member") %>%
  dplyr::group_by(rec_id, fromdayone, respiratory, study_site, 
                  participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contacts_unique_resp <- tidyr::pivot_wider(contacts_resp %>% filter(respiratory == 1), 
                                           names_from = fromdayone, values_from=num_contacts)
contacts_unique_resp$`Both days`[which(is.na(contacts_unique_resp$`Both days`))] <- 0
contacts_unique_resp$`Day 1`[which(is.na(contacts_unique_resp$`Day 1`))] <- 0
contacts_unique_resp$`Day 2`[which(is.na(contacts_unique_resp$`Day 2`))] <- 0
contacts_unique_resp$`NA`[which(is.na(contacts_unique_resp$`NA`))] <- 0
contacts_unique_resp$avg_unique_resp_contacts <- (round(contacts_unique_resp$`Both days` / 2)+
                                                    contacts_unique_resp$`Day 1` + 
                                                    contacts_unique_resp$`Day 2`+
                                                    contacts_unique_resp$`NA`)/2

contacts_ent <- df_contact %>%
  filter(hh_membership == "Non-member") %>%
  dplyr::group_by(rec_id, fromdayone, enteric, study_site, 
                  participant_age, participant_sex, age,
                  occupation, hh_occupants) %>%
  dplyr::summarize(num_contacts = n())

contacts_unique_ent <- tidyr::pivot_wider(contacts_ent %>% filter(enteric == 1), 
                                          names_from = fromdayone, values_from=num_contacts)
contacts_unique_ent$`Both days`[which(is.na(contacts_unique_ent$`Both days`))] <- 0
contacts_unique_ent$`Day 1`[which(is.na(contacts_unique_ent$`Day 1`))] <- 0
contacts_unique_ent$`Day 2`[which(is.na(contacts_unique_ent$`Day 2`))] <- 0
contacts_unique_ent$`NA`[which(is.na(contacts_unique_ent$`NA`))] <- 0
contacts_unique_ent$avg_unique_ent_contacts <- (round(contacts_unique_ent$`Both days` / 2)+
                                                  contacts_unique_ent$`Day 1` + 
                                                  contacts_unique_ent$`Day 2`+
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
                                    if_else(contacts_resp_ent$hh_occupants > 4, "5-6",
                                            if_else(contacts_resp_ent$hh_occupants > 2, "3-4", 
                                                   "1-2")))
contacts_resp_ent$hhsize <- factor(contacts_resp_ent$hhsize, 
                                   levels = c("1-2", "3-4", "5-6", "7+"))

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

write.csv(contacts_resp_ent, here("data/contacts_resp_ent_nonHHcontacts.csv"))
