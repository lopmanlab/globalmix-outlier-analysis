pacman::p_load(here,
               tidyverse,
               plotly)

if(!exists("country")){
  country = "Mozambique"
  cty = "moz"
  hhmbr = ""
}

participants <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_participant_data_aim1.RDS")) 
if(hhmbr == "Non-member"){
  contacts <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_contact_data_aim1.RDS")) %>%
    filter(hh_membership == hhmbr)
}else{
  contacts <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_contact_data_aim1.RDS"))
}

daily <- contacts %>%
  select(-study_site) %>%
  left_join(participants, ., by=c("rec_id" = "rec_id")) %>%
  mutate(duration = case_when(duration_contact == "<5 mins" ~ 0,
                              duration_contact == "5-15 mins" ~ 1,
                              duration_contact == "16-30 mins" ~ 2,
                              duration_contact == "31 mins-1 hr" ~ 3,
                              duration_contact == "1-4 hrs" ~ 4,
                              duration_contact  == ">4 hrs" ~ 5))

#Respiratory:
#All home contacts
#Indoor Contacts with duration "16-30 mins" or greater
#Outdoor Contacts with duration "1-4 hrs" or greater

#Enteric:
#All home contacts
#All contacts involving touch
#Outdoor-only contacts with no touch with duration of "31 mins-1 hr" or greater
daily <- daily %>%
  mutate(respiratory = ifelse(location == "Home", 1,
                              ifelse(where_contact != "Outdoors" & duration >= 2, 1, 
                                     ifelse(where_contact == "Outdoors" & duration >= 4, 1, 0)
                                     )
                              )
         ) %>%
  mutate(enteric = ifelse(location == "Home", 1,
                          ifelse(where_contact != "Outdoors" & touch_contact == "Yes", 1,
                                 ifelse(where_contact != "Outdoors" & touch_contact == "No" & duration >= 3, 1,
                                        ifelse(where_contact == "Outdoors" & touch_contact == "Yes", 1, 
                                               0 )
                                        )
                                 )
                          )
         )

contacts_resp <- daily %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, 
                  participant_sex, contact_age, respiratory, occupation,
                  hh_size_cat) %>%
  dplyr::summarize(num_contacts = n())

contact_daily_resp <- daily %>%
  ungroup() %>%
  group_by(rec_id,study_day) %>%
  dplyr::summarize(num_resp_contacts_byday = sum(respiratory)) %>%
  ungroup() %>%
  dplyr::group_by(rec_id) %>%
  summarise(avg_daily_resp_contacts = (mean(num_resp_contacts_byday))) %>%
  left_join(participants, ., by=c("rec_id" = "rec_id"))


# NOTE: Not doing unique right now
# contacts_resp <- daily %>%
#   dplyr::group_by(rec_id, fromdayone, respiratory, study_site, 
#                   participant_age, participant_sex, contact_age,
#                   occupation, hh_size_cat) %>%
#   dplyr::summarize(num_contacts = n())

# contacts_unique_resp <- tidyr::pivot_wider(contacts_resp %>% filter(respiratory == 1), 
#                                      names_from = fromdayone, values_from=num_contacts)
# contacts_unique_resp$`Both Days`[which(is.na(contacts_unique_resp$`Both Days`))] <- 0
# contacts_unique_resp$`Day1 Only`[which(is.na(contacts_unique_resp$`Day1 Only`))] <- 0
# contacts_unique_resp$`Day2 Only`[which(is.na(contacts_unique_resp$`Day2 Only`))] <- 0
# contacts_unique_resp$`NA`[which(is.na(contacts_unique_resp$`NA`))] <- 0
# contacts_unique_resp$avg_unique_resp_contacts <- (round(contacts_unique_resp$`Both Days` / 2)+
#                                                   contacts_unique_resp$`Day1 Only` + 
#                                                   contacts_unique_resp$`Day2 Only`+
#                                                   contacts_unique_resp$`NA`)/2


contacts_ent <- daily %>%
  dplyr::group_by(rec_id, study_site, study_day, participant_age, 
                  participant_sex, contact_age, enteric, occupation,
                  hh_size_cat) %>%
  dplyr::summarize(num_contacts = n())

contact_daily_ent <- contacts_ent %>%
  filter(enteric == 1) %>%
  dplyr::group_by(rec_id, study_site, participant_age, participant_sex, contact_age,
                  occupation, hh_size_cat) %>%
  summarise(avg_daily_ent_contacts = (mean(num_contacts)))

# NOTE: Not doing unique right now
# contacts_ent <- daily %>%
#   dplyr::group_by(rec_id, fromdayone, enteric, study_site, 
#                   participant_age, participant_sex, contact_age,
#                   occupation, hh_size_cat) %>%
#   dplyr::summarize(num_contacts = n())

# contacts_unique_ent <- tidyr::pivot_wider(contacts_ent %>% filter(enteric == 1), 
#                                      names_from = fromdayone, values_from=num_contacts)
# contacts_unique_ent$`Both Days`[which(is.na(contacts_unique_ent$`Both Days`))] <- 0
# contacts_unique_ent$`Day1 Only`[which(is.na(contacts_unique_ent$`Day1 Only`))] <- 0
# contacts_unique_ent$`Day2 Only`[which(is.na(contacts_unique_ent$`Day2 Only`))] <- 0
# contacts_unique_ent$`NA`[which(is.na(contacts_unique_ent$`NA`))] <- 0
# contacts_unique_ent$avg_unique_ent_contacts <- (round(contacts_unique_ent$`Both Days` / 2)+
#                                                 contacts_unique_ent$`Day1 Only` + 
#                                                 contacts_unique_ent$`Day2 Only`+
#                                                 contacts_unique_ent$`NA`)/2

# One main dataset for all outcomes ---------------------------------------------

contacts_resp_ent <- left_join(contact_daily_resp, 
                               contact_daily_ent %>% 
                                 ungroup() %>%
                                 dplyr::select(rec_id, avg_daily_ent_contacts),
                               by = c("rec_id" = "rec_id")) #%>%
  # left_join(., contacts_unique_resp %>% ungroup() %>%
  #             dplyr::select(rec_id, avg_unique_resp_contacts),
  #           by = c("rec_id" = "rec_id")) %>%
  # left_join(., contacts_unique_ent %>% ungroup() %>%
  #             dplyr::select(rec_id, avg_unique_ent_contacts),
  #           by = c("rec_id" = "rec_id"))

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
                   daily_ent_mean = mean(avg_daily_ent_contacts, na.rm=T))

contacts_resp_ent <- contacts_resp_ent %>%
  mutate(daily_resp_mean_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_mean, 1, 0),
         daily_resp_q75_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q75, 1, 0),
         daily_resp_q90_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q90, 1, 0),
         daily_ent_mean_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_mean, 1, 0),
         daily_ent_q75_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q75, 1, 0),
         daily_ent_q90_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q90, 1, 0))

# Write Data -------------
write.csv(contacts_resp_ent, paste0(here(),"/data/",cty,"/", cty, hhmbr, "_outlier_resp_ent.csv"))

