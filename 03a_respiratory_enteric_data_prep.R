pacman::p_load(here,
               tidyverse,
               plotly)

prepare_resp_ent_outlier_datasets <- function(cty = "moz", hhmbr = ""){
  
  full_names <- c("Mozambique", "India", "Pakistan", "Guatemala")
  names(full_names) <- c("moz", "ind", "pak", "gt")
  country = full_names[cty][1]
  
  participants <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_participant_data_aim1.RDS")) %>%
    mutate(occupation = case_when(is.na(occupation) ~ "Unemployed outside home",
                                  occupation == "Other" ~ "Unemployed outside home",
                                  .default = occupation)) %>%
    mutate(hh_size_cat = case_when(#hh_size >= 8 ~ "(7,50]",
                                   hh_size_cat == "(10,50]" ~ "(5,50]",
                                   hh_size_cat == "(5,10]" ~ "(5,50]",
                                   .default = hh_size_cat)) %>%
    mutate(hh_size_cat = factor(hh_size_cat, levels = c("[0,2]", "(2,5]", "(5,50]")))#"(5,7]", "(7,50]")))
  if(hhmbr == "Non-member"){
    contacts <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_contact_data_aim1.RDS")) %>%
      filter(hh_membership == hhmbr) 
  }else{
    contacts <- readRDS(paste0(here(),"/../","Globalmix/",country,"/", cty,"_contact_data_aim1.RDS"))
  }
  
  daily <- contacts %>%
    dplyr::select(-study_site) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id")) %>%
    mutate(duration = case_when(duration_contact == "<5 mins" ~ 0,
                                duration_contact == "5-15 mins" ~ 1,
                                duration_contact == "16-30 mins" ~ 2,
                                duration_contact == "31 mins-1 hr" ~ 3,
                                duration_contact == "1-4 hrs" ~ 4,
                                duration_contact  == ">4 hrs" ~ 5)) %>%
    mutate(occupation = case_when(is.na(occupation) ~ "Unemployed outside home",
                                  occupation == "Other" ~ "Unemployed outside home",
                                  .default = occupation)) %>%
    mutate(hh_size_cat = case_when(#hh_size >= 9 ~ "(7,50]",
                                   hh_size_cat == "(10,50]" ~ "(5,50]",
                                   hh_size_cat == "(5,10]" ~ "(5,50]",
                                   .default = hh_size_cat)) %>%
    mutate(hh_size_cat = factor(hh_size_cat, levels = c("[0,2]", "(2,5]", "(5,50]")))#"(5,7]", "(7,50]")))
  
  daily <- daily %>%
    #Respiratory - direct deposition:
    #All home contacts with duration "16-30 mins" or greater
    #Indoor Contacts with duration "16-30 mins" or greater
    #Outdoor Contacts with duration "1-4 hrs" or greater
    mutate(respiratory = case_when(location == "Home" ~ 1,
                                   where_contact != "Outdoors" & duration >= 2 ~ 1,
                                   where_contact == "Outdoors" & duration >= 4 ~ 1, 
                                   .default = 0)
           ) %>%
    #Respiratory - airborne:
    #All home contacts
    #Indoor Contacts with duration "1-4 hrs"
    #No outdoor contacts
    mutate(airborne = case_when(location == "Home" ~ 1,
                                where_contact != "Outdoors" & duration >= 4 ~ 1, 
                                .default = 0)
           ) %>%
    #Enteric:
    #All home contacts
    #All contacts involving touch
    #Outdoor-only contacts with no touch with duration of "31 mins-1 hr" or greater
    mutate(enteric = case_when(location == "Home" ~ 1,
                               touch_contact == "Yes" ~ 1,
                               where_contact != "Outdoors" & touch_contact == "No" & duration >= 3 ~ 1,
                               .default = 0)
           )
  
  
  contact_daily_resp <- daily %>%
    ungroup() %>%
    group_by(rec_id,study_day) %>%
    dplyr::summarize(num_resp_contacts_byday = sum(respiratory)) %>%
    ungroup() %>%
    dplyr::group_by(rec_id) %>%
    summarise(avg_daily_resp_contacts = (mean(num_resp_contacts_byday))) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id"))
  
  contact_daily_air <- daily %>%
    ungroup() %>%
    group_by(rec_id,study_day) %>%
    dplyr::summarize(num_air_contacts_byday = sum(airborne)) %>%
    ungroup() %>%
    dplyr::group_by(rec_id) %>%
    summarise(avg_daily_air_contacts = (mean(num_air_contacts_byday))) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id"))
  
  contact_daily_ent <- daily %>%
    ungroup() %>%
    group_by(rec_id,study_day) %>%
    dplyr::summarize(num_ent_contacts_byday = sum(enteric)) %>%
    ungroup() %>%
    dplyr::group_by(rec_id) %>%
    summarise(avg_daily_ent_contacts = (mean(num_ent_contacts_byday))) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id"))
  
  # NOTE: Not doing unique right now
  
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
  
  contacts_resp_air_ent <- left_join(contact_daily_resp, 
                                 contact_daily_ent %>% 
                                   ungroup() %>%
                                   dplyr::select(rec_id, avg_daily_ent_contacts),
                                 by = c("rec_id" = "rec_id")) %>%
  left_join(., contact_daily_air %>% ungroup() %>%
              dplyr::select(rec_id, avg_daily_air_contacts),
            by = c("rec_id" = "rec_id"))
  
  # Outlier variables ------------------------------------------------------------
  contact_summaries <- contacts_resp_air_ent %>%
    distinct() %>%
    ungroup() %>%
    dplyr::summarize(daily_resp_q50 = quantile(avg_daily_resp_contacts, probs = 0.50, na.rm=T),
                     daily_resp_q75 = quantile(avg_daily_resp_contacts, probs = 0.75, na.rm=T),
                     daily_resp_q80 = quantile(avg_daily_resp_contacts, probs = 0.80, na.rm=T),
                     daily_resp_q90 = quantile(avg_daily_resp_contacts, probs = 0.90, na.rm=T),
                     daily_resp_mean = mean(avg_daily_resp_contacts, na.rm=T),
                     daily_air_q50 = quantile(avg_daily_air_contacts, probs = 0.50, na.rm=T),
                     daily_air_q75 = quantile(avg_daily_air_contacts, probs = 0.75, na.rm=T),
                     daily_air_q80 = quantile(avg_daily_air_contacts, probs = 0.80, na.rm=T),
                     daily_air_q90 = quantile(avg_daily_air_contacts, probs = 0.90, na.rm=T),
                     daily_air_mean = mean(avg_daily_air_contacts, na.rm=T),
                     daily_ent_q50 = quantile(avg_daily_ent_contacts, probs = 0.50, na.rm=T),
                     daily_ent_q75 = quantile(avg_daily_ent_contacts, probs = 0.75, na.rm=T),
                     daily_ent_q80 = quantile(avg_daily_ent_contacts, probs = 0.80, na.rm=T),
                     daily_ent_q90 = quantile(avg_daily_ent_contacts, probs = 0.90, na.rm=T),
                     daily_ent_mean = mean(avg_daily_ent_contacts, na.rm=T))
  
  contacts_resp_air_ent <- contacts_resp_air_ent %>%
    mutate(daily_resp_mean_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_mean, 1, 0),
           daily_resp_q50_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q50, 1, 0),
           daily_resp_q75_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q75, 1, 0),
           daily_resp_q80_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q80, 1, 0),
           daily_resp_q90_outlier = ifelse(avg_daily_resp_contacts > contact_summaries$daily_resp_q90, 1, 0),
           daily_air_mean_outlier = ifelse(avg_daily_air_contacts > contact_summaries$daily_air_mean, 1, 0),
           daily_air_q50_outlier = ifelse(avg_daily_air_contacts > contact_summaries$daily_air_q50, 1, 0),
           daily_air_q75_outlier = ifelse(avg_daily_air_contacts > contact_summaries$daily_air_q75, 1, 0),
           daily_air_q80_outlier = ifelse(avg_daily_air_contacts > contact_summaries$daily_air_q80, 1, 0),
           daily_air_q90_outlier = ifelse(avg_daily_air_contacts > contact_summaries$daily_air_q90, 1, 0),
           daily_ent_mean_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_mean, 1, 0),
           daily_ent_q50_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q50, 1, 0),
           daily_ent_q75_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q75, 1, 0),
           daily_ent_q80_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q80, 1, 0),
           daily_ent_q90_outlier = ifelse(avg_daily_ent_contacts > contact_summaries$daily_ent_q90, 1, 0))
  
  return(contacts_resp_air_ent)
}
