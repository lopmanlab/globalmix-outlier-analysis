pacman::p_load(here,
               tidyverse,
               plotly,
               UpSetR,
               ComplexUpset)

prepare_simple_outlier_datasets <- function(cty = "moz", hhmbr = ""){
  
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
    mutate(duration = case_when(duration_contact == "<5 mins" ~ 3,
                                duration_contact == "5-15 mins" ~ 10,
                                duration_contact == "16-30 mins" ~ 23,
                                duration_contact == "31 mins-1 hr" ~ 45,
                                duration_contact == "1-4 hrs" ~ 2.5*60,
                                duration_contact  == ">4 hrs" ~ 6*60)) %>%
    mutate(occupation = case_when(is.na(occupation) ~ "Unemployed outside home",
                                  occupation == "Other" ~ "Unemployed outside home",
                                  .default = occupation)) %>%
    mutate(hh_size_cat = case_when(#hh_size >= 9 ~ "(7,50]",
      hh_size_cat == "(10,50]" ~ "(5,50]",
      hh_size_cat == "(5,10]" ~ "(5,50]",
      .default = hh_size_cat)) %>%
    mutate(hh_size_cat = factor(hh_size_cat, levels = c("[0,2]", "(2,5]", "(5,50]")))#"(5,7]", "(7,50]")))

  daily <- daily %>%
    mutate(nonhh_indoor_time = ifelse(location != "Home", duration, 0))
    
  daily_nonhh <- daily %>%
    ungroup() %>%
    group_by(rec_id,study_day) %>%
    dplyr::summarize(num_nonhh_contacts_byday = sum(hh_membership == "Non-member")) %>%
    ungroup() %>%
    dplyr::group_by(rec_id) %>%
    summarise(avg_daily_nonhh_contacts = (mean(num_nonhh_contacts_byday))) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id"))
  
  daily_indoortime <- daily %>%
    ungroup() %>%
    group_by(rec_id,study_day) %>%
    dplyr::summarize(num_indtime_contacts_byday = sum(nonhh_indoor_time) / 60) %>%
    ungroup() %>%
    dplyr::group_by(rec_id) %>%
    summarise(avg_daily_indtime_contacts = (mean(num_indtime_contacts_byday))) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id"))
  
  daily_touch <- daily %>%
    ungroup() %>%
    group_by(rec_id,study_day) %>%
    dplyr::summarize(num_touch_contacts_byday = sum(touch_contact == "Yes" & hh_membership == "Non-member")) %>%
    ungroup() %>%
    dplyr::group_by(rec_id) %>%
    summarise(avg_daily_touch_contacts = (mean(num_touch_contacts_byday))) %>%
    left_join(participants, ., by=c("rec_id" = "rec_id"))
  
  combined <- left_join(daily_nonhh, 
                        daily_indoortime %>% 
                          ungroup() %>%
                          dplyr::select(rec_id, avg_daily_indtime_contacts),
                        by = c("rec_id" = "rec_id")) %>%
    left_join(., daily_touch %>% ungroup() %>%
                dplyr::select(rec_id, avg_daily_touch_contacts),
              by = c("rec_id" = "rec_id"))
  
  contact_summaries <- combined %>%
    distinct() %>%
    ungroup() %>%
    dplyr::summarize(daily_nonhh_q50 = quantile(avg_daily_nonhh_contacts, probs = 0.50, na.rm=T),
                     daily_nonhh_q75 = quantile(avg_daily_nonhh_contacts, probs = 0.75, na.rm=T),
                     daily_nonhh_q80 = quantile(avg_daily_nonhh_contacts, probs = 0.80, na.rm=T),
                     daily_nonhh_q90 = quantile(avg_daily_nonhh_contacts, probs = 0.90, na.rm=T),
                     daily_nonhh_mean = mean(avg_daily_nonhh_contacts, na.rm=T),
                     daily_nonhh_min = min(avg_daily_nonhh_contacts, na.rm=T),
                     daily_nonhh_max = max(avg_daily_nonhh_contacts, na.rm=T),
                     daily_indtime_q50 = quantile(avg_daily_indtime_contacts, probs = 0.50, na.rm=T),
                     daily_indtime_q75 = quantile(avg_daily_indtime_contacts, probs = 0.75, na.rm=T),
                     daily_indtime_q80 = quantile(avg_daily_indtime_contacts, probs = 0.80, na.rm=T),
                     daily_indtime_q90 = quantile(avg_daily_indtime_contacts, probs = 0.90, na.rm=T),
                     daily_indtime_mean = mean(avg_daily_indtime_contacts, na.rm=T),
                     daily_indtime_min = min(avg_daily_indtime_contacts, na.rm=T),
                     daily_indtime_max = max(avg_daily_indtime_contacts, na.rm=T),
                     daily_touch_q50 = quantile(avg_daily_touch_contacts, probs = 0.50, na.rm=T),
                     daily_touch_q75 = quantile(avg_daily_touch_contacts, probs = 0.75, na.rm=T),
                     daily_touch_q80 = quantile(avg_daily_touch_contacts, probs = 0.80, na.rm=T),
                     daily_touch_q90 = quantile(avg_daily_touch_contacts, probs = 0.90, na.rm=T),
                     daily_touch_mean = mean(avg_daily_touch_contacts, na.rm=T),
                     daily_touch_min = min(avg_daily_touch_contacts, na.rm=T),
                     daily_touch_max = max(avg_daily_touch_contacts, na.rm=T))
  
  contacts_simple <- combined %>%
    mutate(daily_nonhh_mean_outlier = ifelse(avg_daily_nonhh_contacts > contact_summaries$daily_nonhh_mean, 1, 0),
           daily_nonhh_q50_outlier = ifelse(avg_daily_nonhh_contacts > contact_summaries$daily_nonhh_q50, 1, 0),
           daily_nonhh_q75_outlier = ifelse(avg_daily_nonhh_contacts > contact_summaries$daily_nonhh_q75, 1, 0),
           daily_nonhh_q80_outlier = ifelse(avg_daily_nonhh_contacts > contact_summaries$daily_nonhh_q80, 1, 0),
           daily_nonhh_q90_outlier = ifelse(avg_daily_nonhh_contacts > contact_summaries$daily_nonhh_q90, 1, 0),
           daily_indtime_mean_outlier = ifelse(avg_daily_indtime_contacts > contact_summaries$daily_indtime_mean, 1, 0),
           daily_indtime_q50_outlier = ifelse(avg_daily_indtime_contacts > contact_summaries$daily_indtime_q50, 1, 0),
           daily_indtime_q75_outlier = ifelse(avg_daily_indtime_contacts > contact_summaries$daily_indtime_q75, 1, 0),
           daily_indtime_q80_outlier = ifelse(avg_daily_indtime_contacts > contact_summaries$daily_indtime_q80, 1, 0),
           daily_indtime_q90_outlier = ifelse(avg_daily_indtime_contacts > contact_summaries$daily_indtime_q90, 1, 0),
           daily_touch_mean_outlier = ifelse(avg_daily_touch_contacts > contact_summaries$daily_touch_mean, 1, 0),
           daily_touch_q50_outlier = ifelse(avg_daily_touch_contacts > contact_summaries$daily_touch_q50, 1, 0),
           daily_touch_q75_outlier = ifelse(avg_daily_touch_contacts > contact_summaries$daily_touch_q75, 1, 0),
           daily_touch_q80_outlier = ifelse(avg_daily_touch_contacts > contact_summaries$daily_touch_q80, 1, 0),
           daily_touch_q90_outlier = ifelse(avg_daily_touch_contacts > contact_summaries$daily_touch_q90, 1, 0))
  
  print(cty)
  print(hhmbr)
  table(contacts_simple$daily_nonhh_q80_outlier, useNA = "always")  
  prop.table(  table(contacts_simple$daily_nonhh_q80_outlier, useNA = "always")  )
  summary(contacts_simple$avg_daily_indtime_contacts)
  prop.table(  table(contacts_simple$daily_nonhh_q80_outlier, contacts_simple$daily_indtime_q80_outlier, useNA = "always")  )
  write.csv(contact_summaries, paste0(here(),"/data/", c, "_simple_summaries.csv"))
  return(contacts_simple)
  
}