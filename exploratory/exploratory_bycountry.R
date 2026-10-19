rm(list = ls())
pacman::p_load(here,
               tidyverse,
               plotly, 
               png,
               grid,
               gridExtra,
               tiff)


countries = c("moz", "ind", "gt", "pak")

all = list()

for(c in countries){

  all[[c]] <- read.csv(paste0(here(),"/data/",c,"/", c, "_outlier_simple.csv")) %>%
    mutate(age5 = case_when(participant_age %in% c("<1y", "1-4y") ~ "<5y",
                            participant_age %in% c("5-9y", "10-19y") ~ "5-19y",
                            participant_age %in% c("20-29y", "30-39y") ~ "20-39y",
                            participant_age %in% c("40-59y") ~ "40-59y",
                            participant_age %in% c("60+y") ~ "60+y")) %>%
    mutate(age5 = factor(age5, levels = c("<5y", "5-19y", "20-39y", "40-59y", "60+y"))) %>%
    drop_na(age5) %>%
    group_by(age5) %>%
    summarise(rsv_contacts = mean(avg_daily_nonhh_contacts, na.rm = T) )
}

dat <- as.data.frame(all)
