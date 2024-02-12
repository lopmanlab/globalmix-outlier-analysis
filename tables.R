rm(list=ls())
pacman::p_load(here,
               tidyverse,
               ggplot2, 
               plotly, 
               lme4, 
               MASS,
               lmerTest)


contacts_resp_ent <- read.csv(here("data/contacts_resp_ent.csv")) %>%
  mutate(p_age = factor(p_age, c("<4y", "5-9y", "10-19y", "20-29y", "30-39y",
                                 "40-49y", "50-59y", "60+y")))
contacts_unique <- read.csv(here("data/contacts_unique.csv")) %>%
  filter(rec_id %in% unique(contacts_resp_ent$rec_id)) %>%
  mutate(p_age = factor(p_age, c("<4y", "5-9y", "10-19y", "20-29y", "30-39y",
                                 "40-49y", "50-59y", "60+y")))

df_contact <- read.csv(here("data/df_contact.csv"))

# Table 1 ----------------------------------------------------------------------

table1 <- data.frame(Var = c("Overall", "<4y", "5-9y", "10-19y", "20-29y", 
                             "30-39y", "40-49y", "50-59y", "60+y", 
                             "Female", "Male", "Rural", "Urban"),
                     N = c(paste0(nrow(contacts_unique), " (100)"), 
                           paste0(table(contacts_unique$p_age), " (", 
                                  round(100*prop.table(table(contacts_unique$p_age))), ")"),
                           paste0(table(contacts_unique$participant_sex), " (", 
                                  round(100*prop.table(table(contacts_unique$participant_sex))), ")"),
                           paste0(table(contacts_unique$study_site), " (", 
                                  round(100*prop.table(table(contacts_unique$study_site))), ")")
                     ),
                     Median = round(c(median(contacts_unique$avg_unique_contacts),
                                      contacts_unique %>%
                                        group_by(p_age) %>%
                                        summarise_at(vars(avg_unique_contacts), 
                                                     list(name = median))%>%
                                        dplyr::select(name)%>%
                                        unlist(),
                                      contacts_unique %>%
                                        group_by(participant_sex) %>%
                                        summarise_at(vars(avg_unique_contacts), 
                                                     list(name = median))%>%
                                        dplyr::select(name)%>%
                                        unlist(),
                                      contacts_unique %>%
                                        group_by(study_site) %>%
                                        summarise_at(vars(avg_unique_contacts), 
                                                     list(name = median))%>%
                                        dplyr::select(name)%>%
                                        unlist()
                     ),1),
                     Mean = round(c(mean(contacts_unique$avg_unique_contacts),
                              contacts_unique %>%
                                group_by(p_age) %>%
                                summarise_at(vars(avg_unique_contacts), 
                                             list(name = mean))%>%
                                dplyr::select(name)%>%
                                unlist(),
                              contacts_unique %>%
                                group_by(participant_sex) %>%
                                summarise_at(vars(avg_unique_contacts), 
                                             list(name = mean))%>%
                                dplyr::select(name)%>%
                                unlist(),
                              contacts_unique %>%
                                group_by(study_site) %>%
                                summarise_at(vars(avg_unique_contacts), 
                                             list(name = mean))%>%
                                dplyr::select(name)%>%
                                unlist()
                              ),1),
                     R_Median = round(c(median(contacts_resp_ent$avg_unique_resp_contacts),
                                        contacts_resp_ent %>%
                                          group_by(p_age) %>%
                                          summarise_at(vars(avg_unique_resp_contacts), 
                                                       list(name = median))%>%
                                          dplyr::select(name)%>%
                                          unlist(),
                                        contacts_resp_ent %>%
                                          group_by(participant_sex) %>%
                                          summarise_at(vars(avg_unique_resp_contacts), 
                                                       list(name = median))%>%
                                          dplyr::select(name)%>%
                                          unlist(),
                                        contacts_resp_ent %>%
                                          group_by(study_site) %>%
                                          summarise_at(vars(avg_unique_resp_contacts), 
                                                       list(name = median))%>%
                                          dplyr::select(name)%>%
                                          unlist()
                     ),1),
                     R_Mean = round(c(mean(contacts_resp_ent$avg_unique_resp_contacts),
                                    contacts_resp_ent %>%
                                      group_by(p_age) %>%
                                      summarise_at(vars(avg_unique_resp_contacts), 
                                                   list(name = mean))%>%
                                      dplyr::select(name)%>%
                                      unlist(),
                                    contacts_resp_ent %>%
                                      group_by(participant_sex) %>%
                                      summarise_at(vars(avg_unique_resp_contacts), 
                                                   list(name = mean))%>%
                                      dplyr::select(name)%>%
                                      unlist(),
                                    contacts_resp_ent %>%
                                      group_by(study_site) %>%
                                      summarise_at(vars(avg_unique_resp_contacts), 
                                                   list(name = mean))%>%
                                      dplyr::select(name)%>%
                                      unlist()
                     ),1),
                     E_Median = round(c(median(contacts_resp_ent$avg_unique_ent_contacts, na.rm = TRUE),
                                        contacts_resp_ent %>%
                                          drop_na(avg_unique_ent_contacts)%>%
                                          group_by(p_age) %>%
                                          summarise_at(vars(avg_unique_ent_contacts), 
                                                       list(name = median))%>%
                                          dplyr::select(name)%>%
                                          unlist(),
                                        contacts_resp_ent %>%
                                          drop_na(avg_unique_ent_contacts)%>%
                                          group_by(participant_sex) %>%
                                          summarise_at(vars(avg_unique_ent_contacts), 
                                                       list(name = median))%>%
                                          dplyr::select(name)%>%
                                          unlist(),
                                        contacts_resp_ent %>%
                                          drop_na(avg_unique_ent_contacts)%>%
                                          group_by(study_site) %>%
                                          summarise_at(vars(avg_unique_ent_contacts), 
                                                       list(name = median))%>%
                                          dplyr::select(name)%>%
                                          unlist()),1),
                     E_Mean = round(c(mean(contacts_resp_ent$avg_unique_ent_contacts, na.rm = TRUE),
                                      contacts_resp_ent %>%
                                        drop_na(avg_unique_ent_contacts)%>%
                                        group_by(p_age) %>%
                                        summarise_at(vars(avg_unique_ent_contacts), 
                                                     list(name = mean))%>%
                                        dplyr::select(name)%>%
                                        unlist(),
                                      contacts_resp_ent %>%
                                        drop_na(avg_unique_ent_contacts)%>%
                                        group_by(participant_sex) %>%
                                        summarise_at(vars(avg_unique_ent_contacts), 
                                                     list(name = mean))%>%
                                        dplyr::select(name)%>%
                                        unlist(),
                                      contacts_resp_ent %>%
                                        drop_na(avg_unique_ent_contacts)%>%
                                        group_by(study_site) %>%
                                        summarise_at(vars(avg_unique_ent_contacts), 
                                                     list(name = mean))%>%
                                        dplyr::select(name)%>%
                                        unlist()),1)
                     )

write.csv(table1, "data/table1_india.csv")

table(contacts_unique$occupation, useNA = "always")

table1a <- data.frame(Var = c("Overall" ,"2", "3-4", "5-6", "7+", "NA", "Child", 
                              "Farmer/Clerk/Shopkeeper", "Professional", "Retired",
                              "Semiprofessional", "Semiskilled laborer", 
                              "Skilled laborer", "Student", 
                              "Unemployed adult", "Unskilled laborer", "NA"),
                      N = c(paste0(nrow(contacts_unique), " (100)"), 
                            paste0(table(contacts_unique$hhsize, useNA = "always"), " (", 
                                   round(100*prop.table(table(contacts_unique$hhsize, useNA = "always"))), ")"),
                            paste0(table(contacts_unique$occupation, useNA = "always"), " (", 
                                   round(100*prop.table(table(contacts_unique$occupation, useNA = "always"))), ")")
                      ),
                      Median = round(c(median(contacts_unique$avg_unique_contacts),
                                       contacts_unique %>%
                                         group_by(hhsize) %>%
                                         summarise_at(vars(avg_unique_contacts), 
                                                      list(name = median))%>%
                                         dplyr::select(name)%>%
                                         unlist(),
                                       contacts_unique %>%
                                         group_by(occupation) %>%
                                         summarise_at(vars(avg_unique_contacts), 
                                                      list(name = median))%>%
                                         dplyr::select(name)%>%
                                         unlist()
                      ),1),
                      Mean = round(c(mean(contacts_unique$avg_unique_contacts),
                                       contacts_unique %>%
                                         group_by(hhsize) %>%
                                         summarise_at(vars(avg_unique_contacts), 
                                                      list(name = mean))%>%
                                         dplyr::select(name)%>%
                                         unlist(),
                                       contacts_unique %>%
                                         group_by(occupation) %>%
                                         summarise_at(vars(avg_unique_contacts), 
                                                      list(name = mean))%>%
                                         dplyr::select(name)%>%
                                         unlist()
                      ),1))


write.csv(table1a, "data/table1a_india.csv")

median(contacts_resp_ent$avg_unique_resp_contacts)
contacts_resp_ent %>%
  group_by(hhsize) %>%
  summarise_at(vars(avg_unique_resp_contacts), list(name = median))

contacts_resp_ent %>%
  group_by(occupation) %>%
  summarise_at(vars(avg_unique_resp_contacts), list(name = median))

mean(contacts_resp_ent$avg_unique_resp_contacts)
contacts_resp_ent %>%
  group_by(hhsize) %>%
  summarise_at(vars(avg_unique_resp_contacts), list(name = mean))
contacts_resp_ent %>%
  group_by(occupation) %>%
  summarise_at(vars(avg_unique_resp_contacts), list(name = mean))

median(contacts_resp_ent$avg_unique_ent_contacts, na.rm=T)
contacts_resp_ent %>%
  drop_na(avg_unique_ent_contacts)%>%
  group_by(hhsize) %>%
  summarise_at(vars(avg_unique_ent_contacts), list(name = median))
contacts_resp_ent %>%
  drop_na(avg_unique_ent_contacts)%>%
  group_by(occupation) %>%
  summarise_at(vars(avg_unique_ent_contacts), list(name = median))
                      
mean(contacts_resp_ent$avg_unique_ent_contacts, na.rm=T)
contacts_resp_ent %>%
  drop_na(avg_unique_ent_contacts)%>%
  group_by(hhsize) %>%
  summarise_at(vars(avg_unique_ent_contacts), list(name = mean))
contacts_resp_ent %>%
  drop_na(avg_unique_ent_contacts)%>%
  group_by(occupation) %>%
  summarise_at(vars(avg_unique_ent_contacts), list(name = mean))

# Df contact -------------------------------------------------------------------

table(df_contact$respiratory, df_contact$enteric)

quantile(contacts_unique$avg_unique_contacts, 0.9)
