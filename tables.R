rm(list=ls())
pacman::p_load(here,
               dplyr,
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

table1a <- 
