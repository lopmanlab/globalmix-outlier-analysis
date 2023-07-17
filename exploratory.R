##########################################
############## Exploratory ###############
##########################################

#Contact Definition Exploration ------------------------------------------------

# Read in data
rm(list = ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
contact_summaries <- readRDS(here("data/contact_summaries.rds"))
df_contact <- readRDS(here("data/df_contact.rds"))
contacts_resp_ent <- readRDS(here("data/contacts_resp_ent.rds"))

#Average/Q75 respiratory and enteric contacts
contact_summaries

# Proportion of contacts that were considered both
table(df_contact$respiratory == 1 & df_contact$enteric == 1)
16219 / (16219 + 3708)

# Proportion of contacts that were considered both by site
table(df_contact$respiratory == 1 & df_contact$enteric == 1, df_contact$study_site)
#Rural: 78%
9155/(9155+2627)
#Urban: 87%
7064/(7064+1081)

# Respiratory contacts by age group
table(df_contact$respiratory == 1, df_contact$participant_age)
df_contact %>%
  group_by(participant_age, respiratory) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  write.csv(., "data/participant_age_respiratory.csv")

# Enteric Contacts by Age
table(df_contact$enteric == 1, df_contact$participant_age)
df_contact %>%
  group_by(participant_age, enteric) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  write.csv(., "data/participant_age_enteric.csv")

#Average respiratory and enteric contacts by rural/urban 


#Include number of outliers as well per each definition 
table(contacts_resp_ent$daily_ent_q75_outlier)
301/(301+1058)
table(contacts_resp_ent$daily_resp_q75_outlier)
304/(301+1058)

#Number of outliers by site respiratory
table(contacts_resp_ent$daily_resp_q75_outlier, contacts_resp_ent$study_site)
#Rural: 31%
213/(213+483)
#Urban: 14%
91/(91+575)

#Number of outliers by site enteric
table(contacts_resp_ent$daily_ent_q75_outlier, contacts_resp_ent$study_site)
#Rural: 30%
208/(208+485)
#Urban: 14%
93/(93+573)

# Unique contact outliers by site
table(contacts_resp_ent$unique_resp_q75_outlier, contacts_resp_ent$study_site)
#Rural: 31%
218 / (218+478)
#Urban: 14%
90 / (90+576)

table(contacts_resp_ent$unique_ent_q75_outlier, contacts_resp_ent$study_site)
#Rural: 30%
210 / (210+483)
#Urban: 14%
93 / (93+573)

ggplot(data = contacts_resp_ent)+
  geom_histogram(aes(x = avg_daily_resp_contacts))
ggplot(data = contacts_resp_ent)+
  geom_histogram(aes(x = avg_unique_resp_contacts))

ggplot(data = contacts_resp_ent)+
  geom_histogram(aes(x = avg_daily_ent_contacts))
ggplot(data = contacts_resp_ent)+
  geom_histogram(aes(x = avg_unique_ent_contacts))

# Contacts exploration ---------------------------------------------------------

contacts_daily <- readRDS(here("data/contacts_daily.rds"))
contacts_unique <- readRDS(here("data/contact_unique.rds"))

ggplot(data = contacts_daily)+
  geom_histogram(aes(x = avg_daily_contacts))

ggplot(data = contacts_unique)+
  geom_histogram(aes(x = avg_unique_contacts))


# Table 1 - any contact --------------------------------------------------------

summary(contacts_daily$avg_daily_contacts)
quantile(contacts_daily$avg_daily_contacts, probs = c(0.90, 0.95, 0.99))

prop.table(table(contacts_daily$participant_sex, useNA = "always"))*100
prop.table(table(contacts_daily$participant_age, useNA = "always"))*100

tapply(contacts_daily$avg_daily_contacts, contacts_daily$participant_sex, summary)
contacts_daily %>% 
  group_by(participant_sex) %>%
  summarise(q90 = quantile(avg_daily_contacts, probs = 0.9),
            q95 = quantile(avg_daily_contacts, probs = 0.95),
            q99 = quantile(avg_daily_contacts, probs = 0.99))

tapply(contacts_daily$avg_daily_contacts, contacts_daily$participant_age, summary)
contacts_daily %>% 
  group_by(participant_age) %>%
  summarise(q90 = quantile(avg_daily_contacts, probs = 0.9),
            q95 = quantile(avg_daily_contacts, probs = 0.95),
            q99 = quantile(avg_daily_contacts, probs = 0.99))

summary(contacts_unique$avg_unique_contacts)

prop.table(table(contacts_unique$participant_sex, useNA = "always"))*100
prop.table(table(contacts_unique$participant_age, useNA = "always"))*100

tapply(contacts_unique$avg_unique_contacts, contacts_unique$participant_sex, summary)
contacts_unique %>% 
  group_by(participant_sex) %>%
  summarise(q90 = quantile(avg_unique_contacts, probs = 0.9),
            q95 = quantile(avg_unique_contacts, probs = 0.95),
            q99 = quantile(avg_unique_contacts, probs = 0.99))

tapply(contacts_unique$avg_unique_contacts, contacts_unique$participant_age, summary)
contacts_unique %>% 
  group_by(participant_age) %>%
  summarise(q90 = quantile(avg_unique_contacts, probs = 0.9),
            q95 = quantile(avg_unique_contacts, probs = 0.95),
            q99 = quantile(avg_unique_contacts, probs = 0.99))


# Table 1 - respiratory contacts -----------------------------------------------
summary(contacts_resp_ent$avg_daily_resp_contacts)
quantile(contacts_resp_ent$avg_daily_resp_contacts, 
         probs = c(0.90, 0.95, 0.99), 
         na.rm = T)

prop.table(table(contacts_resp_ent$participant_sex, useNA = "always"))*100
prop.table(table(contacts_resp_ent$participant_age, useNA = "always"))*100

tapply(contacts_resp_ent$avg_daily_resp_contacts, contacts_resp_ent$participant_sex, summary)
contacts_resp_ent %>% 
  group_by(participant_sex) %>%
  summarise(q90 = quantile(avg_daily_resp_contacts, probs = 0.9, na.rm = T),
            q95 = quantile(avg_daily_resp_contacts, probs = 0.95, na.rm = T),
            q99 = quantile(avg_daily_resp_contacts, probs = 0.99, na.rm = T))

tapply(contacts_resp_ent$avg_daily_resp_contacts, contacts_resp_ent$participant_age, summary)
contacts_resp_ent %>% 
  group_by(participant_age) %>%
  summarise(q90 = quantile(avg_daily_resp_contacts, probs = 0.9, na.rm = T),
            q95 = quantile(avg_daily_resp_contacts, probs = 0.95, na.rm = T),
            q99 = quantile(avg_daily_resp_contacts, probs = 0.99, na.rm = T))


# Unique Respiratory -----------------------------------------------------------
summary(contacts_resp_ent$avg_unique_resp_contacts)
quantile(contacts_resp_ent$avg_unique_resp_contacts, 
         probs = c(0.90, 0.95, 0.99), 
         na.rm = T)

prop.table(table(contacts_resp_ent$participant_sex, useNA = "always"))*100
prop.table(table(contacts_resp_ent$participant_age, useNA = "always"))*100

tapply(contacts_resp_ent$avg_unique_resp_contacts, 
       contacts_resp_ent$participant_sex, 
       summary)
contacts_resp_ent %>% 
  group_by(participant_sex) %>%
  summarise(q90 = quantile(avg_unique_resp_contacts, probs = 0.9),
            q95 = quantile(avg_unique_resp_contacts, probs = 0.95),
            q99 = quantile(avg_unique_resp_contacts, probs = 0.99))

tapply(contacts_resp_ent$avg_unique_resp_contacts, contacts_resp_ent$participant_age, summary)
contacts_resp_ent %>% 
  group_by(participant_age) %>%
  summarise(q90 = quantile(avg_unique_resp_contacts, probs = 0.9),
            q95 = quantile(avg_unique_resp_contacts, probs = 0.95),
            q99 = quantile(avg_unique_resp_contacts, probs = 0.99))


# Table 1 - enteric contacts -----------------------------------------------
summary(contacts_resp_ent$avg_daily_ent_contacts)
quantile(contacts_resp_ent$avg_daily_ent_contacts, 
         probs = c(0.90, 0.95, 0.99),
         na.rm = T)

prop.table(table(contacts_resp_ent$participant_sex, useNA = "always"))*100
prop.table(table(contacts_resp_ent$participant_age, useNA = "always"))*100

tapply(contacts_resp_ent$avg_daily_ent_contacts, 
       contacts_resp_ent$participant_sex, 
       summary,
       na.rm = T)
contacts_resp_ent %>% 
  group_by(participant_sex) %>%
  summarise(q90 = quantile(avg_daily_ent_contacts, probs = 0.9, na.rm = T),
            q95 = quantile(avg_daily_ent_contacts, probs = 0.95, na.rm = T),
            q99 = quantile(avg_daily_ent_contacts, probs = 0.99, na.rm = T))

tapply(contacts_resp_ent$avg_daily_ent_contacts, 
       contacts_resp_ent$participant_age, 
       summary,
       na.rm = T)
contacts_resp_ent %>% 
  group_by(participant_age) %>%
  summarise(q90 = quantile(avg_daily_ent_contacts, probs = 0.9, na.rm = T),
            q95 = quantile(avg_daily_ent_contacts, probs = 0.95, na.rm = T),
            q99 = quantile(avg_daily_ent_contacts, probs = 0.99, na.rm = T))

# Unique Enteric ---------------------------------------------------------------

summary(contacts_resp_ent$avg_unique_ent_contacts)
quantile(contacts_resp_ent$avg_unique_ent_contacts, 
         probs = c(0.90, 0.95, 0.99),
         na.rm = T)

prop.table(table(contacts_resp_ent$participant_sex, useNA = "always"))*100
prop.table(table(contacts_resp_ent$participant_age, useNA = "always"))*100

tapply(contacts_resp_ent$avg_unique_ent_contacts, contacts_resp_ent$participant_sex, summary)
contacts_resp_ent %>% 
  group_by(participant_sex) %>%
  summarise(q90 = quantile(avg_unique_ent_contacts, probs = 0.9, na.rm = T),
            q95 = quantile(avg_unique_ent_contacts, probs = 0.95, na.rm = T),
            q99 = quantile(avg_unique_ent_contacts, probs = 0.99, na.rm = T))

tapply(contacts_resp_ent$avg_unique_ent_contacts, 
       contacts_resp_ent$participant_age, 
       summary, 
       na.rm = T)
contacts_resp_ent %>% 
  group_by(participant_age) %>%
  summarise(q90 = quantile(avg_unique_ent_contacts, probs = 0.9, na.rm = T),
            q95 = quantile(avg_unique_ent_contacts, probs = 0.95, na.rm = T),
            q99 = quantile(avg_unique_ent_contacts, probs = 0.99, na.rm = T))

# Contact matrix by type of contact -----------------------------------------
# m1data %>%
#     ggplot(aes(x = participant_age, y = contact_age, fill=average_contact)) +
#     geom_raster() +
#     geom_text(aes(participant_age, contact_age, label = average_contact), 
#               color = "black", size = 3) +
#     theme_classic() +
#     scale_fill_gradient2(low="#0571b0", mid="#92c5de", high="#ca0020", 
#                          limits=c(0,8), breaks=(c(0,2,4,6,8))) +
#     labs(x ="Participant age", 
#          y = "Contact age",
#          title = title,
#          fill = "Average\ncontacts") +
#     theme(legend.title = element_text(size = 10),
#           legend.text = element_text(size = 8),
#           legend.justification = "right") +
#     theme(plot.title = element_text(size = 20), 
#           axis.title.x = element_text(size=16, face="bold"),
#           axis.title.y = element_text(size=16, face="bold"),
#           axis.text.x = element_text(size = 10, angle=60),
#           axis.text.y = element_text(size= 10))


