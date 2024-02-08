rm(list=ls())
library(here)
library(plotly)
library(tidyverse)
library(MASS)
library(scales)

contacts_resp_ent <- read.csv(here("data/contacts_resp_ent.csv"))

contacts_resp_ent$age <- as.numeric(contacts_resp_ent$age)
contacts_resp_ent$sex <- contacts_resp_ent$participant_sex
contacts_resp_ent$site <- contacts_resp_ent$study_site

# Unique Q90 threshold outlier model -------------------------------------------------

unique_resp_q90_model <- glm(unique_resp_q90_outlier ~ participant_age + sex + site, 
                            data = contacts_resp_ent, family = binomial())
unique_resp_q90 <- as.data.frame(summary(unique_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q90_model <- glm(unique_ent_q90_outlier ~ participant_age + sex + site, 
                           data = contacts_resp_ent, family = binomial())
unique_ent_q90 <- as.data.frame(summary(unique_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


# Unique figures --------------------------------------------------------
factor_order = c("(Intercept)","participant_age6-11mo", "participant_age1-4y", 
                 "participant_age5-9y", "participant_age10-14y", 
                 "participant_age15-19y", "participant_age20-29y", 
                 "participant_age30-39y", "participant_age40-59y", 
                 "participant_age60+y", "sexMale", "siteUrban")

names(unique_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

unique_resp_q90$log_pvalue <- log(round( unique_resp_q90$p_value, 2))
unique_resp_q90$log_pvalue[which(is.infinite(unique_resp_q90$log_pvalue))] <- -5.99
unique_resp_q90$term <- factor(unique_resp_q90$term, levels = factor_order)

png("figs/unique_resp_q90.png", width=3000, height=1000, res=300)
ggplot(unique_resp_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE), lwd = 1.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site")
dev.off()

unique_ent_q90$log_pvalue <- log(round( unique_ent_q90$p_value, 2))
unique_ent_q90$log_pvalue[which(is.infinite(unique_ent_q90$log_pvalue))] <- -5.99
unique_ent_q90$term <- factor(unique_ent_q90$term, levels = factor_order)

png("figs/unique_ent_q90.png", width=3000, height=1000, res=300)
ggplot(unique_ent_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE), lwd = 1.5)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site")
dev.off()


write.csv(unique_resp_q90, "data/unique_resp_q90.csv")
write.csv(unique_ent_q90, "data/unique_ent_q90.csv")

# With HH Size -------------------------------------------------

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
                                    if_else(contacts_resp_ent$hh_occupants > 4, "5-6",
                                            if_else(contacts_resp_ent$hh_occupants > 2, "3-4",
                                                    "1-2")))
contacts_resp_ent$hhsize <- factor(contacts_resp_ent$hhsize, 
                                   levels = c("1-2", "3-4", "5-6", "7+"))

barplot(prop.table(table(contacts_resp_ent$hhsize)))

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

# table(contacts_resp_ent$participant_age, contacts_resp_ent$p_age)

## Unique Q90 threshold outlier model -------------------------------------------------

unique_resp_q90_model <- glm(unique_resp_q90_outlier ~ p_age + sex + site + hhsize, 
                             data = contacts_resp_ent, family = binomial())
unique_resp_q90 <- as.data.frame(summary(unique_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q90_model <- glm(unique_ent_q90_outlier ~ p_age + sex + site + hhsize, 
                            data = contacts_resp_ent, family = binomial())
unique_ent_q90 <- as.data.frame(summary(unique_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


## Unique figures --------------------------------------------------------
factor_order <- c("(Intercept)", "5-9y", "10-19y", "20-29y", 
                  "30-39y", "40-49y", "50-59y", "60+y", 
                  "Male", "Urban", "3-4", "5-6", "7+")

names(unique_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

unique_resp_q90$Coefficient = factor_order
unique_resp_q90$log_pvalue <- log(round( unique_resp_q90$p_value, 2))
unique_resp_q90$log_pvalue[which(is.infinite(unique_resp_q90$log_pvalue))] <- -5.99
unique_resp_q90$term <- factor(unique_resp_q90$term, factor_order)
unique_resp_q90$Coefficient <- factor(unique_resp_q90$Coefficient,
                                      levels = unique_resp_q90$Coefficient)
unique_resp_q90 <- unique_resp_q90[-1,]
unique_resp_q90$group = c(rep("Age \n(Ref: <4 year)", 7), 
                        "Sex \n(Ref: Female)", 
                        "Site \n(Ref: Rural)",
                        rep("Household Size \n(Ref: 1-2)", 3))

png("figs/unique_hhs_resp_q90.png", width=4500, height=2000, res=300)
ggplot(unique_resp_q90, aes(x = factor(paste0(Coefficient, "&", group), 
                                       level=paste0(Coefficient, "&", group)), 
                            y = estimate, color = log_pvalue)) + 
  geom_point(size = 4) +
  geom_hline(yintercept = 0, lwd = 1.5)+
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE), 
                lwd = 1.5, width = 0.6)+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "right") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size= 10))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ylab("Beta and Wald Confidence Interval")+
  xlab("Predictor")+
  guides(x = ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle("Figure 2. Multivariate association between sociodemographic predictors and high respiratory contact")+
  ylim(-2, 6)+ 
  theme_bw()
dev.off()

unique_ent_q90$Coefficient = factor_order
unique_ent_q90$log_pvalue <- log(round( unique_ent_q90$p_value, 2))
unique_ent_q90$log_pvalue[which(is.infinite(unique_ent_q90$log_pvalue))] <- -5.99
unique_ent_q90$term <- factor(unique_ent_q90$term, levels = factor_order)
unique_ent_q90$Coefficient <- factor(unique_ent_q90$Coefficient, 
                                      levels = unique_ent_q90$Coefficient)
unique_ent_q90 <- unique_ent_q90[-1,]
unique_ent_q90$group = c(rep("Age \n(Ref: <4 years)", 7), 
                            "Sex \n(Ref: Female)", 
                            "Site \n(Ref: Rural)",
                        rep("Household Size \n(Ref: 1-2)", 3))

png("figs/unique_hhs_ent_q90.png", width=4500, height=2000, res=300)
ggplot(unique_ent_q90, aes(x = factor(paste0(Coefficient, "&", group), 
                                      level=paste0(Coefficient, "&", group)), 
                           y = estimate, color = log_pvalue)) + 
  geom_point(size = 4) +
  geom_hline(yintercept = 0, lwd = 1.5)+
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE), 
                lwd = 1.5, width = 0.6)+
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.justification = "right") +
  theme(plot.title = element_text(size = 20),
        axis.title.x = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=16, face="bold"),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size= 10))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ylab("Beta and Wald Confidence Interval")+
  xlab("Predictor")+
  guides(x = ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle("Figure 3. Multivariate association between sociodemographic predictors and high enteric contact")+
  ylim(-2, 6)+ 
  theme_bw()
dev.off()


write.csv(unique_resp_q90, "data/unique_hhs_resp_q90.csv")
write.csv(unique_ent_q90, "data/unique_hhs_ent_q90.csv")

# With Occupation --------------------------------------------------------------

adults_contacts_resp_ent <- contacts_resp_ent %>% 
  filter(age >= 20, participant_age != "<6mo", participant_age != "15-19y")
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Child", "Fisherman"))] <- "Other"
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Retired"))] <- "Unemployed"
table(adults_contacts_resp_ent$occupation)
table(adults_contacts_resp_ent$participant_age)

## Unique Q90 threshold outlier model -------------------------------------------------

unique_resp_q90_model <- glm(unique_resp_q90_outlier ~ 
                               participant_age + sex + site + occupation , 
                             data = adults_contacts_resp_ent, family = binomial())
unique_resp_q90 <- as.data.frame(summary(unique_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q90_model <- glm(unique_ent_q90_outlier ~ 
                              participant_age + sex + site + occupation , 
                            data = adults_contacts_resp_ent, family = binomial())
unique_ent_q90 <- as.data.frame(summary(unique_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


## Unique figures --------------------------------------------------------
factor_order <- c("(Intercept)","30-39y", "40-59y", 
                  "60+y", "Male", "Urban",
                  "Student", 
                  "Other")

names(unique_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

unique_resp_q90$Coefficient = factor_order
# unique_resp_q90$log_pvalue <- log(round( unique_resp_q90$p_value, 2))
# unique_resp_q90$log_pvalue[which(is.infinite(unique_resp_q90$log_pvalue))] <- -5.99
unique_resp_q90$term <- factor(unique_resp_q90$Coefficient, levels = factor_order)
unique_resp_q90$Coefficient <- factor(unique_resp_q90$Coefficient,
                                      levels = unique_resp_q90$Coefficient)
unique_resp_q90 <- unique_resp_q90[-1,]
unique_resp_q90$group = c(rep("Age \n(Ref: 20-29y)", 3), 
                          "Sex \n(Ref: Female)", 
                          "Site \n(Ref: Rural)",
                          rep("Occupation \n(Ref: Unemployed or Retired)", 2))

png("figs/unique_occ_resp_q90.png", width=5000, height=2000, res=300)
ggplot(unique_resp_q90, aes(x = factor(paste0(Coefficient, "&", group), 
                                       level=paste0(Coefficient, "&", group)), 
                            y = estimate))+#, color = log_pvalue)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE), lwd = 1.5)+
  # theme(axis.text.x = element_text(angle = 45))+
  theme(axis.text = element_text(size = 12))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ylab("Estimate")+
  xlab("Coefficient")+
  guides(x = ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")+
  ylim(-7, 7)
dev.off()

unique_ent_q90$Coefficient = factor_order
# unique_ent_q90$log_pvalue <- log(round( unique_ent_q90$p_value, 2))
# unique_ent_q90$log_pvalue[which(is.infinite(unique_ent_q90$log_pvalue))] <- -5.99
unique_ent_q90$term <- factor(unique_ent_q90$Coefficient, levels = factor_order)
unique_ent_q90$Coefficient <- factor(unique_ent_q90$Coefficient,
                                      levels = unique_ent_q90$Coefficient)
unique_ent_q90 <- unique_ent_q90[-1,]
unique_ent_q90$group = c(rep("Age \n(Ref: 20-29y)", 3), 
                          "Sex \n(Ref: Female)", 
                          "Site \n(Ref: Rural)",
                          rep("Occupation \n(Ref: Unemployed or Retired)", 2))

png("figs/unique_occ_ent_q90.png", width=5000, height=2000, res=300)
ggplot(unique_ent_q90,aes(x = factor(paste0(Coefficient, "&", group), 
                                     level=paste0(Coefficient, "&", group)), 
                          y = estimate))+#, color = log_pvalue)) + 
  geom_point(size = 3) +
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE), lwd = 1.5)+
  # theme(axis.text.x = element_text(angle = 45))+
  theme(axis.text = element_text(size = 12))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ylab("Estimate")+
  xlab("Coefficient")+
  guides(x = ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + Occupation")+
  ylim(-7, 7)
dev.off()


write.csv(unique_resp_q90, "data/unique_occ_resp_q90.csv")
write.csv(unique_ent_q90, "data/unique_occ_ent_q90.csv")

# Non HH contacts Unique Q90 threshold outlier model ----------------------------------

contacts_resp_ent_nonHHcontacts <- read.csv(here("data/contacts_resp_ent_nonHHcontacts.csv"))
table(contacts_resp_ent_nonHHcontacts$unique_resp_q90_outlier)
contacts_resp_ent_nonHHcontacts$age <- as.numeric(contacts_resp_ent_nonHHcontacts$age)
contacts_resp_ent_nonHHcontacts$sex <- contacts_resp_ent_nonHHcontacts$participant_sex
contacts_resp_ent_nonHHcontacts$site <- contacts_resp_ent_nonHHcontacts$study_site

contacts_resp_ent_nonHHcontacts$part_age <- if_else(contacts_resp_ent_nonHHcontacts$participant_age == "<6mo" |
                                                      contacts_resp_ent_nonHHcontacts$participant_age == "6-11mo",
                                                    "<1y",
                                                    contacts_resp_ent_nonHHcontacts$participant_age)

table(contacts_resp_ent_nonHHcontacts$part_age, 
      contacts_resp_ent_nonHHcontacts$participant_age)

contacts_resp_ent_nonHHcontacts <- contacts_resp_ent_nonHHcontacts %>%
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

unique_resp_q90_model <- glm(unique_resp_q90_outlier ~ p_age + sex + site + hhsize, 
                             data = contacts_resp_ent_nonHHcontacts, family = binomial())
unique_resp_q90 <- as.data.frame(summary(unique_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

table(contacts_resp_ent_nonHHcontacts$participant_age, contacts_resp_ent_nonHHcontacts$unique_ent_q90_outlier)
table(contacts_resp_ent_nonHHcontacts$participant_age, contacts_resp_ent_nonHHcontacts$unique_resp_q90_outlier)
table(contacts_resp_ent_nonHHcontacts$part_age, contacts_resp_ent_nonHHcontacts$unique_ent_q90_outlier)
table(contacts_resp_ent_nonHHcontacts$part_age, contacts_resp_ent_nonHHcontacts$unique_resp_q90_outlier)
table(contacts_resp_ent_nonHHcontacts$p_age, contacts_resp_ent_nonHHcontacts$unique_ent_q90_outlier)
table(contacts_resp_ent_nonHHcontacts$p_age, contacts_resp_ent_nonHHcontacts$unique_resp_q90_outlier)

unique_ent_q90_model <- glm(unique_ent_q90_outlier ~ part_age + sex + site + hhsize, 
                            data = contacts_resp_ent_nonHHcontacts, family = binomial())
unique_ent_q90 <- as.data.frame(summary(unique_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


write.csv(unique_resp_q90, "data/nonHH_unique_resp_q90.csv")
write.csv(unique_ent_q90, "data/nonHH_unique_ent_q90.csv")

