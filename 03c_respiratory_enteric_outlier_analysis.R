rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(MASS)

contacts_resp_ent <- readRDS(here("data/contacts_resp_ent.RDS"))

contacts_resp_ent$age <- as.numeric(contacts_resp_ent$age)
contacts_resp_ent$sex <- contacts_resp_ent$participant_sex
contacts_resp_ent$site <- contacts_resp_ent$study_site

# Daily Mean threshold outlier model -------------------------------------------------

daily_resp_mean_model <- glm(daily_resp_mean_outlier ~ participant_age + sex + site, 
                      data = contacts_resp_ent, family = binomial())
daily_resp_mean <- as.data.frame(summary(daily_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_mean_model <- glm(daily_ent_mean_outlier ~ participant_age + sex + site, 
                             data = contacts_resp_ent, family = binomial())
daily_ent_mean <- as.data.frame(summary(daily_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

# Daily Q75 threshold outlier model -------------------------------------------------

daily_resp_q75_model <- glm(daily_resp_q75_outlier ~ participant_age + sex + site, 
                             data = contacts_resp_ent, family = binomial())
daily_resp_q75 <- as.data.frame(summary(daily_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q75_model <- glm(daily_ent_q75_outlier ~ participant_age + sex + site, 
                            data = contacts_resp_ent, family = binomial())
daily_ent_q75 <- as.data.frame(summary(daily_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

# Daily Q90 threshold outlier model -------------------------------------------------

daily_resp_q90_model <- glm(daily_resp_q90_outlier ~ participant_age + sex + site, 
                            data = contacts_resp_ent, family = binomial())
daily_resp_q90 <- as.data.frame(summary(daily_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q90_model <- glm(daily_ent_q90_outlier ~ participant_age + sex + site, 
                           data = contacts_resp_ent, family = binomial())
daily_ent_q90 <- as.data.frame(summary(daily_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

# Daily figures --------------------------------------------------------
factor_order = c("(Intercept)","participant_age6-11mo", "participant_age1-4y", 
                 "participant_age5-9y", "participant_age10-14y", 
                 "participant_age15-19y", "participant_age20-29y", 
                 "participant_age30-39y", "participant_age40-59y", 
                 "participant_age60+y", "sexMale", "siteUrban")

names(daily_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

daily_resp_mean$log_pvalue <- log(round( daily_resp_mean$p_value, 2))
daily_resp_mean$log_pvalue[which(is.infinite(daily_resp_mean$log_pvalue))] <- -5.99
daily_resp_mean$term <- factor(daily_resp_mean$term, levels = factor_order)

png("figs/daily_resp_mean.png", width=3000, height=1000, res=300)
ggplot(daily_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site")
dev.off()

daily_ent_mean$log_pvalue <- log(round( daily_ent_mean$p_value, 2))
daily_ent_mean$log_pvalue[which(is.infinite(daily_ent_mean$log_pvalue))] <- -5.99
daily_ent_mean$term <- factor(daily_ent_mean$term, levels = factor_order)

png("figs/daily_ent_mean.png", width=3000, height=1000, res=300)
ggplot(daily_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site")
dev.off()

daily_resp_q75$log_pvalue <- log(round( daily_resp_q75$p_value, 2))
daily_resp_q75$log_pvalue[which(is.infinite(daily_resp_q75$log_pvalue))] <- -5.99
daily_resp_q75$term <- factor(daily_resp_q75$term, levels = factor_order)

png("figs/daily_resp_q75.png", width=3000, height=1000, res=300)
ggplot(daily_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site")
dev.off()

daily_ent_q75$log_pvalue <- log(round( daily_ent_q75$p_value, 2))
daily_ent_q75$log_pvalue[which(is.infinite(daily_ent_q75$log_pvalue))] <- -5.99
daily_ent_q75$term <- factor(daily_ent_q75$term, levels = factor_order)

png("figs/daily_ent_q75.png", width=3000, height=1000, res=300)
ggplot(daily_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site")
dev.off()


write.csv(daily_resp_q75, "data/daily_resp_q75.csv")
write.csv(daily_ent_q75, "data/daily_ent_q75.csv")


daily_resp_q90$log_pvalue <- log(round( daily_resp_q90$p_value, 2))
daily_resp_q90$log_pvalue[which(is.infinite(daily_resp_q90$log_pvalue))] <- -5.99
daily_resp_q90$term <- factor(daily_resp_q90$term, levels = factor_order)

png("figs/daily_resp_q90.png", width=3000, height=1000, res=300)
ggplot(daily_resp_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site")
dev.off()

daily_ent_q90$log_pvalue <- log(round( daily_ent_q90$p_value, 2))
daily_ent_q90$log_pvalue[which(is.infinite(daily_ent_q90$log_pvalue))] <- -5.99
daily_ent_q90$term <- factor(daily_ent_q90$term, levels = factor_order)

png("figs/daily_ent_q90.png", width=3000, height=1000, res=300)
ggplot(daily_ent_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site")
dev.off()


write.csv(daily_resp_q90, "data/daily_resp_q90.csv")
write.csv(daily_ent_q90, "data/daily_ent_q90.csv")

# Unique Mean threshold outlier model -------------------------------------------------

unique_resp_mean_model <- glm(unique_resp_mean_outlier ~ participant_age + sex + site, 
                             data = contacts_resp_ent, family = binomial())
unique_resp_mean <- as.data.frame(summary(unique_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_mean_model <- glm(unique_ent_mean_outlier ~ participant_age + sex + site, 
                            data = contacts_resp_ent, family = binomial())
unique_ent_mean <- as.data.frame(summary(unique_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

# Unique Q75 threshold outlier model -------------------------------------------------

unique_resp_q75_model <- glm(unique_resp_q75_outlier ~ participant_age + sex + site, 
                            data = contacts_resp_ent, family = binomial())
unique_resp_q75 <- as.data.frame(summary(unique_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q75_model <- glm(unique_ent_q75_outlier ~ participant_age + sex + site, 
                           data = contacts_resp_ent, family = binomial())
unique_ent_q75 <- as.data.frame(summary(unique_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

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

names(unique_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(unique_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(unique_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

unique_resp_mean$log_pvalue <- log(round( unique_resp_mean$p_value, 2))
unique_resp_mean$log_pvalue[which(is.infinite(unique_resp_mean$log_pvalue))] <- -5.99
unique_resp_mean$term <- factor(unique_resp_mean$term, levels = factor_order)

png("figs/unique_resp_mean.png", width=3000, height=1000, res=300)
ggplot(unique_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site")
dev.off()

unique_ent_mean$log_pvalue <- log(round( unique_ent_mean$p_value, 2))
unique_ent_mean$log_pvalue[which(is.infinite(unique_ent_mean$log_pvalue))] <- -5.99
unique_ent_mean$term <- factor(unique_ent_mean$term, levels = factor_order)

png("figs/unique_ent_mean.png", width=3000, height=1000, res=300)
ggplot(unique_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site")
dev.off()

unique_resp_q75$log_pvalue <- log(round( unique_resp_q75$p_value, 2))
unique_resp_q75$log_pvalue[which(is.infinite(unique_resp_q75$log_pvalue))] <- -5.99
unique_resp_q75$term <- factor(unique_resp_q75$term, levels = factor_order)

png("figs/unique_resp_q75.png", width=3000, height=1000, res=300)
ggplot(unique_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site")
dev.off()

unique_ent_q75$log_pvalue <- log(round( unique_ent_q75$p_value, 2))
unique_ent_q75$log_pvalue[which(is.infinite(unique_ent_q75$log_pvalue))] <- -5.99
unique_ent_q75$term <- factor(unique_ent_q75$term, levels = factor_order)

png("figs/unique_ent_q75.png", width=3000, height=1000, res=300)
ggplot(unique_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site")
dev.off()


write.csv(unique_resp_q75, "data/unique_resp_q75.csv")
write.csv(unique_ent_q75, "data/unique_ent_q75.csv")


unique_resp_q90$log_pvalue <- log(round( unique_resp_q90$p_value, 2))
unique_resp_q90$log_pvalue[which(is.infinite(unique_resp_q90$log_pvalue))] <- -5.99
unique_resp_q90$term <- factor(unique_resp_q90$term, levels = factor_order)

png("figs/unique_resp_q90.png", width=3000, height=1000, res=300)
ggplot(unique_resp_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
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
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
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
                                    if_else(contacts_resp_ent$hh_occupants > 3, "4-6",
                                            "0-3"))
contacts_resp_ent$hhsize <- factor(contacts_resp_ent$hhsize, 
                                   levels = c("0-3", "4-6", "7+"))

barplot(prop.table(table(contacts_resp_ent$hhsize)))

## Daily Mean threshold outlier model ------------------------------------------

daily_resp_mean_model <- glm(daily_resp_mean_outlier ~ participant_age + sex + site + hhsize, 
                             data = contacts_resp_ent, family = binomial())
daily_resp_mean <- as.data.frame(summary(daily_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_mean_model <- glm(daily_ent_mean_outlier ~ participant_age + sex + site + hhsize, 
                            data = contacts_resp_ent, family = binomial())
daily_ent_mean <- as.data.frame(summary(daily_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Daily Q75 threshold outlier model -------------------------------------------------

daily_resp_q75_model <- glm(daily_resp_q75_outlier ~ participant_age + sex + site + hhsize, 
                            data = contacts_resp_ent, family = binomial())
daily_resp_q75 <- as.data.frame(summary(daily_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q75_model <- glm(daily_ent_q75_outlier ~ participant_age + sex + site + hhsize, 
                           data = contacts_resp_ent, family = binomial())
daily_ent_q75 <- as.data.frame(summary(daily_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Daily Q90 threshold outlier model -------------------------------------------------

daily_resp_q90_model <- glm(daily_resp_q90_outlier ~ participant_age + sex + site + hhsize, 
                            data = contacts_resp_ent, family = binomial())
daily_resp_q90 <- as.data.frame(summary(daily_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q90_model <- glm(daily_ent_q90_outlier ~ participant_age + sex + site + hhsize, 
                           data = contacts_resp_ent, family = binomial())
daily_ent_q90 <- as.data.frame(summary(daily_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Daily figures --------------------------------------------------------
factor_order = c("(Intercept)",
                 "participant_age6-11mo", "participant_age1-4y", 
                 "participant_age5-9y", "participant_age10-14y", 
                 "participant_age15-19y", "participant_age20-29y", 
                 "participant_age30-39y", "participant_age40-59y", 
                 "participant_age60+y", 
                 "sexMale", "siteUrban", 
                 "hhsize4-6", "hhsize7+")

names(daily_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

daily_resp_mean$log_pvalue <- log(round( daily_resp_mean$p_value, 2))
daily_resp_mean$log_pvalue[which(is.infinite(daily_resp_mean$log_pvalue))] <- -5.99
daily_resp_mean$term <- factor(daily_resp_mean$term, levels = factor_order)

# png("figs/daily_hhs_resp_mean.png", width=3000, height=1000, res=300)
# ggplot(daily_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site + HH Size")
# dev.off()

daily_ent_mean$log_pvalue <- log(round( daily_ent_mean$p_value, 2))
daily_ent_mean$log_pvalue[which(is.infinite(daily_ent_mean$log_pvalue))] <- -5.99
daily_ent_mean$term <- factor(daily_ent_mean$term, levels = factor_order)

# png("figs/daily_hhs_ent_mean.png", width=3000, height=1000, res=300)
# ggplot(daily_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site + HH Size")
# dev.off()

daily_resp_q75$log_pvalue <- log(round( daily_resp_q75$p_value, 2))
daily_resp_q75$log_pvalue[which(is.infinite(daily_resp_q75$log_pvalue))] <- -5.99
daily_resp_q75$term <- factor(daily_resp_q75$term, levels = factor_order)

# png("figs/daily_hhs_resp_q75.png", width=3000, height=1000, res=300)
# ggplot(daily_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site + HH Size")
# dev.off()

daily_ent_q75$log_pvalue <- log(round( daily_ent_q75$p_value, 2))
daily_ent_q75$log_pvalue[which(is.infinite(daily_ent_q75$log_pvalue))] <- -5.99
daily_ent_q75$term <- factor(daily_ent_q75$term, levels = factor_order)

# png("figs/daily_hhs_ent_q75.png", width=3000, height=1000, res=300)
# ggplot(daily_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site + HH Size")
# dev.off()


write.csv(daily_resp_q75, "data/daily_hhs_resp_q75.csv")
write.csv(daily_ent_q75, "data/daily_hhs_ent_q75.csv")


daily_resp_q90$log_pvalue <- log(round( daily_resp_q90$p_value, 2))
daily_resp_q90$log_pvalue[which(is.infinite(daily_resp_q90$log_pvalue))] <- -5.99
daily_resp_q90$term <- factor(daily_resp_q90$term, levels = factor_order)

png("figs/daily_hhs_resp_q90.png", width=3000, height=1000, res=300)
ggplot(daily_resp_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site + HH Size")+
  ylim(-7, 7)
dev.off()

daily_ent_q90$log_pvalue <- log(round( daily_ent_q90$p_value, 2))
daily_ent_q90$log_pvalue[which(is.infinite(daily_ent_q90$log_pvalue))] <- -5.99
daily_ent_q90$term <- factor(daily_ent_q90$term, levels = factor_order)

png("figs/daily_hhs_ent_q90.png", width=3000, height=1000, res=300)
ggplot(daily_ent_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site + HH Size")+
  ylim(-7, 7)
dev.off()


write.csv(daily_resp_q90, "data/daily_hhs_resp_q90.csv")
write.csv(daily_ent_q90, "data/daily_hhs_ent_q90.csv")

## Unique Mean threshold outlier model -------------------------------------------------

unique_resp_mean_model <- glm(unique_resp_mean_outlier ~ participant_age + sex + site + hhsize, 
                              data = contacts_resp_ent, family = binomial())
unique_resp_mean <- as.data.frame(summary(unique_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_mean_model <- glm(unique_ent_mean_outlier ~ participant_age + sex + site + hhsize, 
                             data = contacts_resp_ent, family = binomial())
unique_ent_mean <- as.data.frame(summary(unique_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Unique Q75 threshold outlier model -------------------------------------------------

unique_resp_q75_model <- glm(unique_resp_q75_outlier ~ participant_age + sex + site + hhsize, 
                             data = contacts_resp_ent, family = binomial())
unique_resp_q75 <- as.data.frame(summary(unique_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q75_model <- glm(unique_ent_q75_outlier ~ participant_age + sex + site + hhsize, 
                            data = contacts_resp_ent, family = binomial())
unique_ent_q75 <- as.data.frame(summary(unique_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Unique Q90 threshold outlier model -------------------------------------------------

unique_resp_q90_model <- glm(unique_resp_q90_outlier ~ participant_age + sex + site + hhsize, 
                             data = contacts_resp_ent, family = binomial())
unique_resp_q90 <- as.data.frame(summary(unique_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q90_model <- glm(unique_ent_q90_outlier ~ participant_age + sex + site + hhsize, 
                            data = contacts_resp_ent, family = binomial())
unique_ent_q90 <- as.data.frame(summary(unique_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


## Unique figures --------------------------------------------------------

names(unique_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(unique_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(unique_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

unique_resp_mean$log_pvalue <- log(round( unique_resp_mean$p_value, 2))
unique_resp_mean$log_pvalue[which(is.infinite(unique_resp_mean$log_pvalue))] <- -5.99
unique_resp_mean$term <- factor(unique_resp_mean$term, levels = factor_order)

# png("figs/unique_hhs_resp_mean.png", width=3000, height=1000, res=300)
# ggplot(unique_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + HH Size")
# dev.off()

unique_ent_mean$log_pvalue <- log(round( unique_ent_mean$p_value, 2))
unique_ent_mean$log_pvalue[which(is.infinite(unique_ent_mean$log_pvalue))] <- -5.99
unique_ent_mean$term <- factor(unique_ent_mean$term, levels = factor_order)

# png("figs/unique_hhs_ent_mean.png", width=3000, height=1000, res=300)
# ggplot(unique_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + HH Size")
# dev.off()

unique_resp_q75$log_pvalue <- log(round( unique_resp_q75$p_value, 2))
unique_resp_q75$log_pvalue[which(is.infinite(unique_resp_q75$log_pvalue))] <- -5.99
unique_resp_q75$term <- factor(unique_resp_q75$term, levels = factor_order)

# png("figs/unique_hhs_resp_q75.png", width=3000, height=1000, res=300)
# ggplot(unique_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + HH Size")
# dev.off()

unique_ent_q75$log_pvalue <- log(round( unique_ent_q75$p_value, 2))
unique_ent_q75$log_pvalue[which(is.infinite(unique_ent_q75$log_pvalue))] <- -5.99
unique_ent_q75$term <- factor(unique_ent_q75$term, levels = factor_order)

# png("figs/unique_hhs_ent_q75.png", width=3000, height=1000, res=300)
# ggplot(unique_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + HH Size")
# dev.off()


write.csv(unique_resp_q75, "data/unique_hhs_resp_q75.csv")
write.csv(unique_ent_q75, "data/unique_hhs_ent_q75.csv")

unique_resp_q90$Coefficient = c("(Intercept)","6-11mo", "1-4y", 
                               "5-9y", "10-14y", "15-19y", "20-29y", 
                               "30-39y", "40-59y", "60+y", 
                               "Male", "Urban", "4-6", "7+")
unique_resp_q90$log_pvalue <- log(round( unique_resp_q90$p_value, 2))
unique_resp_q90$log_pvalue[which(is.infinite(unique_resp_q90$log_pvalue))] <- -5.99
unique_resp_q90$term <- factor(unique_resp_q90$term, factor_order)
unique_resp_q90$Coefficient <- factor(unique_resp_q90$Coefficient,
                                      levels = unique_resp_q90$Coefficient)
unique_resp_q90 <- unique_resp_q90[-1,]
unique_resp_q90$group = c(rep("Age \n(Ref: <6 months)", 9), 
                        "Sex \n(Ref: Female)", 
                        "Site \n(Ref: Rural)",
                        rep("Household Size \n(Ref: 0-3)", 2))

png("figs/unique_hhs_resp_q90.png", width=4000, height=2000, res=300)
ggplot(unique_resp_q90, aes(x = factor(paste0(Coefficient, "&", group), 
                                       level=paste0(Coefficient, "&", group)), 
                            y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  # theme(axis.text.x = element_text(angle = 45))+
  theme(axis.text = element_text(size = 12))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ylab("Estimate")+
  xlab("Coefficient")+
  guides(x = ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + HH Size")+
  ylim(-7, 7)
dev.off()

unique_ent_q90$Coefficient = c("(Intercept)","6-11mo", "1-4y", 
                                  "5-9y", "10-14y", "15-19y", "20-29y", 
                                  "30-39y", "40-59y", "60+y", 
                               "Male", "Urban", "4-6", "7+")
unique_ent_q90$log_pvalue <- log(round( unique_ent_q90$p_value, 2))
unique_ent_q90$log_pvalue[which(is.infinite(unique_ent_q90$log_pvalue))] <- -5.99
unique_ent_q90$term <- factor(unique_ent_q90$term, levels = factor_order)
unique_ent_q90$Coefficient <- factor(unique_ent_q90$Coefficient, 
                                      levels = unique_ent_q90$Coefficient)
unique_ent_q90 <- unique_ent_q90[-1,]
unique_ent_q90$group = c(rep("Age \n(Ref: <6 months)", 9), 
                            "Sex \n(Ref: Female)", 
                            "Site \n(Ref: Rural)",
                        rep("Household Size \n(Ref: 0-3)", 2))

png("figs/unique_hhs_ent_q90.png", width=4000, height=2000, res=300)
ggplot(unique_ent_q90, aes(x = factor(paste0(Coefficient, "&", group), 
                                      level=paste0(Coefficient, "&", group)), 
                           y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_hline(yintercept = 0)+
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  # theme(axis.text.x = element_text(angle = 45))+
  theme(axis.text = element_text(size = 12))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ylab("Estimate")+
  xlab("Coefficient")+
  guides(x = ggh4x::guide_axis_nested(delim = "&"))+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + HH Size")+
  ylim(-7, 7)
dev.off()


write.csv(unique_resp_q90, "data/unique_hhs_resp_q90.csv")
write.csv(unique_ent_q90, "data/unique_hhs_ent_q90.csv")

# With Occupation --------------------------------------------------------------

adults_contacts_resp_ent <- contacts_resp_ent %>% filter(age >= 20, participant_age != "<6mo")
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Child", "Fisherman"))] <- "Other"
adults_contacts_resp_ent$occupation[which(adults_contacts_resp_ent$occupation %in% 
                                            c("Retired"))] <- "Unemployed"
table(adults_contacts_resp_ent$occupation)
table(adults_contacts_resp_ent$participant_age)

## Daily Mean threshold outlier model ------------------------------------------

daily_resp_mean_model <- glm(daily_resp_mean_outlier ~ participant_age + sex + site + occupation, 
                             data = adults_contacts_resp_ent, family = binomial())
daily_resp_mean <- as.data.frame(summary(daily_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_mean_model <- glm(daily_ent_mean_outlier ~ participant_age + sex + site + occupation, 
                            data = adults_contacts_resp_ent, family = binomial())
daily_ent_mean <- as.data.frame(summary(daily_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Daily Q75 threshold outlier model -------------------------------------------------

daily_resp_q75_model <- glm(daily_resp_q75_outlier ~ participant_age + sex + site + occupation, 
                            data = adults_contacts_resp_ent, family = binomial())
daily_resp_q75 <- as.data.frame(summary(daily_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q75_model <- glm(daily_ent_q75_outlier ~ participant_age + sex + site + occupation, 
                           data = adults_contacts_resp_ent, family = binomial())
daily_ent_q75 <- as.data.frame(summary(daily_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Daily Q90 threshold outlier model -------------------------------------------------

daily_resp_q90_model <- glm(daily_resp_q90_outlier ~ participant_age + sex + site + occupation, 
                            data = adults_contacts_resp_ent, family = binomial())
daily_resp_q90 <- as.data.frame(summary(daily_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

daily_ent_q90_model <- glm(daily_ent_q90_outlier ~ participant_age + sex + site + occupation, 
                           data = adults_contacts_resp_ent, family = binomial())
daily_ent_q90 <- as.data.frame(summary(daily_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Daily figures --------------------------------------------------------
factor_order = c("(Intercept)", 
                 "participant_age30-39y", "participant_age40-59y", 
                 "participant_age60+y", "sexMale", "siteUrban",
                 "occupationStudent", "occupationFarmer",
                 "occupationBusiness person", "occupationOffice worker",
                 "occupationCasual laboror", "occupationHomemaker", 
                 "occupationOther")

names(daily_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(daily_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(daily_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

daily_resp_mean$log_pvalue <- log(round( daily_resp_mean$p_value, 2))
daily_resp_mean$log_pvalue[which(is.infinite(daily_resp_mean$log_pvalue))] <- -5.99
daily_resp_mean$term <- factor(daily_resp_mean$term, levels = factor_order)

# png("figs/daily_occ_resp_mean.png", width=3000, height=1000, res=300)
# ggplot(daily_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")
# dev.off()

daily_ent_mean$log_pvalue <- log(round( daily_ent_mean$p_value, 2))
daily_ent_mean$log_pvalue[which(is.infinite(daily_ent_mean$log_pvalue))] <- -5.99
daily_ent_mean$term <- factor(daily_ent_mean$term, levels = factor_order)

# png("figs/daily_occ_ent_mean.png", width=3000, height=1000, res=300)
# ggplot(daily_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site + Occupation")
# dev.off()

daily_resp_q75$log_pvalue <- log(round( daily_resp_q75$p_value, 2))
daily_resp_q75$log_pvalue[which(is.infinite(daily_resp_q75$log_pvalue))] <- -5.99
daily_resp_q75$term <- factor(daily_resp_q75$term, levels = factor_order)

# png("figs/daily_occ_resp_q75.png", width=3000, height=1000, res=300)
# ggplot(daily_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")
# dev.off()

daily_ent_q75$log_pvalue <- log(round( daily_ent_q75$p_value, 2))
daily_ent_q75$log_pvalue[which(is.infinite(daily_ent_q75$log_pvalue))] <- -5.99
daily_ent_q75$term <- factor(daily_ent_q75$term, levels = factor_order)

# png("figs/daily_occ_ent_q75.png", width=3000, height=1000, res=300)
# ggplot(daily_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site + Occupation")
# dev.off()


write.csv(daily_resp_q75, "data/daily_occ_resp_q75.csv")
write.csv(daily_ent_q75, "data/daily_occ_ent_q75.csv")


daily_resp_q90$log_pvalue <- log(round( daily_resp_q90$p_value, 2))
daily_resp_q90$log_pvalue[which(is.infinite(daily_resp_q90$log_pvalue))] <- -5.99
daily_resp_q90$term <- factor(daily_resp_q90$term, levels = factor_order)

png("figs/daily_occ_resp_q90.png", width=3000, height=1000, res=300)
ggplot(daily_resp_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Daily Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")+
  ylim(-7, 7)
dev.off()

daily_ent_q90$log_pvalue <- log(round( daily_ent_q90$p_value, 2))
daily_ent_q90$log_pvalue[which(is.infinite(daily_ent_q90$log_pvalue))] <- -5.99
daily_ent_q90$term <- factor(daily_ent_q90$term, levels = factor_order)

png("figs/daily_occ_ent_q90.png", width=3000, height=1000, res=300)
ggplot(daily_ent_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Daily Avg Enteric Contacts ~ Age + Sex + Site + Occupation")+
  ylim(-7, 7)
dev.off()


write.csv(daily_resp_q90, "data/daily_occ_resp_q90.csv")
write.csv(daily_ent_q90, "data/daily_occ_ent_q90.csv")

## Unique Mean threshold outlier model -------------------------------------------------

unique_resp_mean_model <- glm(unique_resp_mean_outlier ~ participant_age + sex + site + occupation, 
                              data = adults_contacts_resp_ent, family = binomial())
unique_resp_mean <- as.data.frame(summary(unique_resp_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_mean_model <- glm(unique_ent_mean_outlier ~ participant_age + sex + site + occupation, 
                             data = adults_contacts_resp_ent, family = binomial())
unique_ent_mean <- as.data.frame(summary(unique_ent_mean_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Unique Q75 threshold outlier model -------------------------------------------------

unique_resp_q75_model <- glm(unique_resp_q75_outlier ~ participant_age + sex + site + occupation, 
                             data = adults_contacts_resp_ent, family = binomial())
unique_resp_q75 <- as.data.frame(summary(unique_resp_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q75_model <- glm(unique_ent_q75_outlier ~ participant_age + sex + site + occupation, 
                            data = adults_contacts_resp_ent, family = binomial())
unique_ent_q75 <- as.data.frame(summary(unique_ent_q75_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

## Unique Q90 threshold outlier model -------------------------------------------------

unique_resp_q90_model <- glm(unique_resp_q90_outlier ~ participant_age + sex + site + occupation, 
                             data = adults_contacts_resp_ent, family = binomial())
unique_resp_q90 <- as.data.frame(summary(unique_resp_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()

unique_ent_q90_model <- glm(unique_ent_q90_outlier ~ participant_age + sex + site + occupation, 
                            data = adults_contacts_resp_ent, family = binomial())
unique_ent_q90 <- as.data.frame(summary(unique_ent_q90_model)$coefficients) %>%
  as.data.frame() %>%
  tibble::rownames_to_column()


## Unique figures --------------------------------------------------------

names(unique_resp_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_mean) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(unique_resp_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q75) <- c("term", "estimate", "SE", "test_statistic", "p_value")

names(unique_resp_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")
names(unique_ent_q90) <- c("term", "estimate", "SE", "test_statistic", "p_value")

unique_resp_mean$log_pvalue <- log(round( unique_resp_mean$p_value, 2))
unique_resp_mean$log_pvalue[which(is.infinite(unique_resp_mean$log_pvalue))] <- -5.99
unique_resp_mean$term <- factor(unique_resp_mean$term, levels = factor_order)

# png("figs/unique_occ_resp_mean.png", width=3000, height=1000, res=300)
# ggplot(unique_resp_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")
# dev.off()

unique_ent_mean$log_pvalue <- log(round( unique_ent_mean$p_value, 2))
unique_ent_mean$log_pvalue[which(is.infinite(unique_ent_mean$log_pvalue))] <- -5.99
unique_ent_mean$term <- factor(unique_ent_mean$term, levels = factor_order)

# png("figs/unique_occ_ent_mean.png", width=3000, height=1000, res=300)
# ggplot(unique_ent_mean, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + Occupation")
# dev.off()

unique_resp_q75$log_pvalue <- log(round( unique_resp_q75$p_value, 2))
unique_resp_q75$log_pvalue[which(is.infinite(unique_resp_q75$log_pvalue))] <- -5.99
unique_resp_q75$term <- factor(unique_resp_q75$term, levels = factor_order)

# png("figs/unique_occ_resp_q75.png", width=3000, height=1000, res=300)
# ggplot(unique_resp_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")
# dev.off()

unique_ent_q75$log_pvalue <- log(round( unique_ent_q75$p_value, 2))
unique_ent_q75$log_pvalue[which(is.infinite(unique_ent_q75$log_pvalue))] <- -5.99
unique_ent_q75$term <- factor(unique_ent_q75$term, levels = factor_order)

# png("figs/unique_occ_ent_q75.png", width=3000, height=1000, res=300)
# ggplot(unique_ent_q75, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + Occupation")
# dev.off()


write.csv(unique_resp_q75, "data/unique_occ_resp_q75.csv")
write.csv(unique_ent_q75, "data/unique_occ_ent_q75.csv")


unique_resp_q90$log_pvalue <- log(round( unique_resp_q90$p_value, 2))
unique_resp_q90$log_pvalue[which(is.infinite(unique_resp_q90$log_pvalue))] <- -5.99
unique_resp_q90$term <- factor(unique_resp_q90$term, levels = factor_order)

png("figs/unique_occ_resp_q90.png", width=3000, height=1000, res=300)
ggplot(unique_resp_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Respiratory Contacts ~ Age + Sex + Site + Occupation")+
  ylim(-7, 7)
dev.off()

unique_ent_q90$log_pvalue <- log(round( unique_ent_q90$p_value, 2))
unique_ent_q90$log_pvalue[which(is.infinite(unique_ent_q90$log_pvalue))] <- -5.99
unique_ent_q90$term <- factor(unique_ent_q90$term, levels = factor_order)

png("figs/unique_occ_ent_q90.png", width=3000, height=1000, res=300)
ggplot(unique_ent_q90, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.95))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Enteric Contacts ~ Age + Sex + Site + Occupation")+
  ylim(-7, 7)
dev.off()


write.csv(unique_resp_q90, "data/unique_occ_resp_q90.csv")
write.csv(unique_ent_q90, "data/unique_occ_ent_q90.csv")



