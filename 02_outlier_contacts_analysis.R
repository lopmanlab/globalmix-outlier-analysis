rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(GGally)
library(scales)
library(ggpubr)
library(lme4)
require(MASS)
library(lmerTest)

# Read in Data -----------------------------------------------------------------
unique <- read.csv(here("data/contacts_unique.csv"))
daily <- read.csv(here("data/contacts_daily.csv"))

contact_summaries <- unique %>%
  distinct() %>%
  group_by() %>%
  dplyr::summarize(unique_q50 = quantile(avg_unique_contacts, probs = 0.5),
                   unique_q75 = quantile(avg_unique_contacts, probs = 0.75),
                   unique_q90 = quantile(avg_unique_contacts, probs = 0.90),
                   unique_mean = mean(avg_unique_contacts)) %>%
  cbind(daily %>%
          distinct() %>%
          ungroup() %>%
          dplyr::summarize(daily_q50 = quantile(avg_daily_contacts, probs = 0.5),
                           daily_q75 = quantile(avg_daily_contacts, probs = 0.75),
                           daily_q90 = quantile(avg_daily_contacts, probs = 0.90),
                           daily_mean = mean(avg_daily_contacts))
        )

daily$outlier_mean <- ifelse(daily$avg_daily_contacts > contact_summaries$daily_mean, 
                             1, 0)
daily$outlier_q75 <- ifelse(daily$avg_daily_contacts > contact_summaries$daily_q75, 
                             1, 0)
unique$outlier_mean <- ifelse(unique$avg_unique_contacts > contact_summaries$unique_mean, 
                             1, 0)
unique$outlier_q75 <- ifelse(unique$avg_unique_contacts > contact_summaries$unique_q75, 
                            1, 0)
unique$outlier_q90 <- ifelse(unique$avg_unique_contacts > contact_summaries$unique_q90, 
                             1, 0)
daily$age <- as.numeric(daily$age)
unique$age <- as.numeric(unique$age)

# Daily Mean threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_mean ~ participant_age + sex + site, 
                      data = daily, family = binomial())
mean_daily_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_mean ~ participant_age * sex * site, data = daily, 
                          family = binomial())
mean_daily_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Daily Q75 threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_q75 ~ participant_age + sex + site, 
                      data = daily, family = binomial())
q75_daily_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_q75 ~ participant_age * sex * site, 
                          data = daily, family = binomial())
q75_daily_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Unique Mean threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_mean ~ participant_age + sex + site, 
                      data = unique, family = binomial(link=logit))
mean_unique_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_mean ~ participant_age * sex * site, 
                          data = unique, family = binomial())
mean_unique_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Unique Q75 threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_q75 ~ participant_age + sex + site, 
                      data = unique, family = binomial())
q75_unique_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_q75 ~ participant_age * sex * site, 
                          data = unique, family = binomial())
q75_unique_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# Unique Q90 threshold outlier model -------------------------------------------------

logistic_model <- glm(outlier_q90 ~ participant_age + sex + site, 
                      data = unique, family = binomial())
q90_unique_logistic <- as.data.frame(summary(logistic_model)$coefficients)

logistic_int_model <- glm(outlier_q90 ~ participant_age * sex * site, 
                          data = unique, family = binomial())
q90_unique_logistic_int <- as.data.frame(summary(logistic_int_model)$coefficients)

# No interaction logistic plots---------------------------------------------------
factor_order = c("(Intercept)","participant_age6-11mo", "participant_age1-4y", 
                 "participant_age5-9y", "participant_age10-14y", 
                 "participant_age15-19y", "participant_age20-29y", 
                 "participant_age30-39y", "participant_age40-59y", 
                 "participant_age60+y", "sexMale", "siteUrban")

mean_daily_logistic <- mean_daily_logistic %>% tibble::rownames_to_column()
names(mean_daily_logistic) <- c("term", "estimate", "SE", "test_statistic", "p_value")
mean_daily_logistic_int <- mean_daily_logistic_int %>% tibble::rownames_to_column()
names(mean_daily_logistic_int) <- names(mean_daily_logistic)
mean_unique_logistic <- mean_unique_logistic %>% tibble::rownames_to_column()
names(mean_unique_logistic) <- names(mean_daily_logistic)
mean_unique_logistic_int <- mean_unique_logistic_int %>% tibble::rownames_to_column()
names(mean_unique_logistic_int) <- names(mean_daily_logistic)

mean_daily_logistic$log_pvalue <- log(round(mean_daily_logistic$p_value, 2))
mean_daily_logistic$log_pvalue[which(is.infinite(mean_daily_logistic$log_pvalue))] <- -5.99
mean_daily_logistic$term <- factor(mean_daily_logistic$term, levels = factor_order)

png("figs/mean_daily_logistic.png", width=2500, height=1000, res=300)
ggplot(mean_daily_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Daily Avg Contacts ~ Age + Sex + Site")
dev.off()

mean_unique_logistic$log_pvalue <- log(round(mean_unique_logistic$p_value, 2))
mean_unique_logistic$log_pvalue[which(is.infinite(mean_unique_logistic$log_pvalue))] <- -5.99
mean_unique_logistic$term <- factor(mean_unique_logistic$term, levels = factor_order)

png("figs/mean_unique_logistic.png", width=3000, height=1000, res=300)
ggplot(mean_unique_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site")
dev.off()

# No interaction Q75 plots---------------------------------------------------
q75_daily_logistic <- q75_daily_logistic %>% tibble::rownames_to_column()
names(q75_daily_logistic) <- c("term", "estimate", "SE", "test_statistic", "p_value")
q75_daily_logistic_int <- q75_daily_logistic_int %>% tibble::rownames_to_column()
names(q75_daily_logistic_int) <- names(q75_daily_logistic)
q75_unique_logistic <- q75_unique_logistic %>% tibble::rownames_to_column()
names(q75_unique_logistic) <- names(q75_daily_logistic)
q75_unique_logistic_int <- q75_unique_logistic_int %>% tibble::rownames_to_column()
names(q75_unique_logistic_int) <- names(q75_daily_logistic)
q90_unique_logistic <- q90_unique_logistic %>% tibble::rownames_to_column()
names(q90_unique_logistic) <- names(q75_daily_logistic)

q75_daily_logistic$log_pvalue <- log(round(q75_daily_logistic$p_value, 2))
q75_daily_logistic$log_pvalue[which(is.infinite(q75_daily_logistic$log_pvalue))] <- -5.99
q75_daily_logistic$term <- factor(q75_daily_logistic$term, levels = factor_order)

png("figs/q75_daily_logistic.png", width=3000, height=1000, res=300)
ggplot(q75_daily_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradient2(low = "forestgreen",
                         mid = "goldenrod1",
                         high = "firebrick", 
                         midpoint=log(0.05),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Daily Avg Contacts ~ Age + Sex + Site")
dev.off()

q75_unique_logistic$log_pvalue <- log(round(q75_unique_logistic$p_value, 2))
q75_unique_logistic$log_pvalue[which(is.infinite(q75_unique_logistic$log_pvalue))] <- -5.99
q75_unique_logistic$term <- factor(q75_unique_logistic$term, levels = factor_order)

png("figs/q75_unique_logistic.png", width=3000, height=1000, res=300)
ggplot(q75_unique_logistic, aes(x = term, y = estimate, color = log_pvalue)) + 
  geom_point() +
  geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
  theme(axis.text.x = element_text(angle = 45))+
  scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
                         values = rescale(c(0.01,0.05,0.1)),
                         guide = "colorbar")+
  ggtitle("Logistic: Q75 Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site")
dev.off()

q90_unique_logistic$Coefficient = c("(Intercept)","6-11mo", "1-4y", 
                              "5-9y", "10-14y", "15-19y", "20-29y", 
                              "30-39y", "40-59y", "60+y", "Male", "Urban")
q90_unique_logistic$log_pvalue <- log(round(q90_unique_logistic$p_value, 2))
q90_unique_logistic$log_pvalue[which(is.infinite(q90_unique_logistic$log_pvalue))] <- -5.99
q90_unique_logistic$term <- factor(q90_unique_logistic$term, levels = factor_order)
q90_unique_logistic$Coefficient <- factor(q90_unique_logistic$Coefficient, 
                                          levels = q90_unique_logistic$Coefficient)
q90_unique_logistic <- q90_unique_logistic[-1,]
q90_unique_logistic$group = c(rep("Age \n(Ref: <6 months)", 9), 
                              "Sex \n(Ref: Female)", 
                              "Site \n(Ref: Rural)")

png("figs/q90_unique_logistic.png", width=4000, height=1500, res=300)
ggplot(q90_unique_logistic, aes(x = factor(paste0(Coefficient, "&", group), 
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
  ggtitle("Logistic: Q90 Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site")
dev.off()


# Interaction Logistic plots------------------------------------------------------
# mean_daily_logistic_int$facet <- c(rep("Main", 90),
#                                  rep("Age*Sex", 80),
#                                  rep("Age*Site & Sex*Site", 73),
#                                  rep("Age*Sex*Site", 56)) ->
#   mean_unique_logistic_int$facet 

# mean_daily_logistic_int$log_pvalue <- log(round(mean_daily_logistic_int$p_value, 2))
# mean_daily_logistic_int$log_pvalue[which(is.infinite(mean_daily_logistic_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(mean_daily_logistic_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(mean_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(mean_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(mean_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/mean_daily_logistic_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()
# 
# mean_unique_logistic_int$log_pvalue <- log(round(mean_unique_logistic_int$p_value, 2))
# mean_unique_logistic_int$log_pvalue[which(is.infinite(mean_unique_logistic_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(mean_unique_logistic_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(mean_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(mean_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(mean_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/mean_unique_logistic_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()

# Interaction Q75 Logistic plots------------------------------------------------------
# q75_daily_logistic_int$facet <- c(rep("Main", 90),
#                                    rep("Age*Sex", 80),
#                                    rep("Age*Site & Sex*Site", 73),
#                                    rep("Age*Sex*Site", 56)) ->
#   q75_unique_logistic_int$facet 

# q75_daily_logistic_int$log_pvalue <- log(round(q75_daily_logistic_int$p_value, 2))
# q75_daily_logistic_int$log_pvalue[which(is.infinite(q75_daily_logistic_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(q75_daily_logistic_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(q75_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(q75_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(q75_daily_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/q75_daily_logistic_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()
# 
# q75_unique_logistic_int$log_pvalue <- log(round(q75_unique_logistic_int$p_value, 2))
# q75_unique_logistic_int$log_pvalue[which(is.infinite(q75_unique_logistic_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(q75_unique_logistic_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Logistic: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(q75_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(q75_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(q75_unique_logistic_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/q75_unique_logistic_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()

# Daily Mean threshold outlier model -------------------------------------------------

poisson_model <- glm(outlier_mean ~ participant_age + sex + site, 
                      data = daily, family = poisson(link="log"))
mean_daily_poisson <- as.data.frame(summary(poisson_model)$coefficients)

poisson_int_model <- glm(outlier_mean ~ participant_age * sex * site, data = daily)
mean_daily_poisson_int <- as.data.frame(summary(poisson_int_model)$coefficients)

# Daily Q75 threshold outlier model -------------------------------------------------

poisson_model <- glm(outlier_q75 ~ participant_age + sex + site, 
                      data = daily, family = poisson(link="log"))
q75_daily_poisson <- as.data.frame(summary(poisson_model)$coefficients)

poisson_int_model <- glm(outlier_q75 ~ participant_age * sex * site, 
                          data = daily, family = poisson(link="log"))
q75_daily_poisson_int <- as.data.frame(summary(poisson_int_model)$coefficients)

# Unique Mean threshold outlier model -------------------------------------------------

poisson_model <- glm(outlier_mean ~ participant_age + sex + site, 
                      data = unique, family = binomial(link=logit))
mean_unique_poisson <- as.data.frame(summary(poisson_model)$coefficients)

poisson_int_model <- glm(outlier_mean ~ participant_age * sex * site, 
                          data = unique, family = poisson(link="log"))
mean_unique_poisson_int <- as.data.frame(summary(poisson_int_model)$coefficients)

# Unique Q75 threshold outlier model -------------------------------------------------

poisson_model <- glm(outlier_q75 ~ participant_age + sex + site, 
                      data = unique, family = poisson(link="log"))
q75_unique_poisson <- as.data.frame(summary(poisson_model)$coefficients)

poisson_int_model <- glm(outlier_q75 ~ participant_age * sex * site, 
                          data = unique, family = poisson(link="log"))
q75_unique_poisson_int <- as.data.frame(summary(poisson_int_model)$coefficients)

# No interaction poisson plots---------------------------------------------------
# mean_daily_poisson <- mean_daily_poisson %>% tibble::rownames_to_column()
# names(mean_daily_poisson) <- c("term", "estimate", "SE", "test_statistic", "p_value")
# mean_daily_poisson_int <- mean_daily_poisson_int %>% tibble::rownames_to_column()
# names(mean_daily_poisson_int) <- names(mean_daily_poisson)
# mean_unique_poisson <- mean_unique_poisson %>% tibble::rownames_to_column()
# names(mean_unique_poisson) <- names(mean_daily_poisson)
# mean_unique_poisson_int <- mean_unique_poisson_int %>% tibble::rownames_to_column()
# names(mean_unique_poisson_int) <- names(mean_daily_poisson)
# 
# mean_daily_poisson$log_pvalue <- log(round(mean_daily_poisson$p_value, 2))
# mean_daily_poisson$log_pvalue[which(is.infinite(mean_daily_poisson$log_pvalue))] <- -5.99
# 
# png("figs/mean_daily_poisson.png", width=3000, height=1000, res=300)
# ggplot(mean_daily_poisson, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradient2(low = "forestgreen",
#                          mid = "goldenrod1",
#                          high = "firebrick", 
#                          midpoint=log(0.05),
#                          guide = "colorbar")+
#   ggtitle("Poisson: Mean Outlier Threshold for Daily Avg Contacts ~ Age + Sex + Site")
# dev.off()
# 
# mean_unique_poisson$log_pvalue <- log(round(mean_unique_poisson$p_value, 2))
# mean_unique_poisson$log_pvalue[which(is.infinite(mean_unique_poisson$log_pvalue))] <- -5.99
# 
# png("figs/mean_unique_poisson.png", width=3000, height=1000, res=300)
# ggplot(mean_unique_poisson, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Poisson: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site")
# dev.off()

# Interaction Mean Poisson plots------------------------------------------------------
# mean_daily_poisson_int$facet <- c(rep("Main", 90),
#                                    rep("Age*Sex", 80),
#                                    rep("Age*Site & Sex*Site", 73),
#                                    rep("Age*Sex*Site", 56)) ->
#   mean_unique_poisson_int$facet 

# mean_daily_poisson_int$log_pvalue <- log(round(mean_daily_poisson_int$p_value, 2))
# mean_daily_poisson_int$log_pvalue[which(is.infinite(mean_daily_poisson_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(mean_daily_poisson_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Poisson: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(mean_daily_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(mean_daily_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(mean_daily_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/mean_daily_poisson_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()
# 
# mean_unique_poisson_int$log_pvalue <- log(round(mean_unique_poisson_int$p_value, 2))
# mean_unique_poisson_int$log_pvalue[which(is.infinite(mean_unique_poisson_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(mean_unique_poisson_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Poisson: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(mean_unique_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(mean_unique_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(mean_unique_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/mean_unique_poisson_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()

# Interaction Q75 Poisson plots-------------------------------------------------

# q75_daily_poisson_int$facet <- c(rep("Main", 90),
#                                   rep("Age*Sex", 80),
#                                   rep("Age*Site & Sex*Site", 73),
#                                   rep("Age*Sex*Site", 56)) ->
#   q75_unique_poisson_int$facet 

# q75_daily_poisson_int$log_pvalue <- log(round(q75_daily_poisson_int$p_value, 2))
# q75_daily_poisson_int$log_pvalue[which(is.infinite(q75_daily_poisson_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(q75_daily_poisson_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Poisson: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(q75_daily_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(q75_daily_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(q75_daily_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/q75_daily_poisson_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()
# 
# q75_unique_poisson_int$log_pvalue <- log(round(q75_unique_poisson_int$p_value, 2))
# q75_unique_poisson_int$log_pvalue[which(is.infinite(q75_unique_poisson_int$log_pvalue))] <- -5.99
# 
# p1 <- ggplot(q75_unique_poisson_int , aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Poisson: Mean Outlier Threshold for Unique Avg Contacts ~ Age + Sex + Site\nMain")
# 
# p2 <- ggplot(q75_unique_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex")
# 
# p3 <- ggplot(q75_unique_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Site & Sex*Site")
# 
# p4 <- ggplot(q75_unique_poisson_int, aes(x = term, y = estimate, color = log_pvalue)) + 
#   geom_point() +
#   geom_errorbar(aes(ymin=estimate-1.96*SE, ymax=estimate+1.96*SE))+
#   theme(axis.text.x = element_text(angle = 45))+
#   scale_colour_gradientn(colours = c("forestgreen","goldenrod1","firebrick"), 
#                          values = rescale(c(0.01,0.05,0.1)),
#                          guide = "colorbar")+
#   ggtitle("Age*Sex*Site")
# 
# 
# png("figs/q75_unique_poisson_int.png", width=3000, height=1000, res=300)
# ggarrange(p1, p2, p3, p4, nrow=4, ncol=1)
# dev.off()

