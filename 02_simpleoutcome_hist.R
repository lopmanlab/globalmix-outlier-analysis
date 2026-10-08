pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry)

simple_outcome_hist <- function(cty = "moz", hhmbr = "", equation = "age + sex + study_site + hh_size_cat + occupation", ref, term){
  
  full_names <- c("Mozambique", "India", "Pakistan", "Guatemala")
  names(full_names) <- c("moz", "ind", "pak", "gt")
  country = full_names[cty][1]
  indices <- c("A) Mozambique", "B) India", "C) Pakistan", "D) Guatemala")
  names(indices) <- c("moz", "ind", "pak", "gt")
  index <- indices[cty][1]
  
  contacts_daily <- read.csv(paste0(here(),"/data/",cty,"/", cty, hhmbr, "_outlier_simple.csv")) %>%
    mutate(avg_daily_nonhh_contacts = as.numeric(avg_daily_nonhh_contacts),
           avg_daily_indtime_contacts = as.numeric(avg_daily_indtime_contacts))
  
  # # Determine if negative binomial distribution is the best for nonhh -----------
  lm_model <- lm(data = contacts_daily, avg_daily_nonhh_contacts ~ 1) # Linear model (mean only)
  pois_model <- glm(data = contacts_daily, avg_daily_nonhh_contacts ~ 1, family = poisson())  # Poisson model
  nb_model <- glm.nb(data = contacts_daily, avg_daily_nonhh_contacts ~ 1) # Negative binomial model
  mu_lm <- coef(lm_model)[1]
  mu_pois <- exp(coef(pois_model)[1])
  mu_nb <- exp(coef(nb_model)[1])
  theta_nb <- nb_model$theta
  
  y_vals <- 0:max(contacts_daily$avg_daily_nonhh_contacts, na.rm = T)  # range of y values to plot
  df <- data.frame(
    y = y_vals,
    Normal = dnorm(y_vals, mean = mu_lm, sd = sd(residuals(lm_model))),
    Poisson = dpois(y_vals, lambda = mu_pois),
    NegBinom = dnbinom(y_vals, size = theta_nb, mu = mu_nb)
  )
  df_long <- pivot_longer(df, cols = -y, names_to = "Dist", values_to = "Density")
  
  # png(filename = paste0(here(), "/figs/histograms/", cty, hhmbr, "_nonhh_hist.png" ), height = 800, width = 1200)
  #print(
  p <- ggplot() +
          geom_histogram(data = contacts_daily, aes(x = avg_daily_nonhh_contacts, y = ..density..),
                         bins = length(unique((contacts_daily$avg_daily_nonhh_contacts))),
                         fill = "gray80", color = "black") +
          # geom_line(data = df_long, aes(x = y, y = Density, color = Dist)) +
          geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_nonhh_contacts, probs = 0.75, na.rm=T)), lty = 2, size=1.5)+
          # geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_nonhh_contacts, probs = 0.80, na.rm=T)), lty = 2, size=1.5)+
          # geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_nonhh_contacts, probs = 0.90, na.rm=T)), lty = 4, size=1.5)+
          theme_bw() +
          # ggtitle(paste0(index))+
          xlab("Number of Non-Household Contacts")+
          theme(
            text = element_text(size = 11),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 10),
            plot.title = element_blank()
          )#)
  # dev.off()
  
  ggsave(
    filename = paste0(here(), "/figs/histograms/", cty, hhmbr, "_nonhh_hist.tiff" ),
    plot = p,
    device = "tiff",
    width = 150, height = 89,        # mm; ~single-column width per many journal templates
    units = "mm",
    dpi = 300,
    compression = "lzw"
  )
  
  # Determine if negative binomial distribution is the best for indtime -----------
  lm_model <- lm(data = contacts_daily, avg_daily_indtime_contacts ~ 1) # Linear model (mean only)
  pois_model <- glm(data = contacts_daily, avg_daily_indtime_contacts ~ 1, family = poisson())  # Poisson model
  nb_model <- glm.nb(data = contacts_daily, avg_daily_indtime_contacts ~ 1) # Negative binomial model
  mu_lm <- coef(lm_model)[1]
  mu_pois <- exp(coef(pois_model)[1])
  mu_nb <- exp(coef(nb_model)[1])
  theta_nb <- nb_model$theta
  
  y_vals <- 0:max(contacts_daily$avg_daily_indtime_contacts, na.rm = T)  # range of y values to plot
  df <- data.frame(
    y = y_vals,
    Normal = dnorm(y_vals, mean = mu_lm, sd = sd(residuals(lm_model))),
    Poisson = dpois(y_vals, lambda = mu_pois),
    NegBinom = dnbinom(y_vals, size = theta_nb, mu = mu_nb)
  )
  df_long <- pivot_longer(df, cols = -y, names_to = "Dist", values_to = "Density")
  
  # png(filename = paste0(here(), "/figs/histograms/", cty, hhmbr, "_indtime_hist.png" ), height = 800, width = 1200)
  #print(
  p <- ggplot() +
          geom_histogram(data = contacts_daily, aes(x = avg_daily_indtime_contacts, y = ..density..),
                         bins = length(unique((contacts_daily$avg_daily_indtime_contacts))),
                         fill = "gray80", color = "black") +
          # geom_line(data = df_long, aes(x = y, y = Density, color = Dist)) +
          geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_indtime_contacts, probs = 0.75, na.rm=T)), lty = 2, size=1.5)+
          # geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_indtime_contacts, probs = 0.80, na.rm=T)), lty = 2, size=1.5)+
          # geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_indtime_contacts, probs = 0.90, na.rm=T)), lty = 4, size=1.5)+
          theme_bw() +
          # ggtitle(index)+
          xlab("Indoor-Hours Spent with Non-Household Contacts")+
          theme(
            text = element_text(size = 11),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 10),
            plot.title = element_blank()
          )#)
  # dev.off()
  
  ggsave(
    filename = paste0(here(), "/figs/histograms/", cty, hhmbr, "_indtime_hist.tiff" ),
    plot = p,
    device = "tiff",
    width = 150, height = 89,        # mm; ~single-column width per many journal templates
    units = "mm",
    dpi = 300,
    compression = "lzw"
  )
  
  # Determine if negative binomial distribution is the best for touch -----------
  lm_model <- lm(data = contacts_daily, avg_daily_touch_contacts ~ 1) # Linear model (mean only)
  pois_model <- glm(data = contacts_daily, avg_daily_touch_contacts ~ 1, family = poisson())  # Poisson model
  nb_model <- glm.nb(data = contacts_daily, avg_daily_touch_contacts ~ 1) # Negative binomial model
  mu_lm <- coef(lm_model)[1]
  mu_pois <- exp(coef(pois_model)[1])
  mu_nb <- exp(coef(nb_model)[1])
  theta_nb <- nb_model$theta
  
  y_vals <- 0:max(contacts_daily$avg_daily_touch_contacts, na.rm = T)  # range of y values to plot
  df <- data.frame(
    y = y_vals,
    Normal = dnorm(y_vals, mean = mu_lm, sd = sd(residuals(lm_model))),
    Poisson = dpois(y_vals, lambda = mu_pois),
    NegBinom = dnbinom(y_vals, size = theta_nb, mu = mu_nb)
  )
  df_long <- pivot_longer(df, cols = -y, names_to = "Dist", values_to = "Density")
  
  # png(filename = paste0(here(), "/figs/histograms/", cty, hhmbr, "_touch_hist.png" ), height = 800, width = 1200)
  #print(
  p <- ggplot() +
          geom_histogram(data = contacts_daily, aes(x = avg_daily_touch_contacts, y = ..density..),
                         bins = length(unique((contacts_daily$avg_daily_touch_contacts))),
                         fill = "gray80", color = "black") +
          # geom_line(data = df_long, aes(x = y, y = Density, color = Dist)) +
          geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_touch_contacts, probs = 0.75, na.rm=T)), lty = 2, size=1.5)+
          # geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_touch_contacts, probs = 0.80, na.rm=T)), lty = 2, size=1.5)+
          # geom_vline(aes(xintercept=quantile(contacts_daily$avg_daily_touch_contacts, probs = 0.90, na.rm=T)), lty = 4, size=1.5)+
          theme_bw() +
          # ggtitle(index)+
          xlab("Number of Non-Household Contacts Involving Touch")+
          theme(
            text = element_text(size = 11),
            axis.title = element_text(size = 11),
            axis.text = element_text(size = 10),
            plot.title = element_blank()
          )#)
  # dev.off()
  
  ggsave(
    filename = paste0(here(), "/figs/histograms/", cty, hhmbr, "_touch_hist.tiff" ),
    plot = p,
    device = "tiff",
    width = 150, height = 89,        # mm; ~single-column width per many journal templates
    units = "mm",
    dpi = 300,
    compression = "lzw"
  )
  
  # Save data --------------------------------------------------------------------
  
  return()
}