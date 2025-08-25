pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry)

get_rr_ci_by_group <- function(model, digits = 2) {
  # Get the actual data used in the model
  model_data <- model.frame(model)
  
  # Remove response variable
  predictors <- model_data[, -1, drop = FALSE]
  
  # Drop duplicate rows so each tested combo appears once
  combos <- unique(predictors)[!grepl(paste0("Child",collapse="|" ),names(unique(predictors)))]
  
  # Build model matrix (same coding as model)
  mm <- model.matrix(delete.response(terms(model)), data = combos)
  
  # Match coefficient names
  beta <- coef(model)
  vcov_mat <- vcov(model)
  mm <- mm[, names(beta), drop = FALSE]
  mm <- mm[,!grepl(paste0("Child",collapse="|" ),names(beta))]
  
  # Log RR, SE, CI
  log_rr <- mm %*% beta[!is.na(beta)]
  se <- sqrt(diag(mm %*% vcov_mat %*% t(mm)))
  
  RR <- exp(log_rr)
  lower_CI <- exp(log_rr - 1.96 * se)
  upper_CI <- exp(log_rr + 1.96 * se)
  
  # Combine results
  results <- cbind(combos, 
                   RR = round(RR, digits),
                   Lower_95_CI = round(lower_CI, digits),
                   Upper_95_CI = round(upper_CI, digits))
  
  # Force the first row to be RR=1 for reference
  results$RR[1] <- 1
  results$Lower_95_CI[1] <- 1
  results$Upper_95_CI[1] <- 1
  
  return(results)
}

total_resp_ent_contacts_regression <- function(cty = "moz", hhmbr = "", equation = "age + sex + study_site + hh_size_cat + occupation", ref, term){
  
  full_names <- c("Mozambique", "India", "Pakistan", "Guatemala")
  names(full_names) <- c("moz", "ind", "pak", "gt")
  country = full_names[cty][1]
  
  contacts_daily <- read.csv(paste0(here(),"/data/",cty,"/", cty, hhmbr, "_outlier_resp_air_ent.csv")) %>%
    mutate(avg_daily_resp_contacts = as.numeric(avg_daily_resp_contacts),
           avg_daily_ent_contacts = as.numeric(avg_daily_ent_contacts))
  
  # # Determine if negative binomial distribution is the best for resp -----------
  # lm_model <- lm(data = contacts_daily, avg_daily_resp_contacts ~ 1) # Linear model (mean only)
  # pois_model <- glm(data = contacts_daily, avg_daily_resp_contacts ~ 1, family = poisson())  # Poisson model
  # nb_model <- glm.nb(data = contacts_daily, avg_daily_resp_contacts ~ 1) # Negative binomial model
  # mu_lm <- coef(lm_model)[1]
  # mu_pois <- exp(coef(pois_model)[1])
  # mu_nb <- exp(coef(nb_model)[1])
  # theta_nb <- nb_model$theta
  # 
  # y_vals <- 0:max(contacts_daily$avg_daily_resp_contacts, na.rm = T)  # range of y values to plot
  # df <- data.frame(
  #   y = y_vals,
  #   Normal = dnorm(y_vals, mean = mu_lm, sd = sd(residuals(lm_model))),
  #   Poisson = dpois(y_vals, lambda = mu_pois),
  #   NegBinom = dnbinom(y_vals, size = theta_nb, mu = mu_nb)
  # )
  # df_long <- pivot_longer(df, cols = -y, names_to = "Dist", values_to = "Density") 
  # 
  # png(filename = paste0(here(), "/figs/", cty, hhmbr, "_resp_hist.png" ), height = 800, width = 1200)
  # print(ggplot() +
  #   geom_histogram(data = contacts_daily, aes(x = avg_daily_resp_contacts, y = ..density..), 
  #                  bins = length(unique((contacts_daily$avg_daily_resp_contacts))), 
  #                  fill = "gray80", color = "black") +
  #   geom_line(data = df_long, aes(x = y, y = Density, color = Dist, lty=Dist), size = 1.3) +
  #   theme_bw() +
  #   ggtitle("")+
  #   xlab("")+
  #   theme(text = element_text(size = 20)))
  # dev.off() 
  # 
  # # Determine if negative binomial distribution is the best for ent -----------
  # lm_model <- lm(data = contacts_daily, avg_daily_ent_contacts ~ 1) # Linear model (mean only)
  # pois_model <- glm(data = contacts_daily, avg_daily_ent_contacts ~ 1, family = poisson())  # Poisson model
  # nb_model <- glm.nb(data = contacts_daily, avg_daily_ent_contacts ~ 1) # Negative binomial model
  # mu_lm <- coef(lm_model)[1]
  # mu_pois <- exp(coef(pois_model)[1])
  # mu_nb <- exp(coef(nb_model)[1])
  # theta_nb <- nb_model$theta
  # 
  # y_vals <- 0:max(contacts_daily$avg_daily_ent_contacts, na.rm = T)  # range of y values to plot
  # df <- data.frame(
  #   y = y_vals,
  #   Normal = dnorm(y_vals, mean = mu_lm, sd = sd(residuals(lm_model))),
  #   Poisson = dpois(y_vals, lambda = mu_pois),
  #   NegBinom = dnbinom(y_vals, size = theta_nb, mu = mu_nb)
  # )
  # df_long <- pivot_longer(df, cols = -y, names_to = "Dist", values_to = "Density") 
  # 
  # png(filename = paste0(here(), "/figs/", cty, hhmbr, "_ent_hist.png" ), height = 800, width = 1200)
  # print(ggplot() +
  #   geom_histogram(data = contacts_daily, aes(x = avg_daily_ent_contacts, y = ..density..), 
  #                  bins = length(unique((contacts_daily$avg_daily_ent_contacts))), 
  #                  fill = "gray80", color = "black") +
  #   geom_line(data = df_long, aes(x = y, y = Density, color = Dist, lty=Dist), size = 1.3) +
  #   theme_bw() +
  #   ggtitle("")+
  #   xlab("")+
  #   theme(text = element_text(size = 20)))
  # dev.off() 
  
  # Specify comparison groups --------------------------------------------------
  
  contacts_daily <- contacts_daily %>%
    rename(sex = participant_sex) %>%
    mutate(age = factor(participant_age, levels = c("30-39y",
                                                    "<6mo",
                                                    "6-11mo",
                                                    "1-4y",
                                                    "5-9y", 
                                                    "10-19y",
                                                    "20-29y",
                                                    "40-59y",
                                                    "60+y"))) %>%
    mutate(occupation = factor(occupation, levels = c("Unemployed outside home",
                                                      "Student",
                                                      "Semiskilled / skilled labor",
                                                      "Semiprofessional / professional"))) %>%
    mutate(hh_size_cat = factor(hh_size_cat, levels = c("[0,2]",
                                                        "(2,5]",
                                                        "(5,50]")))
  
  # Regressions ------------------------------------------------------------
  negbin_daily_resp_mod <- glm.nb(as.formula(paste0("avg_daily_resp_contacts ~ ", equation)),
                                  data = contacts_daily)
  
  negbin_daily_ent_mod <- glm.nb(as.formula(paste0("avg_daily_ent_contacts ~ ", equation)),
                                   data = contacts_daily)

  negbin_daily_air_mod <- glm.nb(as.formula(paste0("avg_daily_air_contacts ~ ", equation)),
                                 data = contacts_daily)
  
  coef_exp <- coef(negbin_daily_resp_mod)[!is.na(coef(negbin_daily_resp_mod))]
  negbin_daily_resp <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = exp(coef_exp - 1.96 * sqrt(diag(vcov(negbin_daily_resp_mod)))),
    Upper_95_CI = exp(coef_exp + 1.96 * sqrt(diag(vcov(negbin_daily_resp_mod))))
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%
    mutate(Term = term) %>%
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  coef_exp <- coef(negbin_daily_air_mod)[!is.na(coef(negbin_daily_air_mod))]
  negbin_daily_air <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = exp(coef_exp - 1.96 * sqrt(diag(vcov(negbin_daily_air_mod)))),
    Upper_95_CI = exp(coef_exp + 1.96 * sqrt(diag(vcov(negbin_daily_air_mod))))
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%
    mutate(Term = term) %>%
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  coef_exp <- coef(negbin_daily_ent_mod)[!is.na(coef(negbin_daily_ent_mod))]
  negbin_daily_ent <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = exp(coef_exp - 1.96 * sqrt(diag(vcov(negbin_daily_ent_mod)))),
    Upper_95_CI = exp(coef_exp + 1.96 * sqrt(diag(vcov(negbin_daily_ent_mod))))
  ) %>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%#, 
    mutate(Term = term) %>% #,
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
 
  
  
  # Save data --------------------------------------------------------------------
  
  return(list(negbin_daily_resp, negbin_daily_air, negbin_daily_ent))
}
