pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry,
               quantreg,
               pls,
               performance,
               regclass)

outlier_resp_ent_contacts_regression <- function(cty = "moz", hhmbr = "", prctl = 90, equation = "age + sex + study_site + hh_size_cat + occupation", ref, term){
  
  full_names <- c("Mozambique", "India", "Pakistan", "Guatemala")
  names(full_names) <- c("moz", "ind", "pak", "gt")
  country = full_names[cty][1]
  contacts_daily <- read.csv(paste0(here(),"/data/",cty,"/", cty, hhmbr, "_outlier_resp_air_ent.csv")) 

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
                                                        "(5,50]"))) %>%
    mutate(resp_outlier = case_when(prctl == 90 ~ daily_resp_q90_outlier,
                               prctl == 75 ~ daily_resp_q75_outlier,
                               .default = daily_resp_q50_outlier)) %>%
    mutate(air_outlier = case_when(prctl == 90 ~ daily_air_q90_outlier,
                                    prctl == 75 ~ daily_air_q75_outlier,
                                    .default = daily_air_q50_outlier)) %>%
    mutate(ent_outlier = case_when(prctl == 90 ~ daily_ent_q90_outlier,
                                    prctl == 75 ~ daily_ent_q75_outlier,
                                    .default = daily_ent_q50_outlier))
  
  # Daily Q"prctl" threshold outlier model -------------------------------------------------
  
  daily_resp_model <- glm(paste0("resp_outlier ~ ", equation), 
                          data = contacts_daily, 
                          family = binomial(link = "logit"))
  coef_exp  <- coef(daily_resp_model)[!is.na(coef(daily_resp_model))]
  conf_int <- confint(daily_resp_model)
  daily_resp <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = conf_int[,1],
    Upper_95_CI = conf_int[,2]
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%
    mutate(Term = term) %>%
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  daily_air_model <- glm(paste0("air_outlier ~ ", equation), 
                          data = contacts_daily, 
                          family = binomial(link = "logit"))
  coef_exp  <- coef(daily_air_model)[!is.na(coef(daily_air_model))]
  conf_int <- confint(daily_air_model)
  daily_air <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = conf_int[,1],
    Upper_95_CI = conf_int[,2]
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%#, 
    mutate(Term = term) %>%#,
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  daily_ent_model <- glm(paste0("ent_outlier ~ ", equation),
                         data = contacts_daily, 
                         family = binomial(link = "logit"))
  coef_exp <- coef(daily_ent_model)[!is.na(coef(daily_ent_model))]
  conf_int <- confint(daily_ent_model)
  daily_ent <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = conf_int[,1],
    Upper_95_CI = conf_int[,2]
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%  
    mutate(Term = term) %>%
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  # Quintile Regression -------------------------------------------------------
  
  
  # multi_rqfit <- rq(avg_daily_resp_contacts ~ age + sex + study_site + hh_size_cat + occupation,
  #                   data = contacts_daily, 
  #                   tau = seq(0.2, 0.8, by = 0.2))
  # multi_rqfit$coefficients
  # plot(multi_rqfit)
  # QR = summary.rqs(multi_rqfit, se="iid", covariance = TRUE)
  # exp(coef(QR[[1]]))
  # exp(coef_exp + 1.96 * na.omit(sqrt(diag(vcov(daily_resp_model)))))
  # confint(QR[[1]], parm=NULL, level = 0.95, method = "iid")
  # # sapply(multi_rqfit, function(x) c(tau=x$tau, x$coefficients[-1, ]))
  # individual_rq_fit <- multi_rqfit[[1]]
  # vcov(individual_rq_fit)
  # 
  # cor(contacts_daily %>% 
  #       mutate(age = as.numeric(age),
  #              sex = ifelse(sex == "Male", 1, 0), 
  #              study_site = ifelse(study_site == "Rural", 1, 0), 
  #              hh_size_cat = as.numeric(hh_size_cat), 
  #              occupation = as.numeric(occupation)) %>%
  #       dplyr::select(age, sex, study_site, hh_size_cat, occupation))
  
  return(list(daily_resp, daily_air, daily_ent))
}
