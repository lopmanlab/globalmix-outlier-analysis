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

outlier_simple_contacts_regression <- function(cty = "moz", hhmbr = "", prctl = 80, equation = "age + sex + study_site + hh_size_cat + occupation", ref, term){
  
  full_names <- c("Mozambique", "India", "Pakistan", "Guatemala")
  names(full_names) <- c("moz", "ind", "pak", "gt")
  country = full_names[cty][1]
  contacts_daily <- read.csv(paste0(here(),"/data/",cty,"/", cty, hhmbr, "_outlier_simple.csv")) 
  
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
    mutate(nonhh_outlier = case_when(prctl == 90 ~ daily_nonhh_q90_outlier,
                                    prctl == 80 ~ daily_nonhh_q80_outlier,
                                    prctl == 75 ~ daily_nonhh_q75_outlier,
                                    .default = daily_nonhh_q50_outlier)) %>%
    mutate(indtime_outlier = case_when(prctl == 90 ~ daily_indtime_q90_outlier,
                                   prctl == 80 ~ daily_indtime_q80_outlier,
                                   prctl == 75 ~ daily_indtime_q75_outlier,
                                   .default = daily_indtime_q50_outlier)) %>%
    mutate(touch_outlier = case_when(prctl == 90 ~ daily_touch_q90_outlier,
                                       prctl == 80 ~ daily_touch_q80_outlier,
                                       prctl == 75 ~ daily_touch_q75_outlier,
                                       .default = daily_touch_q50_outlier)) 
  
  # Daily Q"prctl" threshold outlier model -------------------------------------------------
  
  daily_nonhh_model <- glm(paste0("nonhh_outlier ~ ", equation), 
                          data = contacts_daily, 
                          family = binomial(link = "logit"))
  coef_exp  <- coef(daily_nonhh_model)[!is.na(coef(daily_nonhh_model))]
  daily_nonhh <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = exp(coef_exp - 1.96 * na.omit(sqrt(diag(vcov(daily_nonhh_model))))),
    Upper_95_CI = exp(coef_exp + 1.96 * na.omit(sqrt(diag(vcov(daily_nonhh_model)))))
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%
    mutate(Term = term) %>%
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  daily_indtime_model <- glm(paste0("indtime_outlier ~ ", equation), 
                         data = contacts_daily, 
                         family = binomial(link = "logit"))
  coef_exp  <- coef(daily_indtime_model)[!is.na(coef(daily_indtime_model))]
  daily_indtime <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = exp(coef_exp - 1.96 * na.omit(sqrt(diag(vcov(daily_indtime_model))))),
    Upper_95_CI = exp(coef_exp + 1.96 * na.omit(sqrt(diag(vcov(daily_indtime_model)))))
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%#, 
    mutate(Term = term) %>%#,
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  daily_touch_model <- glm(paste0("touch_outlier ~ ", equation), 
                             data = contacts_daily, 
                             family = binomial(link = "logit"))
  coef_exp  <- coef(daily_touch_model)[!is.na(coef(daily_touch_model))]
  daily_touch <- data.frame(
    Term = names(coef_exp),
    RR =  exp(coef_exp),
    Lower_95_CI = exp(coef_exp - 1.96 * na.omit(sqrt(diag(vcov(daily_touch_model))))),
    Upper_95_CI = exp(coef_exp + 1.96 * na.omit(sqrt(diag(vcov(daily_touch_model)))))
  )%>%
    filter(Term != "(Intercept)") %>%
    mutate(Characteristic = ref) %>%#, 
    mutate(Term = term) %>%#,
    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                 ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
    mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                              levels = paste(Term, Characteristic, sep = "&")))
  
  return(list(daily_nonhh, daily_indtime, daily_touch))
}
