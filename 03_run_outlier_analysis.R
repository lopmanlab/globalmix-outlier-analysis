rm(list = ls())
pacman::p_load(here,
               tidyverse,
               plotly)

# Create outlier datasets, with respiratory and enteric contact definitions ----

# The following line loads this function: prepare_resp_ent_outlier_datasets()
source("03a_respiratory_enteric_data_prep.R")

countries = c("moz", "ind", "gt", "pak")
hhmembership = c("", "Non-member")

# Already created the datasets
for(c in countries){
  for(h in hhmembership){
    dat <- prepare_resp_ent_outlier_datasets(cty = c, hhmbr = h)
    write.csv(dat, paste0(here(),"/data/",c,"/", c, h, "_outlier_resp_air_ent.csv"))
  }
}

# Run continuous outcome regressions for resp, air, and ent definitions --------------

# The following line loads this function: total_resp_ent_contacts_regression()
source("03b_total_respiratory_enteric_analysis.R")

reference =  c(rep("Age\nRef: 30-39y", 8),
         "Sex\nRef: Female",
         "Site\nRef: Rural",
         rep("Household Size\nRef: 0-2 members", 2),
         rep("Occupation\nRef: Unemployed, Child, Other", 3))

term <- c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "40-59y", "60+y",
          "Male",
          "Urban",
          "3-5", "6+", #"8+",
          "Student", "Laborer", "Professional")
          #"<6mo, Male", "6-11mo, Male", "1-4y, Male", "5-9y, Male", "10-19y, Male", "20-29y, Male", "40-59y, Male", "60+y, Male",
          #"<6mo, Urban", "6-11mo, Urban", "1-4y, Urban", "5-9y, Urban", "10-19y, Urban", "20-29y, Urban", "40-59y, Urban", "60+y, Urban",
          # Male, Urban

equation = "age + sex + study_site + hh_size_cat + occupation"

for(c in countries){
  for(h in hhmembership){
    dat <- total_resp_ent_contacts_regression(cty = c, hhmbr = h, ref = reference, term = term)
    write.csv(dat[[1]], paste0("results/", c, h,"_negbin_daily_5_resp.csv"))
    write.csv(dat[[2]], paste0("results/", c, h,"_negbin_daily_5_air.csv"))
    write.csv(dat[[3]], paste0("results/", c, h,"_negbin_daily_5_ent.csv"))
  }
}

# Run logistic outcome regressions for resp and ent definitions --------------

# The following line loads this function: outlier_resp_ent_contacts_regression()
source("03c_respiratory_enteric_outlier_analysis.R")

percentile = 90

for(c in countries){
  for(h in hhmembership){
    dat <- outlier_resp_ent_contacts_regression(cty = c, hhmbr = h, prctl = percentile, ref = reference, term = term)
    write.csv(dat[[1]], paste0("results/", c, h,"_", percentile, "_outlier_daily_5_resp.csv"))
    write.csv(dat[[2]], paste0("results/", c, h,"_", percentile,"_outlier_daily_5_air.csv"))
    write.csv(dat[[3]], paste0("results/", c, h,"_", percentile,"_outlier_daily_5_ent.csv"))
  }
}

# Create plots for resp & ent definitions and continuous & dichotomous outcomes --------------

# The following line loads this function: plot_regression_results()
source("03d_plot_analysis_results.R")

for(h in hhmembership){
  plot_regression_results(hhmbr = h, prctl = percentile, name = "daily_5") #could also be daily_5
}


