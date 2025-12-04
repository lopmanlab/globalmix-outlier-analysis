rm(list = ls())
pacman::p_load(here,
               tidyverse,
               plotly)

# Create data for outlier indoor, non-household member, and touch contacts ----

# The following line loads this function: prepare_simple_outlier_datasets()
source("04a_simpleoutcome_data_prep.R")

countries = c("moz", "ind", "gt", "pak")
hhmembership = ""
h = hhmembership

# Already created the datasets
for(c in countries){
  # for(h in hhmembership){
    dat <- prepare_simple_outlier_datasets(cty = c, hhmbr = h)
    write.csv(dat, paste0(here(),"/data/",c,"/", c, h, "_outlier_simple.csv"))
  # }
}

# Run continuous outcome regressions for non-hh and indoor-time contacts --------------

# The following line loads this function: total_resp_ent_contacts_regression()
source("04b_total_simple_analysis.R")

ref =  c(rep("Age\nRef: 30-39y", 8),
         "Sex\nRef: Female",
         "Site\nRef: Rural",
         rep("Household Size\nRef: 0-2 members", 2),
         rep("Occupation\nRef: Unemployed, Child, Other", 3))

term <- c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "40-59y", "60+y",
          "Male",
          "Urban",
          "3-5", "6+", #"8+",
          "Student", "Laborer", "Professional")

equation = "age + sex + study_site + hh_size_cat + occupation"

for(c in countries){
  # for(h in hhmembership){
    dat <- total_simple_contacts_regression(cty = c, hhmbr = h, ref = ref, term = term)
    write.csv(dat[[1]], paste0("results/", c, h,"_negbin_daily_5_nonhh.csv"))
    write.csv(dat[[2]], paste0("results/", c, h,"_negbin_daily_5_indtime.csv"))
    write.csv(dat[[3]], paste0("results/", c, h,"_negbin_daily_5_touch.csv"))
    # }
}

# Run logistic outcome regressions for non-hh and indoor-time contacts --------------

# The following line loads this function: outlier_simple_contacts_regression()
source("04c_simple_outlier_analysis.R")

prctl = 80

for(c in countries){
  # for(h in hhmembership){
    dat <- outlier_simple_contacts_regression(cty = c, hhmbr = h, prctl = prctl, ref = ref, term = term)
    write.csv(dat[[1]], paste0("results/", c, h,"_", prctl, "_outlier_daily_5_nonhh.csv"))
    write.csv(dat[[2]], paste0("results/", c, h,"_", prctl,"_outlier_daily_5_indtime.csv"))
    write.csv(dat[[3]], paste0("results/", c, h,"_", prctl,"_outlier_daily_5_touch.csv"))
  # }
}

# Create plots for non-hh and indoor-time, continuous & dichotomous, outcomes --------------

# The following line loads this function: plot_simple_regression_results()
source("04d_plot_simple_analysis_results.R")

# for(h in hhmembership){
  plot_simple_regression_results(hhmbr = h, prctl = prctl, name = "daily_5") #could also be daily_5
# }

# PCA --------------

# The following line loads this function: pca_analysis()
source("04e_pca_simple_analysis_results.R")

for(c in countries){
  # for(h in hhmembership){
    pca_simple_analysis(cty = c, hhmbr = h, prctl = prctl, name = "daily_5") #could also be daily_5
  # }
}




