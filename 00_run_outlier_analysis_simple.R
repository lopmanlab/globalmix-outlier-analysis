rm(list = ls())
pacman::p_load(here,
               tidyverse,
               plotly, 
               png,
               grid,
               gridExtra,
               tiff)

# Create data for outlier indoor, non-household member, and touch contacts ----

# The following line loads this function: prepare_simple_outlier_datasets()
source("01_simpleoutcome_data_prep.R")

countries = c("moz", "ind", "gt", "pak")

# Already created the datasets
for(c in countries){
    dat <- prepare_simple_outlier_datasets(cty = c)
    write.csv(dat, paste0(here(),"/data/",c,"/", c, "_outlier_simple.csv"))
}

# Run continuous outcome regressions for non-hh and indoor-time contacts --------------

# The following line loads this function: simple_outcome_hist()
source("02_simpleoutcome_hist.R")

ref =  c(rep("Age\nRef: 30-39y", 7),
         "Sex\nRef: Female",
         "Site\nRef: Rural",
         rep("Household Size\nRef: 0-2 members", 2),
         rep("Occupation\nRef: Unemployed, Child, Other", 3))

term <- c("<1y", "1-4y", "5-9y", "10-19y", "20-29y", "40-59y", "60+y",
          "Male",
          "Urban",
          "3-5", "6+", #"8+",
          "Student", "Laborer", "Professional")

equation = "age + sex + study_site + hh_size_cat + occupation"

for(c in countries){
    dat <- simple_outcome_hist(cty = c, ref = ref, term = term)
    write.csv(dat[[1]], paste0("results/", c, "_negbin_daily_5_nonhh.csv"))
    write.csv(dat[[2]], paste0("results/", c, "_negbin_daily_5_indtime.csv"))
    write.csv(dat[[3]], paste0("results/", c, "_negbin_daily_5_touch.csv"))
}

# Read in each panel (now TIFF inputs)
moz = rasterGrob(readTIFF("figs/histograms/moz_nonhh_hist.tiff"), interpolate = TRUE)
ind = rasterGrob(readTIFF("figs/histograms/ind_nonhh_hist.tiff"), interpolate = TRUE)
pak = rasterGrob(readTIFF("figs/histograms/pak_nonhh_hist.tiff"), interpolate = TRUE)
gt  = rasterGrob(readTIFF("figs/histograms/gt_nonhh_hist.tiff"),  interpolate = TRUE)

# Panel labels (A/B/C/D) — journals expect these for multi-panel figures
panel_label <- function(grob, label) {
  arrangeGrob(grob, top = textGrob(label, x = unit(0.02, "npc"), just = "left",
                                   gp = gpar(fontsize = 14, fontface = "bold")))
}

moz_labeled = panel_label(moz, "A. Mozambique")
ind_labeled = panel_label(ind, "B. India")
pak_labeled = panel_label(pak, "C. Pakistan")
gt_labeled  = panel_label(gt,  "D. Guatemala")

# Export as TIFF at journal-required resolution
tiff("figs/histograms/all_countries_nonhh.tiff",
     width = 2244, height = 1683,   # full-page width per Elsevier px spec at 300 dpi
     res = 300, compression = "lzw")

grid.arrange(moz_labeled, ind_labeled, pak_labeled, gt_labeled,
             ncol = 2, nrow = 2)

dev.off()

# Read in each panel (now TIFF inputs)
moz = rasterGrob(readTIFF("figs/histograms/moz_indtime_hist.tiff"), interpolate = TRUE)
ind = rasterGrob(readTIFF("figs/histograms/ind_indtime_hist.tiff"), interpolate = TRUE)
pak = rasterGrob(readTIFF("figs/histograms/pak_indtime_hist.tiff"), interpolate = TRUE)
gt  = rasterGrob(readTIFF("figs/histograms/gt_indtime_hist.tiff"),  interpolate = TRUE)

# Panel labels (A/B/C/D) — journals expect these for multi-panel figures
panel_label <- function(grob, label) {
  arrangeGrob(grob, top = textGrob(label, x = unit(0.02, "npc"), just = "left",
                                   gp = gpar(fontsize = 14, fontface = "bold")))
}

moz_labeled = panel_label(moz, "A. Mozambique")
ind_labeled = panel_label(ind, "B. India")
pak_labeled = panel_label(pak, "C. Pakistan")
gt_labeled  = panel_label(gt,  "D. Guatemala")

# Export as TIFF at journal-required resolution
tiff("figs/histograms/all_countries_indtime.tiff",
     width = 2244, height = 1683,   # full-page width per Elsevier px spec at 300 dpi
     res = 300, compression = "lzw")

grid.arrange(moz_labeled, ind_labeled, pak_labeled, gt_labeled,
             ncol = 2, nrow = 2)

dev.off()

# Read in each panel (now TIFF inputs)
moz = rasterGrob(readTIFF("figs/histograms/moz_touch_hist.tiff"), interpolate = TRUE)
ind = rasterGrob(readTIFF("figs/histograms/ind_touch_hist.tiff"), interpolate = TRUE)
pak = rasterGrob(readTIFF("figs/histograms/pak_touch_hist.tiff"), interpolate = TRUE)
gt  = rasterGrob(readTIFF("figs/histograms/gt_touch_hist.tiff"),  interpolate = TRUE)

# Panel labels (A/B/C/D) — journals expect these for multi-panel figures
panel_label <- function(grob, label) {
  arrangeGrob(grob, top = textGrob(label, x = unit(0.02, "npc"), just = "left",
                                   gp = gpar(fontsize = 14, fontface = "bold")))
}

moz_labeled = panel_label(moz, "A. Mozambique")
ind_labeled = panel_label(ind, "B. India")
pak_labeled = panel_label(pak, "C. Pakistan")
gt_labeled  = panel_label(gt,  "D. Guatemala")

# Export as TIFF at journal-required resolution
tiff("figs/histograms/all_countries_touch.tiff",
     width = 2244, height = 1683,   # full-page width per Elsevier px spec at 300 dpi
     res = 300, compression = "lzw")

grid.arrange(moz_labeled, ind_labeled, pak_labeled, gt_labeled,
             ncol = 2, nrow = 2)

dev.off()


# Run logistic outcome regressions for non-hh and indoor-time contacts --------------

# The following line loads this function: outlier_simple_contacts_regression()
source("03_simple_outlier_analysis.R")

prctl = 75

for(c in countries){
    dat <- outlier_simple_contacts_regression(cty = c, prctl = prctl, ref = ref, term = term)
    write.csv(dat[[1]], paste0("results/", c, "_", prctl, "_outlier_daily_5_nonhh.csv"))
    write.csv(dat[[2]], paste0("results/", c, "_", prctl,"_outlier_daily_5_indtime.csv"))
    write.csv(dat[[3]], paste0("results/", c, "_", prctl,"_outlier_daily_5_touch.csv"))
}

# Create plots for non-hh and indoor-time, continuous & dichotomous, outcomes --------------

# The following line loads this function: plot_simple_regression_results()
source("04_plot_simple_analysis_results.R")

plot_simple_regression_results(prctl = prctl, name = "daily_5")


# PCA --------------

# The following line loads this function: pca_simple_analysis()
source("05_pca_simple_analysis_results.R")

for(c in countries){
    pca_simple_analysis(cty = c, prctl = prctl, name = "daily_5")
}

