pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry)

plot_simple_regression_results <- function(hhmbr = "", prctl = 80, name = "daily_5"){
  
  # Read in Data ---------
  total_nonhh <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  total_indtime <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  total_touch <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  
  outlier_nonhh <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  outlier_indtime <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  outlier_touch <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  
  # Total --------
  title = "Age + Sex + Household Size + Occupation + Higher Education"
  if(name == "daily_5"){
    title = "Age + Sex + Household Size + Occupation"
  }
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_nonhh.png"), width=7000, height=4000, res=300)
  print(ggplot(total_nonhh, aes(x = predictor,y = RR, color = Significance)) + 
          geom_point(size = 3) +
          geom_hline(yintercept = 1, lty = 2, color = "gray")+
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.4, lwd = 1.5)+
          scale_colour_gradient2(low = "firebrick",
                                 mid = "goldenrod1",
                                 high = "forestgreen", 
                                 midpoint=0,
                                 guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          ggtitle(paste0(hhmbr,"\nDaily Non-Hospital Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_indtime.png"), width=7000, height=4000, res=300)
  print(ggplot(total_indtime, aes(x = predictor,y = RR, color = Significance)) + 
          geom_point(size = 3) +
          geom_hline(yintercept = 1, lty = 2, color = "gray")+
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.4, lwd = 1.5)+
          scale_colour_gradient2(low = "firebrick",
                                 mid = "goldenrod1",
                                 high = "forestgreen", 
                                 midpoint=0,
                                 guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          ggtitle(paste0(hhmbr,"\nDaily Non-Household Indoor-Time Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_touch.png"), width=7000, height=4000, res=300)
  print(ggplot(total_touch, aes(x = predictor,y = RR, color = Significance)) + 
          geom_point(size = 3) +
          geom_hline(yintercept = 1, lty = 2, color = "gray")+
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.4, lwd = 1.5)+
          scale_colour_gradient2(low = "firebrick",
                                 mid = "goldenrod1",
                                 high = "forestgreen", 
                                 midpoint=0,
                                 guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          ggtitle(paste0(hhmbr,"\nDaily Non-Household Touch Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  # Scaling outlier bounds -------
  fspec = function(x) ifelse(x<5, x, 5+(x-5)/10)
  fspec_1 = function(x) ifelse(x<5, x, 5+(x-5)*10)
  
  specTrans = trans_new(name = "specialTras",
                        transform = fspec,
                        inverse = fspec_1,
                        breaks = c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100, 150))
  
  # Outlier ---------
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_nonhh.png"), width=7000, height=4000, res=300)
  print(ggplot(outlier_nonhh, aes(x = predictor,y = RR, color = Significance)) +
          geom_point(size = 3) +
          geom_hline(yintercept = 1, lty = 2, color = "gray")+
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.4, lwd = 1.5)+
          scale_colour_gradient2(low = "firebrick",
                                 mid = "goldenrod1",
                                 high = "forestgreen",
                                 midpoint=0,
                                 guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          ggtitle(paste0(hhmbr,"\nOutlier Non-Household Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none")+
          coord_trans(y = specTrans))
  dev.off()
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_indtime.png"), width=7000, height=4000, res=300)
  print(ggplot(outlier_indtime, aes(x = predictor,y = RR, color = Significance)) +
          geom_point(size = 3) +
          geom_hline(yintercept = 1, lty = 2, color = "gray")+
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.4, lwd = 1.5)+
          scale_colour_gradient2(low = "firebrick",
                                 mid = "goldenrod1",
                                 high = "forestgreen",
                                 midpoint=0,
                                 guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          ggtitle(paste0(hhmbr,"\nOutlier Non-Household Indoor-Time Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none")+
          coord_trans(y = specTrans))
  dev.off()
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_touch.png"), width=7000, height=4000, res=300)
  print(ggplot(outlier_touch, aes(x = predictor,y = RR, color = Significance)) +
          geom_point(size = 3) +
          geom_hline(yintercept = 1, lty = 2, color = "gray")+
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.4, lwd = 1.5)+
          scale_colour_gradient2(low = "firebrick",
                                 mid = "goldenrod1",
                                 high = "forestgreen",
                                 midpoint=0,
                                 guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          ggtitle(paste0(hhmbr,"\nOutlier Non-Household Touch Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none")+
          coord_trans(y = specTrans))
  dev.off()
  
}
