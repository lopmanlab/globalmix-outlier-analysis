pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry)

plot_regression_results <- function(hhmbr = "", prctl = 90, name = "daily_5"){
  
  # Read in Data ---------
  total_resp <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_resp.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_resp.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_resp.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_resp.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  total_air <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_air.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_air.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_air.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_air.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  total_ent <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_ent.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_ent.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_ent.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_ent.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  
  outlier_resp <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_resp.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_resp.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_resp.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_resp.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  outlier_air <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_air.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_air.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_air.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_air.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  outlier_ent <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_ent.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_ent.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_ent.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_ent.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  
  # Total --------
  title = "Age + Sex + Household Size + Occupation + Higher Education"
  if(name == "daily_5"){
    title = "Age + Sex + Household Size + Occupation"
  }
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_resp.png"), width=7000, height=3500, res=300)
  print(ggplot(total_resp, aes(x = predictor,y = RR, color = Significance)) + 
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
          ggtitle(paste0(hhmbr,"\nDaily Respiratory Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_air.png"), width=7000, height=3500, res=300)
  print(ggplot(total_air, aes(x = predictor,y = RR, color = Significance)) + 
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
          ggtitle(paste0(hhmbr,"\nDaily Airborne Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_ent.png"), width=7000, height=3500, res=300)
  print(ggplot(total_ent, aes(x = predictor,y = RR, color = Significance)) + 
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
          ggtitle(paste0(hhmbr,"\nDaily Enteric Contacts ~  ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  # Outlier ---------
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_resp.png"), width=7000, height=3500, res=300)
  print(ggplot(outlier_resp, aes(x = predictor,y = RR, color = Significance)) +
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
          ggtitle(paste0(hhmbr,"\nOutlier Respiratory Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_air.png"), width=7000, height=3500, res=300)
  print(ggplot(outlier_air, aes(x = predictor,y = RR, color = Significance)) +
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
          ggtitle(paste0(hhmbr,"\nOutlier Airborne Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()

  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_ent.png"), width=7000, height=3500, res=300)
  print(ggplot(outlier_ent, aes(x = predictor,y = RR, color = Significance)) +
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
          ggtitle(paste0(hhmbr,"\nOutlier Enteric Contacts ~ ", title))+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0, 3)+
          theme_classic()+
          theme(text=element_text(size=18),
                legend.position = "none"))
  dev.off()
}
