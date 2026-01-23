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
  total_nonhh <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "A) Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "B) India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "C) Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "D) Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  total_indtime <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "A) Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "B) India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "C) Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "D) Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  total_touch <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "A) Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "B) India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "C) Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "D) Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  
  outlier_nonhh <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "A) Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "B) India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "C) Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "D) Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  outlier_indtime <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "A) Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "B) India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "C) Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "D) Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  outlier_touch <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "A) Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "B) India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "C) Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "D) Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor)))
  
  # Total --------
  title = "Age + Sex + Household Size + Occupation + Higher Education"
  if(name == "daily_5"){
    title = "Age + Sex + Household Size + Occupation"
  }
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_nonhh.png"), width=7000, height=4000, res=300)
  print(ggplot(total_nonhh, aes(x = predictor,y = RR)) +#, color = Significance)) + 
          geom_hline(yintercept = 1, lwd = 3, color = "gray")+
          geom_point(size = 6) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.2, lwd = 1)+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Associations between participant factors and number of non-household contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_indtime.png"), width=7000, height=4000, res=300)
  print(ggplot(total_indtime, aes(x = predictor,y = RR))+ #, color = Significance)) + 
          geom_hline(yintercept = 1, lwd = 3, color = "gray")+
          geom_point(size = 6) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.2, lwd = 1)+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Associations between participant factors and indoor exposure-hours by country",
               subtitle = "GlobalMix study, 2021-2023")+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0),
                legend.position = "none"))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_touch.png"), width=7000, height=4000, res=300)
  print(ggplot(total_touch, aes(x = predictor,y = RR))+#, color = Significance)) + 
          geom_hline(yintercept = 1, lwd = 3, color = "gray")+
          geom_point(size = 6) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.2, lwd = 1)+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Associations between participant factors and non-household contacts involving touch by country",
               subtitle = "GlobalMix study, 2021-2023")+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0),
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
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_nonhh.png"), width=7000, height=5000, res=300)
  print(ggplot(outlier_nonhh, aes(x = predictor,y = RR))+#, color = Significance)) +
          geom_hline(yintercept = 1, lwd = 3, color = "gray")+
          geom_point(size = 6) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.2, lwd = 1)+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Figure 1. Associations between participant factors and direct deposition contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0),
                legend.position = "none")+
          coord_trans(y = specTrans))
  dev.off()
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_indtime.png"), width=7000, height=5000, res=300)
  print(ggplot(outlier_indtime, aes(x = predictor,y = RR))+#, color = Significance)) +
          geom_hline(yintercept = 1, lwd = 3, color = "gray")+
          geom_point(size = 6) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.2, lwd = 1)+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Figure 2. Associations between participant factors and inhalation contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0),
                legend.position = "none")+
          coord_trans(y = specTrans))
  dev.off()
  
  png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_touch.png"), width=7000, height=5000, res=300)
  print(ggplot(outlier_touch, aes(x = predictor,y = RR))+#, color = Significance)) +
          geom_hline(yintercept = 1, lwd = 3, color = "gray")+
          geom_point(size = 6) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.2, lwd = 1)+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Risk Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Figure 3. Associations between participant factors and fecal-oral contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0),
                legend.position = "none")+
          coord_trans(y = specTrans))
  dev.off()
  
}
