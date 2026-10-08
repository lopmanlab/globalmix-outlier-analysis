pacman::p_load(here,
               tidyverse,
               plotly, 
               MASS,
               GGally,
               scales,
               ggpubr,
               legendry,
               ggsci, 
               ggbreak,
               grid,
               gtable)

plot_simple_quantile_results <- function(name = "daily_5"){
  
  # Read in Data ---------
  quantile_nonhh <- read.csv(paste0("results/", "moz", "_quantile",  "_nonhh.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", "_quantile",  "_nonhh.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", "_quantile",  "_nonhh.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", "_quantile",  "_nonhh.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique"))) %>%
    rename(Country = country)
  quantile_indtime <- read.csv(paste0("results/", "moz", "_quantile",  "_indtime.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", "_quantile",  "_indtime.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", "_quantile",  "_indtime.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", "_quantile",  "_indtime.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique"))) %>%
    rename(Country = country)
  quantile_touch <- read.csv(paste0("results/", "moz", "_quantile",  "_touch.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", "_quantile",  "_touch.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", "_quantile",  "_touch.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", "_quantile",  "_touch.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique")))%>%
    rename(Country = country)
  
  write.csv(quantile_nonhh, "results/quantile_nonhh.csv")
  write.csv(quantile_indtime, "results/quantile_indtime.csv")
  write.csv(quantile_touch, "results/quantile_touch.csv")
  
  # Scaling outlier bounds -------
  fspec = function(x) ifelse(x<5, x, 5+(x-5)/10)
  fspec_1 = function(x) ifelse(x<5, x, 5+(x-5)*10)
  
  specTrans = trans_new(name = "specialTras",
                        transform = fspec,
                        inverse = fspec_1,
                        breaks = c(0, 1, 2, 3, 4, 5, 10, 20, 50, 100, 150))
  
  # Outlier ---------
  
  png(paste0("figs/","_quantile_",  "_nonhh.png"), width=7000, height=3000, res=300)
  print(ggplot(quantile_nonhh, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
          geom_hline(yintercept = 1, lwd = 2, color = "gray")+
          geom_point(size = 6, position=position_dodge(width=0.7)) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.5, lwd = 2, position=position_dodge(width=0.7))+
          scale_color_d3()+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Odds Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Figure 1. Associations between participant factors and direct deposition contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          # facet_wrap(~ Country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0))+
          coord_trans(y = specTrans))
  dev.off()
  
  png(paste0("figs/","_quantile_",  "_indtime.png"), width=7000, height=3000, res=300)
  print(ggplot(quantile_indtime, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
          geom_hline(yintercept = 1, lwd = 2, color = "gray")+
          geom_point(size = 6, position=position_dodge(width=0.7)) +
          geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.5, lwd = 2, position=position_dodge(width=0.7))+
          scale_color_d3()+
          # scale_colour_gradient2(low = "firebrick",
          #                        mid = "goldenrod1",
          #                        high = "forestgreen", 
          #                        midpoint=0,
          #                        guide = "colorbar")+
          ylab("Estimate (Odds Ratio)")+
          xlab("Socio-Demographic Characteristic")+
          guides(x = guide_axis_nested(key = "&")) +
          labs(title = "Figure 2. Associations between participant factors and inhalation contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          # facet_wrap(~ Country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0))+
          coord_trans(y = specTrans))
  dev.off()
  
  # png(paste0("figs/","_quantile_",  "_touch.png"), width=7000, height=3500, res=300)
  # print(ggplot(quantile_touch, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
  #         geom_hline(yintercept = 1, lwd = 2, color = "gray")+
  #         geom_point(size = 6, position=position_dodge(width=0.7)) +
  #         geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.5, lwd = 2, position=position_dodge(width=0.7))+
  #         scale_color_d3()+
  #         # scale_colour_gradient2(low = "firebrick",
  #         #                        mid = "goldenrod1",
  #         #                        high = "forestgreen", 
  #         #                        midpoint=0,
  #         #                        guide = "colorbar")+
  #         ylab("Estimate (Odds Ratio)")+
  #         xlab("Socio-Demographic Characteristic")+
  #         guides(x = guide_axis_nested(key = "&")) +
  #         labs(title = "Figure 3. Associations between participant factors and fecal-oral contacts by country",
  #              subtitle = "GlobalMix study, 2021-2023")+
  #         # facet_wrap(~ Country, ncol=1, dir="v", scales="free_y") +
  #         # ylim(0.5, 3)+
  #         theme_classic()+
  #         theme(title = element_text(size = 24),
  #               axis.text = element_text(size = 18),
  #               text=element_text(size=18),
  #               strip.text = element_text(size = 18, hjust = 0))+
  #         coord_transform(y = specTrans) +
  #         #scale_y_break(c(40, 58), scales = 0.2))
  #         scale_y_break(c(38, 56), scales = 0.2, space = 0.2, symbol = NULL, ticklabels = NULL))
  # dev.off()
  
  # p <- ggplot(quantile_touch, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
  #              geom_hline(yintercept = 1, lwd = 2, color = "gray")+
  #              geom_point(size = 6, position=position_dodge(width=0.7)) +
  #              geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.5, lwd = 2, position=position_dodge(width=0.7))+
  #              scale_color_d3()+
  #              # scale_colour_gradient2(low = "firebrick",
  #              #                        mid = "goldenrod1",
  #              #                        high = "forestgreen", 
  #              #                        midpoint=0,
  #              #                        guide = "colorbar")+
  #              ylab("Estimate (Odds Ratio)")+
  #              xlab("Socio-Demographic Characteristic")+
  #              guides(x = guide_axis_nested(key = "&")) +
  #              labs(title = "Figure 3. Associations between participant factors and fecal-oral contacts by country",
  #                   subtitle = "GlobalMix study, 2021-2023")+
  #              # facet_wrap(~ Country, ncol=1, dir="v", scales="free_y") +
  #              # ylim(0.5, 3)+
  #              theme_classic()+
  #              theme(title = element_text(size = 24),
  #                    axis.text = element_text(size = 18),
  #                    text=element_text(size=18),
  #                    strip.text = element_text(size = 18, hjust = 0))+
  #              coord_transform(y = specTrans) +
  #              #scale_y_break(c(40, 58), scales = 0.2))
  #              scale_y_break(c(38, 56), scales = 0.2, space = 0.2, symbol = NULL, ticklabels = NULL)
  # 
  # p <- p + theme(
  #   panel.border = element_blank(),
  #   panel.background = element_blank(),
  #   panel.grid = element_blank()
  # )
  # p
  # 
  # g <- ggplotGrob(p)  # may need patchworkGrob() or ggplot_gtable(ggplot_build(p)) depending on ggbreak version
  # 
  # # find grobs that are lines/rects at the panel border
  # g$grobs[grepl("panel", g$layout$name)]  # inspect what's there
  # 
  # # once you spot the offending grob (often named like "panel-1-1.border" or similar),
  # # remove or make it invisible:
  # which_border <- which(grepl("border", sapply(g$grobs, function(x) x$name)))
  # for (i in which_border) g$grobs[[i]] <- zeroGrob()
  # 
  # grid.newpage()
  # png(paste0("figs/","_quantile_",  "_touch.png"), width=7000, height=3500, res=300)
  # grid.draw(g)
  # dev.off()
  p <- ggplot(quantile_nonhh, aes(x = predictor, y = RR, color = Country)) +
    geom_hline(yintercept = 1, lwd = 2, color = "gray") +
    geom_point(size = 6, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI),
                  width = 0.5, lwd = 2, position = position_dodge(width = 0.7)) +
    scale_color_d3() +
    scale_y_log10(trans = pseudo_log_trans(sigma = 1, base = 10),
                  breaks = c( 1, 2, 5, 10, 20, 50, 100)) +
    ylab("Estimate (Odds Ratio, log scale)") +
    xlab("Socio-Demographic Characteristic") +
    guides(x = guide_axis_nested(key = "&")) +
    labs(title = "Figure 1. Associations between participant factors and 1st vs. 4th quantile direct deposition contacts by country",
         subtitle = "GlobalMix study, 2021-2023") +
    theme_classic() +
    theme(title = element_text(size = 24),
          axis.text = element_text(size = 18),
          text = element_text(size = 18),
          strip.text = element_text(size = 18, hjust = 0))
  
  png(paste0("figs/","quantile_",  "_nonhh.png"), width=7000, height=3500, res=300)
  p
  dev.off()
  
  p <- ggplot(quantile_touch, aes(x = predictor, y = RR, color = Country)) +
    geom_hline(yintercept = 1, lwd = 2, color = "gray") +
    geom_point(size = 6, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI),
                  width = 0.5, lwd = 2, position = position_dodge(width = 0.7)) +
    scale_color_d3() +
    scale_y_log10(trans = pseudo_log_trans(sigma = 1, base = 10),
                  breaks = c( 1, 2, 5, 10, 20, 50, 100)) +
    ylab("Estimate (Odds Ratio, log scale)") +
    xlab("Socio-Demographic Characteristic") +
    guides(x = guide_axis_nested(key = "&")) +
    labs(title = "Figure 2. Associations between participant factors and 1st vs. 4th quantile inhalation contacts by country",
         subtitle = "GlobalMix study, 2021-2023") +
    theme_classic() +
    theme(title = element_text(size = 24),
          axis.text = element_text(size = 18),
          text = element_text(size = 18),
          strip.text = element_text(size = 18, hjust = 0))
  
  png(paste0("figs/","_quantile_",  "_indtime.png"), width=7000, height=3500, res=300)
  p
  dev.off()
  
  p <- ggplot(quantile_touch, aes(x = predictor, y = RR, color = Country)) +
    geom_hline(yintercept = 1, lwd = 2, color = "gray") +
    geom_point(size = 6, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI),
                  width = 0.5, lwd = 2, position = position_dodge(width = 0.7)) +
    scale_color_d3() +
    scale_y_log10(trans = pseudo_log_trans(sigma = 1, base = 10),
                  breaks = c( 1, 2, 5, 10, 25, 50, 100, 150)) +
    ylab("Estimate (Odds Ratio, log scale)") +
    xlab("Socio-Demographic Characteristic") +
    guides(x = guide_axis_nested(key = "&")) +
    labs(title = "Figure 3. Associations between participant factors and 1st vs. 4th quantile fecal-oral contacts by country",
         subtitle = "GlobalMix study, 2021-2023") +
    theme_classic() +
    theme(title = element_text(size = 24),
          axis.text = element_text(size = 18),
          text = element_text(size = 18),
          strip.text = element_text(size = 18, hjust = 0))
  
  png(paste0("figs/","_quantile_",  "_.png"), width=7000, height=3500, res=300)
  p
  dev.off()
  
}
