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

plot_simple_regression_results <- function(hhmbr = "", prctl = 75, name = "daily_5"){
  
  # Read in Data ---------
  total_nonhh <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_nonhh.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique")))
  total_indtime <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_indtime.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique")))
  total_touch <- read.csv(paste0("results/", "moz", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_negbin_", name, "_touch.csv")) %>% mutate(country = "Pakistan") ) %>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique")))
  
  outlier_nonhh <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_nonhh.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique"))) %>%
    rename(Country = country)
  outlier_indtime <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_indtime.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique"))) %>%
    rename(Country = country)
  outlier_touch <- read.csv(paste0("results/", "moz", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "Mozambique") %>%
    rbind(read.csv(paste0("results/", "ind", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "India") ) %>%
    rbind(read.csv(paste0("results/", "gt", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "Guatemala") ) %>%
    rbind(read.csv(paste0("results/", "pak", hhmbr,"_",prctl,"_outlier_", name, "_touch.csv")) %>% mutate(country = "Pakistan"))%>%
    mutate(predictor = factor(predictor, levels = unique(predictor))) %>%
    mutate(country = factor(country, levels = c("Guatemala", "India", "Pakistan", "Mozambique")))%>%
    rename(Country = country)
  
  # Total --------
  title = "Age + Sex + Household Size + Occupation + Higher Education"
  if(name == "daily_5"){
    title = "Age + Sex + Household Size + Occupation"
  }
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_nonhh.png"), width=7000, height=3000, res=300)
  print(ggplot(total_nonhh, aes(x = predictor,y = RR, color = country)) +#, color = Significance)) + 
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
          labs(title = "Associations between participant factors and number of non-household contacts by country",
               subtitle = "GlobalMix study, 2021-2023")+
          # facet_wrap(~ country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0)))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_indtime.png"), width=7000, height=3000, res=300)
  print(ggplot(total_indtime, aes(x = predictor,y = RR, color = country))+ #, color = Significance)) + 
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
          labs(title = "Associations between participant factors and indoor exposure-hours by Country",
               subtitle = "GlobalMix study, 2021-2023")+
          # facet_wrap(~ Country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0)))
  dev.off()
  
  png(paste0("figs/",hhmbr,"negbin_", name, "_touch.png"), width=7000, height=3000, res=300)
  print(ggplot(total_touch, aes(x = predictor,y = RR, color = country))+#, color = Significance)) + 
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
          labs(title = "Associations between participant factors and non-household contacts involving touch by country",
               subtitle = "GlobalMix study, 2021-2023")+
          # facet_wrap(~ Country, ncol=1, dir="v", scales="free_y") +
          # ylim(0.5, 3)+
          theme_classic()+
          theme(title = element_text(size = 24),
                axis.text = element_text(size = 18),
                text=element_text(size=18),
                strip.text = element_text(size = 18, hjust = 0)))
  dev.off()
  
  # Scaling outlier bounds -------
  fspec = function(x) ifelse(x<5, x, 5+(x-5)/10)
  fspec_1 = function(x) ifelse(x<5, x, 5+(x-5)*10)
  
  specTrans = trans_new(name = "specialTras",
                        transform = fspec,
                        inverse = fspec_1,
                        breaks = c(0, 1, 2, 3, 4, 10, 20, 50, 100, 150))
  
  # Outlier ---------
  
  # png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_nonhh.png"), width=7000, height=3000, res=300)
  #print(
  #labs(title = "Figure 1. Associations between participant factors and direct deposition contacts by country",
       #      subtitle = "GlobalMix study, 2021-2023")+
  p <-  ggplot(outlier_nonhh, aes(x = predictor, y = RR, color = Country)) +
    geom_hline(yintercept = 1, lwd = 1, color = "gray") +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI), width = 0.4, lwd = 0.8,
                  position = position_dodge(width = 0.7)) +
    scale_color_d3() +
    ylab("Estimate (Odds Ratio)") +
    xlab("Socio-Demographic Characteristic") +
    guides(x = guide_axis_nested(key = "&")) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 10),#, angle = 45),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 10, hjust = 0),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    ) +
    scale_y_log10(
      trans = pseudo_log_trans(sigma = 1, base = 10),
      breaks = c(1, 2, 3, 4, 6, 8, 10, 15, 20)
    )
  
  tiff(paste0("figs/", prctl, "_", name, "_nonhh.tiff"),
       width = 3543, height = 1772, res = 300, compression = "lzw")
  print(p)
  dev.off()
  
  # png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_indtime.png"), width=7000, height=3000, res=300)
  p <- ggplot(outlier_indtime, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
          geom_hline(yintercept = 1, lwd = 1, color = "gray") +
          geom_point(size = 3, position = position_dodge(width = 0.7)) +
          geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI), width = 0.4, lwd = 0.8,
                        position = position_dodge(width = 0.7)) +
          scale_color_d3() +
          ylab("Estimate (Odds Ratio)") +
          xlab("Socio-Demographic Characteristic") +
          guides(x = guide_axis_nested(key = "&")) +
          theme_classic() +
          theme(
            axis.text.x = element_text(size = 10),#, angle = 45),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size = 11),
            strip.text = element_text(size = 10, hjust = 0),
            legend.position = "bottom",
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 10),
            plot.title = element_blank(),
            plot.subtitle = element_blank()
          ) +
          scale_y_log10(trans = pseudo_log_trans(sigma = 1, base = 10),
                        breaks = c( 1, 2, 3, 4, 6, 8, 10, 15, 20)) 
  # dev.off()
  tiff(paste0("figs/", prctl, "_", name, "_indtime.tiff"),
       width = 3543, height = 1772, res = 300, compression = "lzw")
  print(p)
  dev.off()
  
  # png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_touch.png"), width=7000, height=3500, res=300)
  # print(ggplot(outlier_touch, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
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
  
  # p <- ggplot(outlier_touch, aes(x = predictor,y = RR, color = Country))+#, color = Significance)) +
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
  # png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_touch.png"), width=7000, height=3500, res=300)
  # grid.draw(g)
  # dev.off()
  # png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_touch.png"), width=7000, height=3000, res=300)
  p <- ggplot(outlier_touch, aes(x = predictor, y = RR, color = Country)) +
    geom_hline(yintercept = 1, lwd = 1, color = "gray") +
    geom_point(size = 3, position = position_dodge(width = 0.7)) +
    geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI), width = 0.4, lwd = 0.8,
                  position = position_dodge(width = 0.7)) +
    scale_color_d3() +
    ylab("Estimate (Odds Ratio)") +
    xlab("Socio-Demographic Characteristic") +
    guides(x = guide_axis_nested(key = "&")) +
    theme_classic() +
    theme(
      axis.text.x = element_text(size = 10),#, angle = 45),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 11),
      strip.text = element_text(size = 10, hjust = 0),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 10),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    ) +
    scale_y_log10(trans = pseudo_log_trans(sigma = 1, base = 10),
                  breaks = c( 1, 2, 3, 4, 6, 10, 15, 25, 50, 100, 150)) 

  tiff(paste0("figs/", prctl, "_", name, "_touch.tiff"),
       width = 3543, height = 1772, res = 300, compression = "lzw")
  print(p)
  dev.off()
  
  # # shared theme/layers
  # base_layers <- list(
  #   geom_hline(yintercept = 1, lwd = 2, color = "gray"),
  #   geom_point(size = 6, position = position_dodge(width = 0.7)),
  #   geom_errorbar(aes(ymin = Lower_95_CI, ymax = Upper_95_CI),
  #                 width = 0.5, lwd = 2, position = position_dodge(width = 0.7)),
  #   scale_color_d3(),
  #   theme_classic(),
  #   theme(axis.text = element_text(size = 18),
  #         text = element_text(size = 18),
  #         strip.text = element_text(size = 18, hjust = 0))
  # )
  # 
  # # TOP panel: zoomed into the high range, x-axis stripped
  # p_top <- ggplot(outlier_touch, aes(x = predictor, y = RR, color = Country)) +
  #   base_layers +
  #   coord_cartesian(ylim = c(56, 170)) +
  #   scale_y_continuous(breaks = c(60, 100, 140, 180)) +
  #   labs(title = "Figure 3. Associations between participant factors and fecal-oral contacts by country",
  #        subtitle = "GlobalMix study, 2021-2023",
  #        x = NULL, y = NULL) +
  #   theme(axis.text.x = element_blank(),
  #         axis.ticks.x = element_blank(),
  #         axis.line.x = element_blank(),
  #         legend.position = "none",
  #         plot.margin = margin(b = 0))
  # 
  # # BOTTOM panel: full detail below 50, with x-axis + legend
  # p_bottom <- ggplot(outlier_touch, aes(x = predictor, y = RR, color = Country)) +
  #   base_layers +
  #   coord_cartesian(ylim = c(0, 38)) +
  #   guides(x = guide_axis_nested(key = "&")) +
  #   ylab("Estimate (Odds Ratio)") +
  #   xlab("Socio-Demographic Characteristic") +
  #   theme(plot.margin = margin(t = 0))
  # 
  # png(paste0("figs/",hhmbr,prctl,"_outlier_", name, "_touch.png"), width=7000, height=3500, res=300)
  # # stack them, top panel gets less vertical space
  # print(p_top / p_bottom + plot_layout(heights = c(1, 3), guides = "collect"))
  # dev.off()
  
  
}
