rm(list=ls())
library(here)
library(dplyr)
library(ggplot2)
library(plotly)
library(GGally)
library(scales)
library(ggpubr)
library(legendry)

# Read in Data -----------------------------------------------------------------

country = "Pakistan"
cty = "pak"
hhmbr = ""

urban <- read.csv(paste0(here(),"/data/", cty, hhmbr, "_urban_daily_negbin.csv")) %>%
                    filter(Term != "(Intercept)") %>%
                    select(-X) %>%
                    mutate(Characteristic = c(rep("Age\nRef: 30-39y", 8),
                                              "Sex\nRef: Female",
                                              rep("Household Size\nRef: 0-2 members", 3),
                                              rep("Occupation\nRef: Unemployed", 4), 
                                              rep("Higher Education\nRef: Primary school", 4))) %>%
                    mutate(Term = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "40-59y", "60+y",
                                    "Male",
                                    "3-5", "6-10", "11+",
                                    "Student", "Laborer", "Professional", "Other",
                                    "In school", "Secondary", "College+", "None")) %>%
                    mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                                         ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
  mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                            levels = paste(Term, Characteristic, sep = "&")))

rural <- read.csv(paste0(here(),"/data/", cty, hhmbr, "_rural_daily_negbin.csv"))%>%
  filter(Term != "(Intercept)") %>%
  select(-X) %>%
  mutate(Characteristic = c(rep("Age\nRef: 30-39y", 8),
                            "Sex\nRef: Female",
                            rep("Household Size\nRef: 0-2 members", 3),
                            rep("Occupation\nRef: Unemployed", 4), 
                            rep("Higher Education\nRef: Primary school", 4))) %>%
  mutate(Term = c("<6mo", "6-11mo", "1-4y", "5-9y", "10-19y", "20-29y", "40-59y", "60+y",
                  "Male",
                  "3-5", "6-10", "11+",
                  "Student", "Laborer", "Professional", "Other",
                  "In school", "Secondary", "College+", "None")) %>%
  mutate(Significance = ifelse((Lower_95_CI) < 1 & (Upper_95_CI) > 1, 0,
                      ifelse((Lower_95_CI) < 1 & (Upper_95_CI) < 1, -1, 1))) %>%
  mutate(predictor = factor(paste(Term, Characteristic, sep = "&"),
                            levels = paste(Term, Characteristic, sep = "&")))


# No interaction plots----------------------------------------------------------

png(paste0(here(),"/figs/", cty, "_", hhmbr, "_urban_daily_negbin.png"), width=6500, height=2500, res=300)
ggplot(urban, aes(x = predictor,
                  y = RR, color = Significance)) + 
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
  ggtitle("Urban Mozambique\nDaily Contacts ~ Age + Sex + Household Size + Occupation + Higher Education")+
  theme_classic()+
  theme(text=element_text(size=20),
        legend.position = "none")
dev.off()

png(paste0(here(),"/figs/", cty, "_", hhmbr, "_rural_daily_negbin.png"), width=3500, height=1500, res=250)
ggplot(rural, aes(x = predictor,
                  y = RR, color = Significance)) + 
  geom_point() +
  geom_hline(yintercept = 1, lty = 2, color = "gray")+
  geom_errorbar(aes(ymin=Lower_95_CI, ymax=Upper_95_CI), width = 0.5)+
  scale_colour_gradient2(low = "firebrick",
                         mid = "goldenrod1",
                         high = "forestgreen", 
                         midpoint=0,
                         guide = "colorbar")+
  ylab("Estimate (Risk Ratio)")+
  xlab("Socio-Demographic Characteristic")+
  guides(x = guide_axis_nested(key = "&")) +
  ggtitle("Urban Mozambique\nDaily Contacts ~ Age + Sex + Household Size + Occupation + Higher Education")+
  theme_classic()
dev.off()


