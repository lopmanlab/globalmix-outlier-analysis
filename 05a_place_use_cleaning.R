####################################################
#Exploring Place Use Data for Airborne Transmission#
####################################################

rm(list=ls())
pacman::p_load(here,
               tidyverse,
               scales)


# Reading and cleaning data ----------------------------------------------------

placeuse_raw <- readRDS(paste0(here(),"/../","globalmix-mozambique/data/clean/locations_visited_aim1.RDS"))

placeuse_clean <- placeuse_raw %>%
  mutate(time_visited = factor(time_visited, levels=c("<5 mins", "5-15 mins", "16-30 mins", "31 mins-1 hr", "1-4 hrs", ">4 hrs")))%>%
  mutate(num_pax_place = as.numeric(num_pax_place)) 

contacts_resp_ent <- readRDS(here("data/contacts_resp_ent.RDS"))

# Summary tables -------------------------------------------------------------

summary(placeuse_clean %>% group_by(place_visited) %>% dplyr::select(num_pax_place))
tapply(placeuse_clean$num_pax_place, (placeuse_clean$place_visited), summary)

png("figs/place_by_time.png", height = 750, width = 1250, res = 200)
ggplot(placeuse_clean %>% 
         drop_na(place_visited, time_visited), 
       aes(fill=time_visited, y=place_visited)) + 
  geom_bar(position="fill", stat="count")
dev.off()

png("figs/pax_by_place_time.png", height = 750, width = 1500, res = 200)
ggplot(placeuse_clean %>%
         drop_na(place_visited, time_visited) %>%
         group_by(place_visited, time_visited) %>%
         summarise(avg_num_pax = mean(num_pax_place, na.rm = TRUE),
                   med_num_pax = median(num_pax_place, na.rm = TRUE)), 
       aes(x=time_visited, y=place_visited)) +
  geom_raster(aes(fill = avg_num_pax))+
  scale_fill_gradient(low = 'white',high = 'steelblue')+
  geom_text(aes(label=paste0(round(avg_num_pax), " (", med_num_pax, ")")))+
  guides(fill=guide_legend(title="Number of People:\nMean (Median)"))
dev.off()

png("figs/pax_by_place_time_hist.png", height = 3000, width = 1500, res = 200)
ggplot(placeuse_clean %>%
         drop_na(place_visited, time_visited)) +
  geom_histogram(aes(x = num_pax_place, color = num_pax_place))+
  facet_grid(place_visited ~ time_visited) +
  scale_fill_gradient(low = 'white',high = 'steelblue')+
  theme_bw()
dev.off()

placeuse_clean <- placeuse_clean %>%
  mutate(duration = case_when(time_visited == "<5 mins" ~ 5/2,
                               time_visited == "5-15 mins" ~ 20/2,
                               time_visited == "16-30 mins" ~ (30+16)/2,
                               time_visited == "31 mins-1 hr" ~ (31+60)/2,
                               time_visited == "1-4 hrs" ~ 60*(4+1)/2,
                               time_visited == ">4 hrs" ~ 4*60,
                               .default = NA)) %>%
  mutate(person_hours = num_pax_place * (duration/60)) 

placeuse_clean %>%
  dplyr::select(num_pax_place, duration, person_hours)

table(placeuse_clean$time_visited)
table(placeuse_clean$time_visited, placeuse_clean$duration)

hist(placeuse_clean$person_hours)
summary(placeuse_clean$person_hours)

ggplot(placeuse_clean) +
  geom_histogram(aes(x = person_hours), binwidth=1)+
  theme_bw()+
  geom_vline(xintercept = 12.5, color = "red")+
  geom_vline(xintercept = 23.3, color = "blue")+
  xlim(0, 150)+
  ylim(0, 700)

# How to combine days / visit frequency?
table(placeuse_clean$study_day)
table(placeuse_clean$visited_frequency)
names(placeuse_clean)

placeuse_byID <- placeuse_clean %>%
  group_by(rec_id) %>%
  summarise(sum_person_hours = sum(person_hours)) %>%
  left_join(., contacts_resp_ent %>% 
              ungroup() %>%
              dplyr::select(rec_id, unique_resp_q90_outlier, unique_ent_q90_outlier))


summary(placeuse_byID$sum_person_hours)
quantile(placeuse_byID$sum_person_hours, 0.75, na.rm = T)

png(filename = "figs/placeuse.png")
ggplot(placeuse_byID) +
  geom_histogram(aes(x = sum_person_hours))+
  theme_bw()+
  geom_vline(xintercept = 133, color = "red")+
  geom_vline(xintercept = 94, color = "blue")+
  xlim(0, 1000)
dev.off()

png(filename = "figs/respiratory_placeuse.png")
ggplot(placeuse_byID) +
  geom_histogram(aes(x = sum_person_hours, fill = factor(unique_resp_q90_outlier)))+
  theme_bw()+
  facet_grid(rows = vars(unique_resp_q90_outlier), scales = "free")+
  ggtitle("Sum of place use person hours over 2 days by respiratory outlier status")+
  geom_vline(xintercept = 133, lty = 1)+
  geom_vline(xintercept = 94, lty = 2)+
  xlim(0, 1000)
dev.off()

png(filename = "figs/enteric_placeuse.png")
ggplot(placeuse_byID) +
  geom_histogram(aes(x = sum_person_hours, fill = factor(unique_ent_q90_outlier)))+
  theme_bw()+
  facet_grid(rows = vars(unique_ent_q90_outlier), scales = "free")+
  ggtitle("Sum of place use person hours over 2 days by enteric outlier status")+
  geom_vline(xintercept = 133, lty = 1)+
  geom_vline(xintercept = 94, lty = 2)+
  xlim(0, 1000)
dev.off()
