### exploratory data analysis
# this code loads the final data set from tidy-data.R
# and creates the visualizations included in the report
# (and many more!)
# it also saves the figures in the appropriate folder
#########
# author: Gian Carlo Diluvi
#########


# preamble ####
library(MASS)
library(tidyverse)
library(readr)
library(ggplot2)
library(patchwork)
library(lubridate)
library(stringr)
library(statmod) #tweedie glm
ggplot2::theme_set(theme_classic())


# load data ####
stages_2020 <- readr::read_csv("../data/tour-de-france-data.csv")


# data summaries ####

glimpse(stages_2020)

# riders by stage
stages_2020 %>% 
  dplyr::select(stage, rider_number) %>% 
  dplyr::group_by(stage) %>% 
  dplyr::summarise(n = n())

# different stage types
stages_2020 %>% 
  dplyr::distinct(stage_type)




# viz ####
text_size <- 18

# time differences boxplots per stage
stages_2020 %>% 
  ggplot(aes(x = factor(stage), y = timediff)) +
  geom_boxplot() 
#geom_jitter(width = 0.1, alpha = 0.5)

# time differences histogram
stages_2020 %>% 
  ggplot() +
  geom_histogram(aes(timediff), color = "black", bins = 20,
                 fill = "#FDE725FF") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = NULL) +
  labs(x = "time difference to leader (seconds)",
       y = "",
       title = "Tour de France 2020: distribution of time difference to GC leader") +
  theme(text = element_text(size = text_size),
        axis.ticks.y = element_blank(),
        axis.line.y.left = element_blank())

# time differences histogram
stages_2020 %>% 
  ggplot() +
  geom_histogram(aes(timediff), color = "black", bins = 20,
                 fill = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = NULL) +
  labs(x = "time difference to leader (seconds)",
       y = "") +
  theme(text = element_text(size = text_size),
        axis.ticks.y = element_blank(),
        axis.line.y.left = element_blank())
ggsave("../doc/fig/timediff_hist.png")


# time difference boxplots per stage
stages_2020 %>% 
  dplyr::filter(stage < 20) %>% 
  ggplot(aes(x = factor(stage), y = timediff)) +
  geom_boxplot() +
  labs(x = "stage",
       y = "time difference to leader (seconds)")  +
  theme(text = element_text(size = text_size))
#geom_jitter(width = 0.1, alpha = 0.5)
ggsave("../doc/fig/timediff_stage.png")


# time difference trend by distance
stages_2020 %>% 
  ggplot(aes(x = distance, y = timediff)) +
  geom_jitter(width = 2) +
  labs(x = "stage distance (km)",
       y = "time difference to leader (seconds)") +
  theme(text = element_text(size = text_size))
ggsave("../doc/fig/timediff_distance.png")


# time difference by stage and contender
stages_2020 %>% 
  dplyr::filter(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                    29, 71, 91, 121, 161)) %>% 
  ggplot(aes(x = stage, y = timediff, group = rider, color= rider)) +
  geom_line() +
  labs(x = "stage distance (km)",
       y = "time difference to leader (seconds)",
       color = "rider") +
  theme(text = element_text(size = text_size)) +
  scale_color_viridis_d()
ggsave("../doc/fig/timediff_contender_distance.png")


# time difference by contender
timediff_contender <- stages_2020 %>% 
  dplyr::filter(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                    29, 71, 91, 121, 161)) %>% 
  ggplot(aes(x = rider, y = timediff)) +
  geom_boxplot() +
  labs(x = "rider",
       y = "time difference to leader (seconds)",
       color = "rider") +
  theme(text = element_text(size = text_size)) +
  coord_flip()
ggsave("../doc/fig/timediff_contender.png", timediff_contender)


# time difference by team
timediff_team <- stages_2020 %>% 
  ggplot(aes(x = team, y = timediff)) +
  geom_boxplot() +
  labs(x = "team",
       y = "time difference to leader (seconds)",
       color = "rider") +
  theme(text = element_text(size = text_size)) +
  coord_flip()
ggsave("../doc/fig/timediff_team.png", timediff_team)


# glue time diff by team and rider in one plot

timediff_contender <- timediff_contender + 
  labs(tag = "(a)")
timediff_team <- timediff_team + 
  labs(tag = "(b)")
timediff_contender_team <- timediff_contender / timediff_team
ggsave("../doc/fig/timediff_contender_team.png", timediff_contender_team, height = 12)


# time boxplots per stage
stages_2020 %>% 
  dplyr::filter(stage < 20) %>% 
  ggplot(aes(x = factor(stage), y = time_seconds)) +
  geom_boxplot() +
  labs(x = "stage",
       y = "time (seconds)")  +
  theme(text = element_text(size = text_size))
#geom_jitter(width = 0.1, alpha = 0.5)
ggsave("../doc/fig/time_stage.png")

# time trend by distance and stage
stages_2020 %>% 
  ggplot(aes(x = distance, y = time_seconds, color = factor(stage))) +
  geom_point() +
  scale_color_viridis_d() +
  theme(text = element_text(size = text_size))


# time trend by distance
stages_2020 %>% 
  ggplot(aes(x = distance, y = time_seconds)) +
  geom_point() +
  labs(x = "stage distance (km)",
       y = "time (seconds)") +
  theme(text = element_text(size = text_size))
ggsave("../doc/fig/time_distance.png")


# time qqplot
stages_2020 %>% 
  ggplot(aes(sample = time_seconds)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "theoretical",
       y = "sample") +
  theme(text = element_text(size = text_size))
ggsave("../doc/fig/time_qqplot.png")

# histogram of time
stages_2020 %>% 
  ggplot() +
  geom_histogram(aes(time_seconds), color = "black", bins = 20,
                 fill = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = NULL) +
  labs(x = "time (seconds)",
       y = "") +
  theme(text = element_text(size = text_size),
        axis.ticks.y = element_blank(),
        axis.line.y.left = element_blank())
ggsave("../doc/fig/time_hist.png")

# cumtime per stage
stages_2020 %>% 
  ggplot(aes(x = factor(stage), y = cumtime)) +
  geom_jitter(width = 0.2) +
  labs(x = "stage",
       y = "cumulative time (s)") +
  theme(text = element_text(size = text_size))
ggsave("../doc/fig/cumtime_stage.png")



# top contenders
top_5 <- stages_2020 %>% 
  dplyr::filter(stage == 21) %>% 
  dplyr::top_n(-5, gc_rank)

# top contenders + Egan Bernal plot
stages_2020 %>% 
  dplyr::filter(rider_number %in% c(top_5$rider_number, 1), stage > 4) %>% 
  dplyr::mutate(rider = factor(rider, levels = c(top_5$rider, "Egan Bernal"))) %>% 
  ggplot(aes(x = stage, y = gc_rank, group = rider, color = rider)) +
  geom_line(size = 2) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = 1:21) +
  labs(x = "Stage",
       y = "General classification position",
       color = "Cyclist",
       title = "Top GC contenders in Le Tour 2020",
       subtitle = "(stage 5 onwards)") +
  theme(text = element_text(size = text_size))

# avg speed by type of stage
stages_2020 %>% 
  ggplot(aes(x = stage_type, y = 60*60*distance / time_seconds)) +
  geom_boxplot() +
  labs(x = "Stage type",
       y = "Average speed (km/h)")


# avg speed by stage
stages_2020 %>% 
  ggplot(aes(x = as.factor(stage), y = 60*60*distance / time_seconds)) +
  geom_boxplot() +
  labs(x = "Stage",
       y = "Average speed (km/h)")



# contender plots
stages_2020 %>% 
  dplyr::filter(stage < 20 & stage > 4) %>% 
  dplyr::mutate(contender = ifelse(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161), rider, "Other")) %>% 
  dplyr::filter(contender != "Other") %>% 
  ggplot(aes(x = gc_rank)) +
  geom_histogram() +
  facet_wrap(.~contender) +
  labs(x = "General classification position",
       y = "") +
  theme(text = element_text(size = text_size))