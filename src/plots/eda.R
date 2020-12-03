### eda

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


# load data
stages <- readr::read_csv("stages_TDF.csv")
riders <- readr::read_csv("tdf-riders.csv")
classif <- readr::read_csv("tdf_2020_classifications.csv")

stages_2020 <- readr::read_csv("scraped.csv") %>% 
  dplyr::rename(rank = Rank,
                rider = Rider,
                rider_number = `Rider No.`,
                team = Team,
                time = Times,
                bonus = B,
                penalty = P) %>% 
  dplyr::select(rank, rider, rider_number, team, time, bonus, penalty) %>% 
  dplyr::mutate(stage = cumsum(ifelse(rank == 1, 1, 0)),
                rider = stringr::str_to_title(rider),
                team = stringr::str_to_title(team)) %>% 
  dplyr::left_join(stages %>% 
                     dplyr::filter(lubridate::year(date) == 2020) %>% 
                     dplyr::mutate(Stage = as.double(Stage)),
                   by = c("stage" = "Stage")) %>% 
  dplyr::left_join(classif)


riders_2020 <- stages_2020 %>% 
  dplyr::distinct(rider, rider_number) %>% 
  dplyr::arrange(rider_number)


stages_2020 <- stages_2020 %>% 
  dplyr::left_join(riders_2020 %>% 
                     dplyr::rename(winner = rider, stage_winner = rider_number)) %>% 
  dplyr::select(-winner)


# calculate times and rankings
stages_2020 <- stages_2020 %>% 
  dplyr::mutate(time = lubridate::hms(time),
                time_seconds = lubridate::period_to_seconds(time),
                bonus = stringr::str_replace(bonus, "B : ", ""),
                bonus = stringr::str_replace(bonus, "''", ""),
                bonus = stringr::str_replace(bonus, "-", "0"),
                bonus = as.double(bonus), # convert bonus to seconds
                bonus = ifelse(stage == 21, 0, bonus), # no bonus in last stage
                penalty = stringr::str_replace(penalty, "P : ", ""),
                penalty = stringr::str_replace(penalty, "''", ""),
                penalty = stringr::str_replace(penalty, "-", "0"),
                penalty = as.double(penalty), # convert penalty to seconds
                time_seconds = time_seconds - bonus + penalty) %>%  # bonus seconds are subtracted from total time, penalties added
  dplyr::group_by(rider_number) %>% 
  dplyr::mutate(cumtime = ifelse(stage == 21, time_seconds, cumsum(time_seconds)), # calculcate rider's cumulative time by stage
                time_seconds = ifelse(stage == 21, cumtime - lag(cumtime), time_seconds)) %>% # fix stage 21 seconds 
  dplyr::ungroup() %>% 
  dplyr::mutate(time = lubridate::seconds_to_period(time_seconds), time) %>% 
  dplyr::group_by(stage) %>% 
  dplyr::arrange(cumtime, rank) %>% 
  dplyr::mutate(gc_rank = 1:n(),
                timediff = cumtime - min(cumtime)) %>% 
  dplyr::ungroup()
  

# save data
readr::write_csv(stages_2020, path = "../../data/tour-de-france-data.csv")


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
  theme(text = element_text(text_size),
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
  theme(text = element_text(text_size),
        axis.ticks.y = element_blank(),
        axis.line.y.left = element_blank())
ggsave("../../doc/fig/timediff_hist.png")


# time difference boxplots per stage
stages_2020 %>% 
  dplyr::filter(stage < 20) %>% 
  ggplot(aes(x = factor(stage), y = timediff)) +
  geom_boxplot() +
  labs(x = "stage",
       y = "time difference to leader (seconds)")  +
  theme(text = element_text(text_size))
#geom_jitter(width = 0.1, alpha = 0.5)
ggsave("../../doc/fig/timediff_stage.png")


# time difference trend by distance
stages_2020 %>% 
  ggplot(aes(x = distance, y = timediff)) +
  geom_jitter(width = 2) +
  labs(x = "stage distance (km)",
       y = "time difference to leader (seconds)") +
  theme(text = element_text(text_size))
ggsave("../../doc/fig/timediff_distance.png")


# time difference by stage and contender
stages_2020 %>% 
  dplyr::filter(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                    29, 71, 91, 121, 161)) %>% 
  ggplot(aes(x = stage, y = timediff, group = rider, color= rider)) +
  geom_line() +
  labs(x = "stage distance (km)",
       y = "time difference to leader (seconds)",
       color = "rider") +
  theme(text = element_text(text_size)) +
  scale_color_viridis_d()
ggsave("../../doc/fig/timediff_contender_distance.png")


# time difference by contender
timediff_contender <- stages_2020 %>% 
  dplyr::filter(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161)) %>% 
  ggplot(aes(x = rider, y = timediff)) +
  geom_boxplot() +
  labs(x = "rider",
       y = "time difference to leader (seconds)",
       color = "rider") +
  theme(text = element_text(text_size)) +
  coord_flip()
ggsave("../../doc/fig/timediff_contender.png", timediff_contender)


# time difference by team
timediff_team <- stages_2020 %>% 
  ggplot(aes(x = team, y = timediff)) +
  geom_boxplot() +
  labs(x = "team",
       y = "time difference to leader (seconds)",
       color = "rider") +
  theme(text = element_text(text_size)) +
  coord_flip()
ggsave("../../doc/fig/timediff_team.png", timediff_team)


# glue time diff by team and rider in one plot

timediff_contender <- timediff_contender + 
  labs(tag = "(a)")
timediff_team <- timediff_team + 
  labs(tag = "(b)")
timediff_contender_team <- timediff_contender / timediff_team
ggsave("../../doc/fig/timediff_contender_team.png", timediff_contender_team, height = 12)


# time boxplots per stage
stages_2020 %>% 
  dplyr::filter(stage < 20) %>% 
  ggplot(aes(x = factor(stage), y = time_seconds)) +
  geom_boxplot() +
  labs(x = "stage",
       y = "time (seconds)")  +
  theme(text = element_text(text_size))
#geom_jitter(width = 0.1, alpha = 0.5)
ggsave("../../doc/fig/time_stage.png")

# time trend by distance and stage
stages_2020 %>% 
  ggplot(aes(x = distance, y = time_seconds, color = factor(stage))) +
  geom_point() +
  scale_color_viridis_d() +
  theme(text = element_text(text_size))


# time trend by distance
stages_2020 %>% 
  ggplot(aes(x = distance, y = time_seconds)) +
  geom_point() +
  labs(x = "stage distance (km)",
       y = "time (seconds)") +
  theme(text = element_text(text_size))
ggsave("../../doc/fig/time_distance.png")


# time qqplot
stages_2020 %>% 
  ggplot(aes(sample = time_seconds)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "theoretical",
       y = "sample") +
  theme(text = element_text(text_size))
ggsave("../../doc/fig/time_qqplot.png")

# histogram of time
stages_2020 %>% 
  ggplot() +
  geom_histogram(aes(time_seconds), color = "black", bins = 20,
                 fill = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = NULL) +
  labs(x = "time (seconds)",
       y = "") +
  theme(text = element_text(text_size),
        axis.ticks.y = element_blank(),
        axis.line.y.left = element_blank())
ggsave("../../doc/fig/time_hist.png")

# cumtime per stage
stages_2020 %>% 
  ggplot(aes(x = factor(stage), y = cumtime)) +
  geom_jitter(width = 0.2) +
  labs(x = "stage",
       y = "cumulative time (s)") +
  theme(text = element_text(text_size))
ggsave("../../doc/fig/cumtime_stage.png")



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




# modelling
training <- stages_2020 %>% 
  dplyr::filter(stage < 20)

# model 1
mod1 <- glm(gc_rank ~ distance + stage_type + rider, data = training, family = poisson(link = "log"))

tibble(rank = training$gc_rank,
       fitted = mod1$fitted.values,
       residuals = rank - fitted,
       mod_res = mod1$residuals,
       norm_res = (rank - fitted) / sqrt(fitted)) %>% 
  ggplot(aes(x = fitted, y = norm_res)) +
  geom_point() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = -1, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values",
       y = "Normalized residuals")

# model 2
training <- training %>% 
  dplyr::mutate(contender = ifelse(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161), rider, "Other"))


mod2 <- glm(gc_rank ~ distance + stage_type + stage + contender, data = training, family = poisson(link = "log"))
summary(mod2)

tibble(rank = training$gc_rank,
       fitted = mod2$fitted.values,
       residuals = rank - fitted,
       mod_res = mod2$residuals,
       norm_res = (rank - fitted) / sqrt(fitted)) %>% 
  ggplot(aes(x = fitted, y = norm_res)) +
  geom_jitter(width = 0.9) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = -1, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values",
       y = "Normalized residuals")



# model 3

mod3 <- glm.nb(gc_rank ~ distance + stage_type + stage + rider, data = training)
summary(mod3)

tibble(rank = training$gc_rank,
       fitted = mod3$fitted.values,
       residuals = rank - fitted,
       mod_res = mod3$residuals,
       norm_res = (rank - fitted) / sqrt(fitted + (1/mod3$theta) * fitted^2)) %>% 
  ggplot(aes(x = fitted, y = norm_res)) +
  geom_jitter(width = 0.9) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  geom_hline(yintercept = -1, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values",
       y = "Normalized residuals")



# basic lm
lm_mod <- lm(cumtime ~ stage + distance + stage_type, data = stages_2020)
summary(lm_mod)
#plot(lm_mod)

tibble(x = lm_mod$fitted.values, 
       y = lm_mod$residuals) %>% 
  ggplot(aes(x, y)) +
  geom_jitter() +
  geom_hline(yintercept = 0) +
  labs(x = "fitted values",
       y = "normalized residuals")


# basic lm for time instead of cumtime
training <- stages_2020 %>% 
  dplyr::mutate(contender = ifelse(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161), rider, "Other"))

lm_mod <- lm(time_seconds ~ stage + distance + contender, data = training %>% dplyr::filter(stage < 20))
summary(lm_mod)
#plot(lm_mod)

tibble(x = lm_mod$fitted.values, 
       y = lm_mod$residuals,
       y_std = rstandard(lm_mod)) %>% 
  ggplot(aes(x, y_std)) +
  geom_jitter() +
  geom_hline(yintercept = 0) +
  labs(x = "fitted values",
       y = "normalized residuals")

pred_data <- training %>%
  dplyr::filter(stage == 20) %>% 
  dplyr::select(distance, contender)

#preds <- predict(lm_mod, newdata = pred_data)

#pred_data <- pred_data %>% 
#  dplyr::mutate(mean_prediction = preds) %>% 
#  dplyr::arrange(mean_prediction)
#pred_data

#stages_2020 %>% 
#  dplyr::filter(stage == 20) %>% 
#  dplyr::select(rider, timediff) %>% 
#  dplyr::mutate(mean_prediction = preds) %>% 
#  dplyr::arrange(timediff)


# tweedie 1
tweedie_training <- stages_2020 %>% 
  #dplyr::filter(stage < 20) %>% 
  dplyr::mutate(contender = ifelse(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161), rider, "Other"),
                tp = ifelse(rider_number %in% c(131, 11), rider, "Other"),
                stage_type = stringr::str_replace(stage_type, "Mountain time trial", "Mountain stage"))
  
tweedie1 <- glm(timediff ~ stage + distance + team + contender, data = tweedie_training %>% dplyr::filter(stage < 20),
                family = tweedie(var.power = 1.5,link.power = 0))
summary(tweedie1)
#plot(tweedie1)

pred_data <- tweedie_training %>%
  dplyr::filter(stage == 20) %>% 
  dplyr::select(stage, distance, team, contender)

preds <- exp(predict(tweedie1, newdata = pred_data)) # exp to get in scale of mean and not log scale
phi <- 17.95275

pred_data <- pred_data %>% 
  dplyr::mutate(prediction = preds,
                logpred = log(preds),
                min_prediction = prediction - min(prediction),
                lambda = 2* sqrt(logpred) / phi,
                prob0 = exp(-lambda)) %>% 
  dplyr::arrange(prob0)
pred_data

stages_2020 %>% 
  dplyr::filter(stage == 20) %>% 
  dplyr::select(rider, timediff) %>% 
  dplyr::mutate(prediction = preds,
                min_prediction = prediction - min(prediction),
                lambda = 2* sqrt(prediction) / phi,
                prob0 = exp(-lambda)) %>% 
  dplyr::arrange(desc(prob0))


# residuals
tibble(x = tweedie1$fitted.values,
       tmp_y = tweedie_training %>% filter(stage<20) %>% pull(timediff),
       y = (x - tmp_y) / sqrt(phi * x^(1.5))) %>% 
  ggplot(aes(x, y)) +
  geom_jitter(height = 0.4) +
  geom_hline(yintercept = 3, linetype = "dotted") +
  geom_hline(yintercept = -3, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values",
       y = "Normalized residuals") +
  scale_y_continuous(breaks = -5:5) +
  theme(text = element_text(size = text_size))
ggsave("../../doc/fig/norm_res.png")  


# fitted timediff by stage
colors = c("true" = "#39558CFF", "fitted" = "#56C667FF", "predicted" = "#E55C30FF")

tibble(x = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(stage),
       fitted = tweedie1$fitted.values,
       true = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(timediff)) %>% 
  ggplot(aes(x, fitted)) +
  geom_point(aes(color = "fitted"), alpha = 0.9) +
  geom_point(aes(x + 0.25, true, color = "true"), alpha = 0.9) +
  geom_point(data = tibble(xx = 20.25, true = stages_2020 %>% dplyr::filter(stage == 20) %>% pull(timediff)),
             aes(xx, true, color = "true")) +
  geom_point(data = tibble(xx = 20, predicted = preds),
             aes(xx, predicted, color = "predicted")) +
  labs(x = "stage",
       y = "time difference to leader (seconds)",
       color = "") +
  scale_color_manual(values = colors, breaks = c("true", "fitted", "predicted")) +
  theme(text = element_text(size = text_size),
        legend.position = "top")
ggsave("../../doc/fig/fitted_predicted.png")

# top 10 predicted vs actual
stages_2020 %>% 
  dplyr::filter(stage == 20) %>% 
  dplyr::select(rider, gc_rank, timediff) %>% 
  dplyr::mutate(prediction = preds,
                lambda = 2* sqrt(prediction) / phi,
                prob0 = exp(-lambda)) %>% 
  dplyr::arrange(desc(prob0)) %>% 
  dplyr::mutate(pred_gc_rank = 1:n()) %>% 
  dplyr::top_n(10, wt = -gc_rank) %>% 
  dplyr::arrange(gc_rank) %>% 
  dplyr::select(rider, gc_rank, pred_gc_rank, prob0)







