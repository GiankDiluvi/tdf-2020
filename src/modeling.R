### modeling
# this code loads the final data set from tidy-data.R
# and fits the two tweedie regression models
# it also generates the figures included in the results section
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
text_size <- 18

# modelling ####

# tweedie 1: log response function
tweedie_training <- stages_2020 %>% 
  #dplyr::filter(stage < 20) %>% 
  dplyr::mutate(contender = ifelse(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161), rider, "Other"),
                tp = ifelse(rider_number %in% c(131, 11), rider, "Other"),
                stage_type = stringr::str_replace(stage_type, "Mountain time trial", "Mountain stage"))

tweedie1 <- glm(timediff ~ stage + distance + team + contender, data = tweedie_training %>% dplyr::filter(stage < 20),
                family = tweedie(var.power = 1.5, link.power = 0))
summary(tweedie1)
#plot(tweedie1)


pred_data <- tweedie_training %>%
  dplyr::filter(stage == 20) %>% 
  dplyr::select(stage, distance, team, contender)

preds <- exp(predict(tweedie1, newdata = pred_data)) # exp to get in scale of mean and not log scale
preds_log <- preds
phi <- 17.95275
phi_log <- 17.95275


stages_2020 %>% 
  dplyr::filter(stage == 20) %>% 
  dplyr::select(rider, timediff) %>% 
  dplyr::mutate(prediction = preds,
                min_prediction = prediction - min(prediction),
                lambda = 2* sqrt(prediction) / phi,
                prob0 = exp(-lambda)) %>% 
  dplyr::arrange(desc(prob0))


# residuals
log_residuals <- tibble(x = tweedie1$fitted.values,
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
ggsave("../doc/fig/norm_res.png", log_residuals)  


# fitted timediff by stage
colors = c("true" = "#39558CFF", "fitted" = "#56C667FF", "predicted" = "#E55C30FF")

log_predicted <- tibble(x = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(stage),
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
ggsave("../doc/fig/fitted_predicted.png", log_predicted)



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








# tweedie 2: canonical response function
# canonical response
tweedie_training <- stages_2020 %>% 
  #dplyr::filter(stage < 20) %>% 
  dplyr::mutate(contender = ifelse(rider_number %in% c(131, 11, 101, 61, 94, 141, 1, 3, 14, 
                                                       29, 71, 91, 121, 161), rider, "Other"),
                tp = ifelse(rider_number %in% c(131, 11), rider, "Other"),
                stage_type = stringr::str_replace(stage_type, "Mountain time trial", "Mountain stage"))

tweedie2 <- glm(timediff ~ stage + distance + team + contender, data = tweedie_training %>% dplyr::filter(stage < 20),
                family = tweedie(var.power = 1.5, link.power=-0.5))
summary(tweedie2)
#plot(tweedie2)


pred_data <- tweedie_training %>%
  dplyr::filter(stage == 20) %>% 
  dplyr::select(stage, distance, team, contender)

preds <- (predict(tweedie2, newdata = pred_data))^(-2) # sqrt to get in scale of mean and not log scale
phi <- 21.27573
phi_canonical <- 21.27573


stages_2020 %>% 
  dplyr::filter(stage == 20) %>% 
  dplyr::select(rider, timediff) %>% 
  dplyr::mutate(prediction = preds,
                min_prediction = prediction - min(prediction),
                lambda = 2* sqrt(prediction) / phi,
                prob0 = exp(-lambda)) %>% 
  dplyr::arrange(desc(prob0))


# residuals
canonical_residuals <- tibble(x = tweedie2$fitted.values,
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
ggsave("../doc/fig/norm_res_canonical.png", canonical_residuals)  


# fitted timediff by stage
colors = c("true" = "#39558CFF", "fitted" = "#56C667FF", "predicted" = "#E55C30FF")

canonical_predicted <- tibble(x = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(stage),
                              fitted = tweedie2$fitted.values,
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
ggsave("../doc/fig/fitted_predicted_canonical.png", canonical_predicted)






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



# glue predictions
colors = c("true" = "#711A6EFF", "log" = "#D64B40FF", "canonical" = "#FBB91FFF")


tibble(x = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(stage),
       fitted = tweedie1$fitted.values,
       true = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(timediff)) %>% 
  ggplot(aes(x, true)) +
  geom_point(aes(color = "true"), alpha = 0.2) +
  geom_point(aes(x + 0.25, fitted, color = "log"), alpha = 0.9) +
  geom_point(data = tibble(xx = 20, true = stages_2020 %>% dplyr::filter(stage == 20) %>% pull(timediff)),
             aes(xx, true, color = "true"), alpha = 0.2) +
  geom_point(data = tibble(xx = 20.25, predicted = preds_log),
             aes(xx, predicted, color = "log")) +
  geom_point(data = tibble(x = stages_2020 %>% dplyr::filter(stage < 20) %>% pull(stage),
                           fitted = tweedie2$fitted.values),
             aes(x = x + 0.5, y = fitted, color = "canonical"), alpha = 0.9) +
  geom_point(data = tibble(xx = 20.5, predicted = preds),
             aes(xx, predicted, color = "canonical")) +
  labs(x = "stage",
       y = "time difference to leader (seconds)",
       color = "") +
  scale_color_manual(values = colors, breaks = c("true", "log", "canonical"),
                     labels = c("True", "Log response", "Canonical response")) +
  theme(text = element_text(size = text_size),
        legend.position = "top")
ggsave("../doc/fig/fitted_predicted_comparison.png")


# glue residuals
colors = c("log" = "#D64B40FF", "canonical" = "#FBB91FFF")


tibble(x_log = tweedie1$fitted.values,
       x_canonical = tweedie2$fitted.values,
       tmp_y = tweedie_training %>% filter(stage<20) %>% pull(timediff),
       y_log = (x_log - tmp_y) / sqrt(phi_log * x_log^(1.5)),
       y_canonical = (x_canonical - tmp_y) / sqrt(phi_canonical * x_canonical^(1.5))) %>% 
  ggplot() +
  geom_jitter(aes(x_log, y_log, color = "log"),
              height = 0.4, alpha = 0.4) +
  geom_jitter(aes(x_canonical, y_canonical, color = "canonical"),
              height = 0.4, alpha = 0.4) +
  geom_hline(yintercept = 3, linetype = "dotted") +
  geom_hline(yintercept = -3, linetype = "dotted") +
  geom_hline(yintercept = 0) +
  labs(x = "Fitted values",
       y = "Normalized residuals",
       color = "") +
  scale_y_continuous(breaks = -5:5) +
  theme(text = element_text(size = text_size),
        legend.position = "top") +
  scale_color_manual(values = colors, breaks = c("log", "canonical"),
                     labels = c("Log response", "Canonical response"))
ggsave("../doc/fig/norm_res_comparison.png")