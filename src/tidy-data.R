### data wrangling
# this code loads the scraped and auxiliary data sets
# and merges them together, calculating relevant variables
# it then saves the data set in the data subdirectory of the repo
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


# load data
stages <- readr::read_csv("aux_data/stages_TDF.csv")
riders <- readr::read_csv("aux_data/tdf-riders.csv")
classif <- readr::read_csv("aux_data/tdf_2020_classifications.csv")

stages_2020 <- readr::read_csv("aux_data/scraped.csv") %>% 
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
readr::write_csv(stages_2020, path = "../data/tour-de-france-data.csv")