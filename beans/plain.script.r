# Jared Adam
# 1/25/2024
# Plain script
# going to work through as much bean stuff as I can
# this script will be a bit unorganized 


# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)

# 2022 data ####
# 2022: did this in a hurry but then I ended up paying for the wifi anywho
# bean scripts 
one <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentintelPrey.ALLtimes.2022.csv")
two <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentPreyandPF.Beans.2022.csv")
three <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/Pitfalls.ALLtimings.2022.csv")

sent_beans <- one

pf <- two

pf_raw <- three


# 2022 and 2023 data ####
slugs = slugs_beans_all %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y'))  %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>%
  rename(season = seaon) %>% 
  group_by(season, year, month, plot, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)

