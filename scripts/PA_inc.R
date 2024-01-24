# Jared Adam
# PA PSA damage incidence
# three years, two growth stages 

# packages #### 
library(tidyverse)
library(lme4)
library(emmeans)

# data #####
damage_inc <- PSA_PA_Inc
unique(damage_inc$treatment)
damage_inc <- damage_inc %>% 
  mutate(treatment = case_when(plotid %in% c(101,203,304,401,503) ~ 1,
                               plotid %in% c(103,204,302,403,501) ~ 2,
                               plotid %in% c(102,201,303,402,502) ~ 3, 
                               plotid %in% c(104,202,301,404,504) ~ 4))
unique(damage_inc$treatment)