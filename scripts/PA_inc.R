# Jared Adam
# PA PSA damage incidence
# three years, two growth stages 

# packages #### 
library(tidyverse)
library(lme4)
library(emmeans)
library(performance)

# data #####
damage_inc <- PSA_PA_Inc
unique(damage_inc$treatment)
damage_inc <- damage_inc %>% 
  mutate(treatment = case_when(plotid %in% c(101,203,304,401,503) ~ 1,
                               plotid %in% c(103,204,302,403,501) ~ 2,
                               plotid %in% c(102,201,303,402,502) ~ 3, 
                               plotid %in% c(104,202,301,404,504) ~ 4))
unique(damage_inc$treatment)

# wrangling ####
damage_inc 

# 2021 total and damaged are in the wrong columns
# subset, rename. rbind 


dmg_clean <- damage_inc %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y"),
         year = as.factor(year))%>% 
  dplyr::select(-date) %>% 
  mutate(growth = as.factor(growth),
         plotid = as.factor(plotid),
         treatment = as.factor(treatment), 
         block = as.factor(block)) %>% 
  dplyr::select(-location, -tempC) %>% 
  relocate(year)

dmg_sum <- dmg_clean %>%
  filter(year != 2021) %>% 
  group_by(year, growth, treatment, block, plotid) %>% 
  summarise(total_sum = sum(total),
            damaged_sum = sum(damaged),
            prop_damaged = damaged_sum/total_sum) %>% 
  print(n = Inf)

dmg_2021_clean <- dmg_clean %>% 
  filter(year == 2021) %>% 
  rename(damaged = total,
         total = damaged) %>% 
  relocate(year, growth, plotid, block, treatment, transect, damaged, total) %>% 
  group_by(year, growth, treatment, block, plotid) %>% 
  summarise(total_sum = sum(total),
            damaged_sum = sum(damaged),
            prop_damaged = damaged_sum/total_sum) %>%
  print(n = Inf)

damage_done <- rbind(dmg_2021_clean, dmg_sum)

# model ####
damage_done

m1_full <- glmer(prop_damaged ~ treatment + (1|year/growth/block/plotid), 
                 data = damage_done, family = binomial, 
                 weights = total_sum)
summary(m1_full)
r2_nakagawa(m1_full)
m1_em <- emmeans(m1_full, pairwise~treatment, type = 'response')
plot_emm <- as.data.frame(m1_em$emmeans)

# nope
m2_grow <- glmer(prop_damaged ~ treatment*growth + (1|year/block/plotid), 
                 data = damage_done, family = binomial, 
                 weights = total_sum)




# plot ####

ggplot(plot_emm, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, linewidth = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = asymp.LCL, ymax = asymp.UCL), 
                alpha = .6, width = 0, linewidth = 1)+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))
