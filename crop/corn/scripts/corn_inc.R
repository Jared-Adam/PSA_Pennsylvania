# Jared Adam
# PA PSA damage incidence corn
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

damage_done <- rbind(dmg_2021_clean, dmg_sum) %>% 
  mutate(growth = case_when(growth == "v3" ~ "V3", 
                            growth == "v5" ~ "V5", 
                            .default = as.factor(growth))) %>% 
  mutate(growth = as.factor(growth))
unique(damage_done$treatment)


dam_plot <- damage_done %>% 
  group_by(growth, treatment) %>% 
  summarise(mean = mean(prop_damaged),
            sd = sd(prop_damaged),
            n = n(),
            se = sd/sqrt(n)) %>% 
  print(n = Inf)

# model ####
damage_done

m1_full <- glmer(prop_damaged ~ treatment*growth + (1|year/block/plotid/growth), 
                 data = damage_done, family = binomial, 
                 weights = total_sum)
summary(m1_full)
r2_nakagawa(m1_full)
# Conditional R2: 0.129
# Marginal R2: 0.062
m1_em <- emmeans(m1_full, ~treatment*growth, type = 'response')
pairs(m1_em)
plot_emm <- as.data.frame(m1_em)
pwpm(m1_em)

# plot ####




ggplot(dam_plot, aes(color = treatment))+
  geom_point(aes(x = treatment, y = mean), size = 5,
             position = position_dodge(width = .75))+
  facet_wrap(~growth)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 2)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18))+
  labs(
    title = "Corn: Damage Incidence",
    subtitle = "Years: 2021-2023",
    y = "Mean proportion damaged (damaged / total)",
    x = "Treatment"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        plot.subtitle = element_text(size = 18),
        plot.title = element_text(size = 24),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        strip.text.x = element_text(size = 20), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())

