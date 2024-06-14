# Jared Adam
# PA PSA damage incidence corn
# three years, two growth stages 

# packages #### 
library(tidyverse)
library(lme4)
library(emmeans)
library(performance)
library(jtools)
library(huxtable)
library(rempsyc)
library(multcomp)
library(ggpubr)

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
  mutate(treatment = case_when(plotid %in% c("101",'203','304','401', '503') ~ "1",
                               plotid %in% c('102', '201','303','402','502') ~ "3",
                               plotid %in% c('103','204', '302', '403', '501') ~ "2",
                               plotid %in% c('104', '202', '301', '404', '504') ~ "4")) %>% 
  mutate(growth = as.factor(growth)) %>% 
  mutate(treatment = as.factor(treatment)) 
unique(damage_done$treatment)


dam_plot <- damage_done %>% 
  group_by(growth, treatment) %>% 
  summarise(mean = mean(prop_damaged),
            sd = sd(prop_damaged),
            n = n(),
            se = sd/sqrt(n)) %>% 
  print(n = Inf)

damage_done %>% 
  group_by(growth) %>% 
  summarise(mean = mean(prop_damaged),
            sd = sd(prop_damaged),
            n = n(),
            se = sd/sqrt(n)) %>% 
  print(n = Inf)

# model ####
# correlation test 
dmg_cor <- damage_done %>% 
  ungroup() %>% 
  dplyr::select(prop_damaged, block, plotid, treatment, year, growth)

model.matrix(~0+., data = dmg_cor) %>% 
  cor(use = 'pairwise.complete.obs') %>% 
  ggcorrplot(show.diag = FALSE, type = 'lower', lab = TRUE, lab_size = 2)



damage_done

m0 <- glmer(prop_damaged ~ 
            (1|year/block/plotid/growth), 
            data = damage_done, family = binomial, 
            weights = total_sum)

m1 <- glmer(prop_damaged ~ treatment + 
              (1|year/block/plotid/growth), 
            data = damage_done, family = binomial, 
            weights = total_sum)

m2 <- glmer(prop_damaged ~ treatment + growth + 
              (1|year/block/plotid/growth), 
            data = damage_done, family = binomial, 
            weights = total_sum)

m3 <- glmer(prop_damaged ~ treatment * growth + 
              (1|year/block/plotid/growth), 
                 data = damage_done, family = binomial, 
                 weights = total_sum)

m4 <- glmer(prop_damaged ~ treatment * year + 
              (1|block/plotid/growth), 
            data = damage_done, family = binomial, 
            weights = total_sum)

check_model(m4)
check_model(m3)
binned_residuals(m4)
summary(m4)



isSingular(m3)
anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    5 887.95 901.88 -438.97   877.95                          
# m1    8 887.04 909.34 -435.52   871.04  6.9101  3  0.0748198 .  
# m2    9 877.20 902.29 -429.60   859.20 11.8368  1  0.0005807 ***
# m3   12 878.32 911.77 -427.16   854.32  4.8779  3  0.1809610  


summary(m3)
r2_nakagawa(m3)
# Conditional R2: 0.129
# Marginal R2: 0.062
m3_em <- emmeans(m3, ~growth)
pairs(m3_em)
pwpm(m3_em)
cld(m3_em, Letters = letters)




# stats.table <- as.data.frame(summary(m3)$coefficients)
# #CI <- confint(m3)
# stats.table <-cbind(row.names(stats.table), stats.table)
# names(stats.table) <- c("Term", "B", "SE", "t", "p")
# nice_table(stats.table, highlight = TRUE)







# plot ####

ggplot(dam_plot, aes(color = treatment))+
  geom_point(aes(x = treatment, y = mean), size = 10)+
  facet_wrap(~growth)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 2)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  theme(axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18))+
  labs(
    title = "Corn: Damage Incidence x Treatment",
    subtitle = "Years: 2021-2023",
    y = "Mean proportion damaged (damaged / total)",
    x = "Treatment",
    caption = "DPP: Days pre plant
DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 26, color = "grey25"))

# For PUBs
ggplot(dam_plot)+
  geom_point(aes(x = treatment, y = mean), size = 5,
             position = position_dodge(width = .75))+
  facet_wrap(~growth)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 2)+
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
