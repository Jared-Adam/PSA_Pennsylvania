# Jared Adam
# PA PSA damage incidence corn
# three years, two growth stages 

# packages #### 
library(tidyverse)
library(lme4)
library(MASS)
library(performance)
library(emmeans)
library(ggpubr)
library(rempsyc)
library(multcomp)
library(car)
library(ggResidpanel)
install.packages('ggcorrplot')
library(ggcorrplot)

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
            (1|year/block/plotid), 
            data = damage_done, family = binomial, 
            weights = total_sum)

m1 <- glmer(prop_damaged ~ treatment + 
              (1|year/block/plotid), 
            data = damage_done, family = binomial, 
            weights = total_sum)

m2 <- glmer(prop_damaged ~ treatment + growth + 
              (1|year/block/plotid), 
            data = damage_done, family = binomial, 
            weights = total_sum)

m3 <- glmer(prop_damaged ~ treatment * growth + 
              (1|year/block/plotid), 
                 data = damage_done, family = binomial, 
                 weights = total_sum)

isSingular(m3)
anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    4 1640.9 1652.1 -816.47   1632.9                          
# m1    7 1635.4 1654.9 -810.68   1621.4  11.579  3   0.008973 ** 
# m2    8 1447.9 1470.2 -715.96   1431.9 189.439  1  < 2.2e-16 ***
# m3   11 1424.2 1454.9 -701.11   1402.2  29.711  3  1.587e-06 ***

summary(m3)


r2_nakagawa(m3)
# Conditional R2: 0.364
# Marginal R2: 0.059
cld(emmeans(m3, ~treatment), Letters = letters)
# treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1          0.755 0.243 Inf     0.280      1.23  a    
# 4          0.789 0.242 Inf     0.314      1.26  a    
# 2          0.970 0.244 Inf     0.492      1.45  ab   
# 3          1.417 0.245 Inf     0.937      1.90   b   

cld(emmeans(m3, ~growth), Letters = letters)
# growth emmean    SE  df asymp.LCL asymp.UCL .group
# V5      0.574 0.205 Inf     0.172     0.975  a    
# V3      1.392 0.206 Inf     0.989     1.796   b  

cld(emmeans(m3, ~treatment|growth), Letters = letters)
# growth = V3:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1          0.936 0.353 Inf     0.244      1.63  a    
# 4          1.483 0.358 Inf     0.782      2.18  ab   
# 2          1.620 0.360 Inf     0.914      2.32   b   
# 3          1.707 0.358 Inf     1.004      2.41   b   
# 
# growth = V5:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 4          0.215 0.517 Inf    -0.798      1.23  a    
# 2          0.424 0.521 Inf    -0.597      1.44  a    
# 1          0.619 0.519 Inf    -0.398      1.64  ab   
# 3          1.469 0.523 Inf     0.444      2.49   b 


# stats.table <- as.data.frame(summary(m3)$coefficients)
# #CI <- confint(m3)
# stats.table <-cbind(row.names(stats.table), stats.table)
# names(stats.table) <- c("Term", "B", "SE", "t", "p")
# nice_table(stats.table, highlight = TRUE)



# plot ####

# dmg_growth <- c('V3 a', 'V5 b')
# names(dmg_growth) <- c('V3', 'V5')
# facet_wrap(~growth, labeller = labeller(growth = dmg_growth))+


# pub plot 4.24.25

dmg_prop_fig_df <- cld(emmeans(m3, ~treatment|growth, type = 'response'), Letters = letters)


gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

num_labs <- data.frame(label = c('1)', '2)'),
                       growth = c('V3', 'V5'))

ggplot(dmg_prop_fig_df, aes(x = treatment, y = prob))+
  facet_wrap(~growth, labeller = labeller(growth = gs.labs))+
  geom_point(size = 5)+
  ylim(0,1)+
  geom_errorbar(aes(x = treatment, ymin = prob - SE, ymax = prob + SE, width = .5), data = dmg_prop_fig_df)+
  geom_text(data = dmg_prop_fig_df, aes(y = 1, label = trimws(.group)), size = 9)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(
    title = "Corn: Damage Incidence x Treatment",
    subtitle = "Years: 2021-2023",
    x = 'Treatment termination',
    y = 'Proportion damaged (damaged / total)')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=28),
        axis.text.y = element_text(size = 28),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = num_labs, mapping = aes(x = 0.6, y = 1,label = label), size = 9)
  

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
