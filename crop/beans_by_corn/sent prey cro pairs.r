# Jared Adam 
# sentinel prey script for checking pairs of corn and soybeans

# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)
library(lmtest)
library(nlme)
library(multcomp)

# data ####
beans_sent <- sent_prey_beans_all
sent <- PSA_PA_Sent_prey

# wrangling ####
# corn 

# extract year from the date
colnames(sent)
sent_years <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  relocate(year, growth_stage, plot_id, block, treatment, row, sample, n.absent, n.partial, n.predated, 
           d.absent, d.partial, d.predated, to.predated, n.weather, d.weather)%>%
  mutate(n.predated = as.double(n.predated),
         d.predated = as.double(d.predated),
         to.predated = as.double(to.predated)) %>% 
  mutate(crop = 'corn', 
         crop= as.factor(crop)) %>% 
  print(n = Inf)

# subset by year and then growth stage 
cent_21 <- subset(sent_years, year == '2021')
cent_22 <- subset(sent_years, year == '2022')
cent_23 <- subset(sent_years, year == '2023')

pred_tot <- sent_years %>% 
  dplyr::select(-n.absent, -n.partial, -d.absent, -d.partial, -d.predated)


sent_prop <- sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  group_by(growth_stage, treatment) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  mutate_at(vars(1:2), factor) %>% 
  print(n= Inf)



####
###
##
#

# beans 

colnames(beans_sent)
sent_years <- beans_sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date, -pm.predators, -am.predators) %>% 
  rename(plot_id = ploitid) %>% 
  rename(n.absent = pm.absent,
         n.partial = pm.partial,
         d.absent = am.absent, 
         d.partial = am.partial, 
         d.predated = d.pred, 
         n.weather = pm.weather, 
         d.weather = am.weather,
         n.predated = n.pred) %>% 
  relocate(year, growth_stage, plot_id, block, treatment, row, sample, n.absent, n.partial, n.predated, 
           d.absent, d.partial, d.predated, to.predated, n.weather, d.weather)%>%
  mutate(n.predated = as.double(n.predated),
         d.predated = as.double(d.predated),
         to.predated = as.double(to.predated)) %>% 
  mutate(growth_stage = as.factor(growth_stage)) %>% 
  mutate(crop = 'beans',
         crop = as.factor(crop)) %>% 
  print(n = Inf)

# subset by year and then growth stage 
bent_22 <- subset(sent_years, year == '2022')
bent_23 <- subset(sent_years, year == '2023')
# 
# pred_tot <- sent_years %>% 
#   dplyr::select(-pm.absent, -pm.partial, -am.absent, -am.partial, -d.pred, -n.pred)


sent_prop <- beans_sent %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  dplyr::select(-location, -date) %>% 
  rename(plot_id = ploitid) %>% 
  group_by(growth_stage, treatment) %>% 
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n)) %>% 
  print(n= Inf)




# pairings ####
# corn 2021 - soybean 2022
colnames(cent_21)
colnames(bent_22)
cbent2122 <- rbind(cent_21, bent_22) %>% 
  relocate(year, growth_stage, crop) %>% 
  mutate_at(vars(1:6), as.factor)

cbent2122 %>% 
  group_by(crop, treatment) %>% 
  summarise(mean = mean(to.predated),
            sd = sd(to.predated), 
            n = n(), 
            se = sd/sqrt(n))
# crop  treatment  mean    sd     n     se
#   1 corn  1         0.656 0.478    90 0.0504
# 2 corn  2         0.711 0.456    90 0.0480
# 3 corn  3         0.844 0.364    90 0.0384
# 4 corn  4         0.911 0.286    90 0.0302
# 5 beans 1         0.889 0.316    90 0.0333
# 6 beans 2         0.967 0.181    90 0.0190
# 7 beans 3         0.967 0.181    90 0.0190
# 8 beans 4         0.933 0.251    90 0.0264


# corn 2022 - soybean 2023
cbent2223 <- rbind(cent_22, bent_23)%>% 
  relocate(year, growth_stage, crop) %>% 
  mutate_at(vars(1:6), as.factor)

cbent2223 %>% 
  group_by(crop, treatment) %>% 
  summarise(mean = mean(to.predated),
            sd = sd(to.predated), 
            n = n(), 
            se = sd/sqrt(n))
# crop  treatment  mean    sd     n     se
#   1 corn  1         0.9   0.302    90 0.0318
# 2 corn  2         0.944 0.230    90 0.0243
# 3 corn  3         0.878 0.329    90 0.0347
# 4 corn  4         0.933 0.251    90 0.0264
# 5 beans 1         0.733 0.445    90 0.0469
# 6 beans 2         0.878 0.329    90 0.0347
# 7 beans 3         0.944 0.230    90 0.0243
# 8 beans 4         0.833 0.375    90 0.0395

# models ####
cbent2122

m0 <- glmer(to.predated ~ +
              (crop|block/plot_id),
            family = binomial,
            data = cbent2122)

m1 <- glmer(to.predated ~ treatment+
              (crop|block/plot_id),
            family = binomial,
            data = cbent2122)

m2 <- glmer(to.predated ~ treatment + crop+
              (crop|block/plot_id),
            family = binomial,
            data = cbent2122)

m3 <- glmer(to.predated ~ treatment*crop+
              (crop|block/plot_id),
            family = binomial,
            data = cbent2122)
anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)   
# m0    7 553.76 585.82 -269.88   539.76                         
# m1   10 548.72 594.51 -264.36   528.72 11.0437  3   0.011492 * 
# m2   11 542.29 592.66 -260.14   520.29  8.4313  1   0.003688 **
# m3   14 541.40 605.50 -256.70   513.40  6.8946  3   0.075333 . 

hist(residuals(m3))
summary(m3)
check_model(m3)

cld(emmeans(m3, ~treatment), Letters = letters, max.overlaps = Inf)
# treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1           1.41 0.254 Inf     0.908      1.90  a    
# 2           2.19 0.353 Inf     1.495      2.88  ab   
# 4           2.56 0.329 Inf     1.920      3.21   b   
# 3           2.59 0.364 Inf     1.881      3.31   b  

cld(emmeans(m3, ~crop), Letters = letters, max.overlaps = Inf)
# crop  emmean    SE  df asymp.LCL asymp.UCL .group
# corn    1.43 0.176 Inf      1.09      1.78  a    
# beans   2.94 0.335 Inf      2.29      3.60   b

cld(emmeans(m3, ~crop*treatment), Letters = letters, max.overlaps = Inf)
# crop  treatment emmean    SE  df asymp.LCL asymp.UCL .group
# corn  1          0.669 0.288 Inf     0.105      1.23  a    
# corn  2          0.924 0.294 Inf     0.347      1.50  ab   
# corn  3          1.741 0.346 Inf     1.063      2.42  abc  
# beans 1          2.144 0.402 Inf     1.357      2.93   bc  
# corn  4          2.405 0.424 Inf     1.574      3.24   bc  
# beans 4          2.725 0.487 Inf     1.771      3.68    c  
# beans 3          3.449 0.630 Inf     2.215      4.68    c  
# beans 2          3.450 0.630 Inf     2.215      4.69    c  



# corn 22 - beans 23
cbent2223

m10 <- glmer(to.predated ~ +
              (crop|block/plot_id),
            family = binomial,
            data = cbent2223)

m11 <- glmer(to.predated ~ treatment+
              (crop|block/plot_id),
            family = binomial,
            data = cbent2223)

m12 <- glmer(to.predated ~ treatment + crop+
              (crop|block/plot_id),
            family = binomial,
            data = cbent2223)

m13 <- glmer(to.predated ~ treatment*crop+
              (crop|block/plot_id),
            family = binomial,
            data = cbent2223)
anova(m10, m11, m12, m13)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# m10    7 529.30 561.35 -257.65   515.30                       
# m11   10 530.05 575.85 -255.03   510.05 5.2456  3    0.15467  
# m12   11 528.72 579.09 -253.36   506.72 3.3351  1    0.06782 .
# m13   14 526.26 590.37 -249.13   498.26 8.4538  3    0.03751 *

hist(residuals(m13))
summary(m13)
check_model(m13)

cld(emmeans(m13, ~treatment|crop), Letters = letters, max.overlaps = Inf)
# crop = corn:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3           1.97 0.325 Inf     1.337      2.61  a    
# 1           2.20 0.354 Inf     1.506      2.89  a    
# 4           2.64 0.425 Inf     1.808      3.47  a    
# 2           2.84 0.462 Inf     1.929      3.74  a    
# 
# crop = beans:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1           1.03 0.272 Inf     0.497      1.56  a    
# 4           1.63 0.312 Inf     1.022      2.25  ab   
# 2           2.00 0.348 Inf     1.317      2.68  ab   
# 3           2.87 0.479 Inf     1.927      3.81   b  


cld(emmeans(m13, ~treatment*crop), Letters = letters, max.overlaps = Inf)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         beans   1.03 0.272 Inf     0.497      1.56  a    
# 4         beans   1.63 0.312 Inf     1.022      2.25  ab   
# 3         corn    1.97 0.325 Inf     1.337      2.61  ab   
# 2         beans   2.00 0.348 Inf     1.317      2.68  ab   
# 1         corn    2.20 0.354 Inf     1.506      2.89  ab   
# 4         corn    2.64 0.425 Inf     1.808      3.47   b   
# 2         corn    2.84 0.462 Inf     1.929      3.74   b   
# 3         beans   2.87 0.479 Inf     1.927      3.81   b   

# plots ####
# 21 -23
cld <- c('ab', 'a', 'ab', 'b',  'ab', 'b','b', 'ab')
cbent2122_plot <- cbent2122 %>%
  mutate(crop = case_when(crop == 'corn' ~ 'Corn',
                          crop == 'beans' ~ 'Soybean')) %>%
  group_by(treatment, crop) %>%
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n))

cbent2122_plot$cld <- cld

ggplot(cbent2122_plot, aes(treatment, prop, color = treatment))+
  geom_point(size = 10)+
  facet_wrap(~crop, strip.position = 'right', ncol = 1)+
  coord_flip()+
  geom_errorbar(aes(x = treatment,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(labels=c("Green", "Late", "Early", "No CC"),
                   limits = c("3", "4", "2", "1"))+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Mean predation x Treatment and Crop",
    subtitle = "Years: 2021 Corn - 2022 Soybean",
    x = "Treatment termination",
    y = "Mean proportion attacked ( x / 1 )"
#     caption = "DPP: Days pre plant
# DAP: Days after plant"
    )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 32),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        strip.text = element_text(size = 32))+
  geom_text(aes(x = treatment, y = 1,label = trimws(cld)), size = 10 , color = 'black')


####
###
##
#

# 22 - 23
cld <- c('b', 'a', 'b', 'ab', 'ab', 'b', 'b', 'ab')
cbent2223_plot <- cbent2223 %>%
  mutate(crop = case_when(crop == 'corn' ~ 'Corn',
                          crop == 'beans' ~ 'Soybean')) %>%
  group_by(treatment, crop) %>%
  summarise(prop = mean(to.predated),
            sd = sd(to.predated),
            n = n(),
            se = sd/sqrt(n))

cbent2223_plot$cld <- cld

ggplot(cbent2223_plot, aes(treatment, prop, color = treatment))+
  geom_point(size = 10)+
  facet_wrap(~crop, strip.position = 'right', ncol = 1)+
  coord_flip()+
  geom_errorbar(aes(x = treatment,ymin = prop - se, ymax = prop + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(labels=c("Green", "Late", "Early", "No CC"),
                   limits = c("3", "4", "2", "1"))+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Mean predation x Treatment and Crop",
    subtitle = "Years: 2022 Corn - 2023 Soybean",
    x = "Treatment",
    y = "Mean proportion attacked ( x / 1 )"
    #     caption = "DPP: Days pre plant
    # DAP: Days after plant"
  )+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 26),
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        strip.text = element_text(size = 32))+
  geom_text(aes(x = treatment, y = 1,label = trimws(cld)), size = 10 , color = 'black')
