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
              (1|block/plot_id),
            family = binomial,
            data = cbent2122)

m1 <- glmer(to.predated ~ treatment+
              (1|block/plot_id),
            family = binomial,
            data = cbent2122)

m2 <- glmer(to.predated ~ treatment + crop+
              (1|block/plot_id),
            family = binomial,
            data = cbent2122)

m3 <- glmer(to.predated ~ treatment*crop+
              (1|block/plot_id),
            family = binomial,
            data = cbent2122)
anova(m0, m1, m2, m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    3 580.87 594.61 -287.43   574.87                          
# m1    6 574.49 601.96 -281.24   562.49 12.3795  3    0.00619 ** 
# m2    7 535.51 567.57 -260.76   521.51 40.9773  1   1.54e-10 ***
# m3   10 534.38 580.17 -257.19   514.38  7.1326  3    0.06779 .  


hist(residuals(m3))
summary(m3)
check_model(m3)

cld(emmeans(m3, ~treatment), Letters = letters, max.overlaps = Inf)
# treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1           1.39 0.246 Inf     0.905      1.87  a    
# 2           2.16 0.345 Inf     1.482      2.84  ab   
# 4           2.53 0.322 Inf     1.903      3.16   b   
# 3           2.56 0.358 Inf     1.863      3.27   b   

cld(emmeans(m3, ~crop), Letters = letters, max.overlaps = Inf)
# crop  emmean    SE  df asymp.LCL asymp.UCL .group
# corn    1.42 0.162 Inf      1.10      1.74  a    
# beans   2.90 0.261 Inf      2.39      3.42   b 

cld(emmeans(m3, ~crop*treatment), Letters = letters, max.overlaps = Inf)
# crop  treatment emmean    SE  df asymp.LCL asymp.UCL .group
# corn  1          0.658 0.263 Inf     0.142      1.17  a    
# corn  2          0.917 0.272 Inf     0.384      1.45  ab   
# corn  3          1.722 0.325 Inf     1.085      2.36  abc  
# beans 1          2.116 0.366 Inf     1.399      2.83   bc  
# corn  4          2.376 0.402 Inf     1.588      3.16   bc  
# beans 4          2.691 0.451 Inf     1.806      3.58    c  
# beans 2          3.402 0.604 Inf     2.219      4.59    c  
# beans 3          3.408 0.604 Inf     2.223      4.59    c  



# corn 22 - beans 23
cbent2223

m10 <- glmer(to.predated ~ +
              (1|block/plot_id),
            family = binomial,
            data = cbent2223)

m11 <- glmer(to.predated ~ treatment+
              (1|block/plot_id),
            family = binomial,
            data = cbent2223)

m12 <- glmer(to.predated ~ treatment + crop+
              (1|block/plot_id),
            family = binomial,
            data = cbent2223)

m13 <- glmer(to.predated ~ treatment*crop+
              (1|block/plot_id),
            family = binomial,
            data = cbent2223)
anova(m10, m11, m12, m13)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)   
# m10    3 530.51 544.25 -262.26   524.51                         
# m11    6 529.13 556.60 -258.56   517.13  7.3860  3   0.060561 . 
# m12    7 523.30 555.36 -254.65   509.30  7.8211  1   0.005164 **
# m13   10 518.93 564.72 -249.47   498.93 10.3727  3   0.015650 * 

hist(residuals(m13))
summary(m13)
check_model(m13)

cld(emmeans(m13, ~treatment|crop), Letters = letters, max.overlaps = Inf)
# crop = corn:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3           1.98 0.331 Inf     1.332      2.63  a    
# 1           2.21 0.360 Inf     1.501      2.91  a    
# 4           2.65 0.430 Inf     1.806      3.49  a    
# 2           2.84 0.467 Inf     1.928      3.76  a    
# 
# crop = beans:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1           1.02 0.249 Inf     0.529      1.51  a    
# 4           1.62 0.293 Inf     1.043      2.19  ab   
# 2           1.98 0.331 Inf     1.332      2.63  ab   
# 3           2.84 0.467 Inf     1.928      3.76   b  


cld(emmeans(m13, ~treatment*crop), Letters = letters, max.overlaps = Inf)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         beans   1.02 0.249 Inf     0.529      1.51  a    
# 4         beans   1.62 0.293 Inf     1.043      2.19  ab   
# 3         corn    1.98 0.331 Inf     1.332      2.63  ab   
# 2         beans   1.98 0.331 Inf     1.332      2.63  ab   
# 1         corn    2.21 0.360 Inf     1.501      2.91  ab   
# 4         corn    2.65 0.430 Inf     1.806      3.49   b   
# 3         beans   2.84 0.467 Inf     1.928      3.76   b   
# 2         corn    2.84 0.467 Inf     1.928      3.76   b  

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
