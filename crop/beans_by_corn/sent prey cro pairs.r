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

pred_tot <- sent_years %>% 
  dplyr::select(-pm.absent, -pm.partial, -am.absent, -am.partial, -d.pred, -n.pred)


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
              (1|gblock/plot_id/crop),
            family = binomial,
            data = cbent2122)

m1 <- glmer(to.predated ~ treatment+
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2122)

m2 <- glmer(to.predated ~ treatment + crop+
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2122)

m3 <- glmer(to.predated ~ treatment*crop+
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2122)
anova(m0, m1, m2, m3)


hist(residuals(m3))
summary(m3)
check_model(m3)

cbent2122_emm <- cld(emmeans(m3, ~treatment + crop), Letters = letters, max.overlaps = Inf)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         corn   0.663 0.276 Inf     0.123      1.20  a    
# 2         corn   0.921 0.283 Inf     0.366      1.48  ab   
# 3         corn   1.732 0.336 Inf     1.074      2.39  abc  
# 1         beans  2.123 0.375 Inf     1.388      2.86   bc  
# 4         corn   2.390 0.413 Inf     1.581      3.20   bc  
# 4         beans  2.699 0.458 Inf     1.801      3.60    c  
# 2         beans  3.421 0.612 Inf     2.222      4.62    c  
# 3         beans  3.422 0.611 Inf     2.224      4.62    c  

# corn 22 - beans 23
cbent2223

m10 <- glmer(to.predated ~ +
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2223)

m11 <- glmer(to.predated ~ treatment+
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2223)

m12 <- glmer(to.predated ~ treatment + crop+
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2223)

m13 <- glmer(to.predated ~ treatment*crop+
              (1|block/plot_id/crop),
            family = binomial,
            data = cbent2223)
anova(m10, m11, m12, m13)


hist(residuals(m13))
summary(m13)
check_model(m13)

cbent2223_emm <- cld(emmeans(m13, ~treatment + crop), Letters = letters, max.overlaps = Inf)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         beans   1.03 0.264 Inf     0.508      1.54  a    
# 4         beans   1.63 0.306 Inf     1.029      2.23  ab   
# 2         beans   1.99 0.342 Inf     1.321      2.66  ab   
# 3         corn    1.99 0.342 Inf     1.321      2.66  ab   
# 1         corn    2.22 0.372 Inf     1.492      2.95  ab   
# 4         corn    2.66 0.439 Inf     1.802      3.52   b   
# 3         beans   2.86 0.476 Inf     1.925      3.79   b   
# 2         corn    2.86 0.476 Inf     1.925      3.79   b  

# plots ####
# 21 -23
# emmeans

ggplot(cbent2122_emm, aes(treatment, emmean, color = treatment))+
  geom_point(size = 10)+
  facet_wrap(~crop, strip.position = 'right', ncol = 1)+
  coord_flip()+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(labels=c("1-3 DAP", "3-7 DPP", "14-28 DPP", "No CC"),
                   limits = c("3", "4", "2", "1"))+ 
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  labs(
    title = "Mean predation x Treatment and Crop",
    subtitle = "Years: 2021 Corn - 2022 Soybean",
    x = "Treatment",
    y = "Emmeans of predation",
    caption = "DPP: Days pre plant
DAP: Days after plant")+
  theme(legend.position = 'none',
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"),
        strip.text = element_text(size = 26))+
  geom_text(aes(x = treatment, y = 4.5,label = trimws(.group)), size = 10 , color = 'black')

# proportions 
# this one is better
cld <- c('a', 'bc', 'ab', 'c', 'abc', 'c', 'bc', 'c')
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
