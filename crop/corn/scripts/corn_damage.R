# Jared Adam 
# PA PSA Damage type assessment of corn
# 1/23/2024
# two collection timings: v3 and v5
# binomial data? 



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
# data ####
damage_type <- PSA_PA_damage



damage_type

unique(damage_type$treatment)
(NA %in% damage_type$treatment)
# all of 2023 does not have a treatment number.. whoops

damage_type <- damage_type %>% 
  mutate(treatment = case_when(plot_id %in% c(101,203,304,401,503) ~ 1,
                               plot_id %in% c(103,204,302,403,501) ~ 2,
                               plot_id %in% c(102,201,303,402,502) ~ 3, 
                               plot_id %in% c(104,202,301,404,504) ~ 4))

unique(damage_type$treatment)
(NA %in% damage_type$treatment)

unique(damage_type$damage_type)
# we need to create a new column for each damage type. 
# this will be accomplished by splitting the damage into new columns 

# test ####
test <- damage_type[1:300,]
unique(test$damage_type)


# this one works: will need to adapt column names a bit with the whole df 
df <- spread(test, damage_type, damage_type)
look <- df %>% 
  dplyr::select(-na) %>%
  unite(multiple, c('s, bcw' , 's, taw'), sep = " ", remove = TRUE, na.rm = TRUE) %>% 
  mutate(multiple = case_when(multiple != "" ~ 1)) %>% 
  #dplyr::select(-'s, bcw', -'s, taw') %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         #multiple = case_when(multiple %in% c('s, bcw' , 's, taw' , 's, bcw s, taw') ~ 1),
         taw = case_when(taw == 'taw' ~ 1)) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0))
sum(look$multiple)

# wrangling ####
# try on full data set 
dmg <- damage_type

dmg <- spread(dmg, damage_type, damage_type)
colnames(dmg[9:28])
new_dmg <- dmg %>% 
  dplyr::select(-location, -tempC, -na) %>% 
  unite(multiple, c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                     "s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb"), sep = "-", remove = TRUE, na.rm = TRUE) %>% 
  mutate(multiple = case_when(multiple != "" ~ 1)) %>% 
  mutate(bcw = case_when(bcw == 'bcw' ~ 1),
         s = case_when(s =='s' ~ 1),
         taw = case_when(taw == 'taw' ~ 1),
         sb = case_when(sb == 'sb' ~ 1),
         d = case_when(d == 'd' ~ 1),
         t = case_when(t == 't' ~ 1),
         other = case_when(other == 'other' ~ 1),
         #multiple = case_when(multiple %in% c( "bcw, sb","bcw, taw","multiple","s, bcw","s, bcw, sb","s, sb",
                                               #"s, taw","s, taw, bcw","s, taw, sb","s,sb","taw, bcw","taw, sb") ~ 1),
         ) %>% 
  mutate_if(is.double, ~replace(., is.na(.), 0)) %>% 
  dplyr::select(-d, -t) %>% # I think these two are typos and I removed them 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, "%Y")) %>%
  dplyr::select(-date) %>% 
  relocate(year) %>% 
  mutate(treatment = case_when(plot_id %in% c("101",'203','304','401', '503') ~ "1",
                               plot_id %in% c('102', '201','303','402','502') ~ "3",
                               plot_id %in% c('103','204', '302', '403', '501') ~ "2",
                               plot_id %in% c('104', '202', '301', '404', '504') ~ "4")) %>% 
  mutate(growth_stage = as.factor(growth_stage),
         block = as.factor(block),
         treatment = as.factor(treatment),
         year = as.factor(year)) %>% 
  mutate(damage_score = case_when(damage_score == "na" ~ "0",
                                  is.na(damage_score) ~ "0",
                                  .default = as.character(damage_score))) %>% 
  replace(is.na(.),0) %>% 
  mutate(damage_score = as.numeric(damage_score)) %>% 
  print(n = 10)
#check before removing 
# unique(dmg_new$d), sum(dmg_new$d)
sum(new_dmg$multiple)
sum(new_dmg$s)
sum(new_dmg$other)
sum(new_dmg$sb)
sum(new_dmg$taw)
sum(new_dmg$bcw)
# these all have numbers 

# damage severity models ####
new_dmg
unique(new_dmg$damage_score)
dmg_sev <- new_dmg %>% 
  mutate(plot_id = as.factor(plot_id))

avg_dmg <- dmg_sev %>%
  dplyr::select(treatment, year, growth_stage, block, plot_id, damage_score) 

# test here 
avg_dmg <- avg_dmg %>% 
  mutate(dmg_prop = damage_score/4,
         total_damage_score = 4)


btest <- glmer(dmg_prop ~ treatment*growth_stage +
                 (1|year/block/plot_id), 
               family = binomial,
               weights = total_damage_score,
               data = avg_dmg)
summary(btest)
Anova(btest)
cld(emmeans(btest, ~treatment|growth_stage, type = 'response'), Letters = letters)
# growth_stage = V3:
#   treatment emmean     SE  df asymp.LCL asymp.UCL .group
# 1          -1.51 0.0867 Inf     -1.68    -1.337  a    
# 4          -1.42 0.0859 Inf     -1.59    -1.249  ab   
# 2          -1.37 0.0859 Inf     -1.54    -1.205  ab   
# 3          -1.25 0.0846 Inf     -1.42    -1.084   b   
# 
# growth_stage = V5:
#   treatment emmean     SE  df asymp.LCL asymp.UCL .group
# 2          -1.68 0.0887 Inf     -1.86    -1.507  a    
# 4          -1.65 0.0879 Inf     -1.82    -1.475  a    
# 1          -1.56 0.0870 Inf     -1.73    -1.391  a    
# 3          -1.12 0.0844 Inf     -1.29    -0.956   b   

cld(emmeans(btest, ~growth_stage), Letters = letters)

# 4.9.2025 plot
dmg_prob_plot <- cld(emmeans(btest, ~treatment|growth_stage, type = 'response'), Letters = letters)

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

num_labs <- data.frame(label = c('1)', '2)'),
                       growth_stage = c('V3', 'V5'))

dmg_prob_plot %>% 
  ggplot(aes(x = treatment, y = prob))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = treatment, ymin = prob - SE, ymax = prob + SE, width = .5), data = dmg_prob_plot)+
  geom_text(data = dmg_prob_plot, aes(y = 0.27, label = trimws(.group)), size = 8)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(title = 'Corn: Response Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Response damage score (x/4)')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = num_labs, mapping = aes(x = 0.6, y = 0.28,label = label), size = 8)





# new plot (11/6/2024)

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

avg_dmg %>% 
  group_by(treatment, growth_stage, plot_id) %>% 
  summarise(mean = mean(dmg_prop)) %>% 
  mutate(letters = case_when(growth_stage == 'V3' & treatment == '1' ~ 'a',
                             growth_stage == 'V3' & treatment == '2' ~ 'ab',
                             growth_stage == 'V3' & treatment == '3' ~ 'b',
                             growth_stage == 'V3' & treatment == '4' ~ 'ab',
                             growth_stage == 'V5' & treatment == '1' ~ 'a',
                             growth_stage == 'V5' & treatment == '2' ~ 'a',
                             growth_stage == 'V5' & treatment == '3' ~ 'b',
                             growth_stage == 'V5' & treatment == '4' ~ 'a')
         ) %>% 
  ggplot(aes(x = treatment, y = mean, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(title = 'Corn: Average Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Average damage score (x/4)')+
  theme(legend.position = "none",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24))+
  guides(fill = guide_legend(title = 'Growth Stage'))+
  geom_text(aes(x = treatment, y = 0.4, label = trimws(letters)), size = 10, color = "black")

  













# sm0 <- glmer(damage_score ~ 
#                (1|year/block/plot_id), 
#              data = avg_dmg, 
#              family = poisson)
# 
# sm1 <- glmer(damage_score ~ treatment +
#                   (1|year/block/plot_id), 
#                 data = avg_dmg, 
#              family = poisson)
# 
# sm2 <- glmer(damage_score ~ treatment + growth_stage +
#                   (1|year/block/plot_id), 
#                 data = avg_dmg, 
#              family = poisson)
# 
# sm3 <- glmer(damage_score ~ treatment*growth_stage +
#                   (1|year/block/plot_id), 
#                 data = avg_dmg, 
#                 family = poisson)
# 
# rePCA(sm3)
# isSingular(sm3)
# anova(sm0, sm1, sm2, sm3)
# npar   AIC   BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# sm0    7 13953 14001 -6969.4    13939                          
# sm1   10 13884 13952 -6931.8    13864 75.3390  3  3.065e-16 ***
# sm2   11 13884 13959 -6931.2    13862  1.1879  1     0.2757    
# sm3   14 13864 13960 -6918.2    13836 25.9585  3  9.730e-06 ***
# hist(residuals(sm3))
# summary(sm3)
# binned_residuals(sm3)
# check_model(sm3)
# r2_nakagawa(sm3)
# Conditional R2: 0.100
# Marginal R2: 0.029

# cld(emmeans(sm3, ~treatment|growth_stage), Letters = letters)
# 

# avg df plot 

avg_dam_p <- dmg_sev %>% 
  group_by(treatment, year, growth_stage) %>% 
  summarise(mean = mean(damage_score),
            sd = sd(damage_score), 
            n = n(), 
            se = sd/sqrt(n))

# trt_ord <- c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP")
# avg_for_paper <- dmg_sev %>% 
#   mutate(treatment = case_when(treatment == '1' ~ 'No CC',
#                                treatment == '2' ~ '14-28 DPP',
#                                treatment == '3' ~ '1-3 DAP',
#                                treatment == '4' ~ '3-7 DPP')) %>% 
#   mutate(treatment = factor(treatment, levels = trt_ord)) %>% 
#   group_by(treatment, growth_stage) %>% 
#   summarise(mean = mean(damage_score),
#             sd = sd(damage_score), 
#             n = n(), 
#             se = sd/ sqrt(n))
# names(avg_for_paper) <- c("Treatment", "Growth Stage", "Mean","Sd", "n", "SE")
# avg_dmg_table <- flextable(avg_for_paper)
# avg_dmg_table <- autofit(avg_dmg_table)
# theme_zebra(avg_dmg_table) %>% 
#   save_as_docx(path = 'average.dmg.trt.gs.docx')
# 
# 
# 
# ggplot(avg_dam_p, aes(x = treatment, y = mean))+
#   geom_bar(stat= 'identity', position = 'dodge')+
#   facet_wrap(~year)+
#   geom_errorbar(aes(ymin = mean-se, ymax = mean+se))
# 
# # sum df plot
# sum_dmg <- dmg_sev %>% 
#   group_by(treatment, year, growth_stage, plot_id) %>% 
#   summarise(sum = sum(damage_score))
# 
# a1 <- aov(sum ~ year , data = sum_dmg)
# TukeyHSD(a1)  
# 
# # dmg severity plot ####
# #   treatment  emmean     SE  df asymp.LCL asymp.UCL .group
# # 1         -0.3168 0.0787 Inf    -0.471  -0.16253  a    
# # 4         -0.2246 0.0779 Inf    -0.377  -0.07192  ab   
# # 2         -0.1464 0.0771 Inf    -0.298   0.00467   b   
# # 3         -0.1011 0.0767 Inf    -0.251   0.04930   b   
# # 
# # growth_stage = V5:
# #   treatment  emmean     SE  df asymp.LCL asymp.UCL .group
# # 4         -0.4917 0.0806 Inf    -0.650  -0.33370  a    
# # 2         -0.4710 0.0807 Inf    -0.629  -0.31294  a    
# # 1         -0.4444 0.0798 Inf    -0.601  -0.28800  a    
# # 3         -0.0779 0.0770 Inf    -0.229   0.07295   b   

# facet gs
cld_av_dmg <- avg_dam_p %>% 
  mutate(group = case_when(
    treatment %in% c('2','3') & growth_stage %in% c('V3') ~ 'b',
    treatment == '1' & growth_stage == 'V3' ~ 'a',
    treatment == '4' & growth_stage == 'V3' ~ 'ab',
    treatment %in% c('4','2','1') & growth_stage == 'V5' ~'a',
    treatment == '3' & growth_stage == 'V5' ~ 'b'
  ))

ggplot(cld_av_dmg, aes(x = treatment, y = mean, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  facet_wrap(~growth_stage)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(title = 'Corn: Average Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Average damage')+
  theme(legend.position = "none",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24))+
  guides(fill = guide_legend(title = 'Growth Stage'))+
  geom_text(aes(label = group, y = 1.15), size = 10)


# by growth stage
ggplot(avg_dam_p, aes(x = treatment, y = mean, fill = growth_stage))+
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#1B9E77"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(title = 'Corn: Average Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Average damage score (x / n)')+
  theme(legend.position = "bottom",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24))+
  guides(fill = guide_legend(title = 'Growth Stage'))

ggplot(avg_dam_p, aes(x = treatment, y = mean, fill = treatment))+
  facet_wrap(~growth_stage)+
  geom_boxplot(alpha = 0.5, fill = "black")+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs(title = 'Corn: Average Damage Score x Treatment and Growth Stage',
       subtitle = "Years: 2021-2023",
       x = 'Treatment',
       y = 'Average Damage Score x Treatment (x / n)',
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 24),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))



# sum_dmg
# 
# year.labs <- c("2021  a", "2022  b", "2023  c")
# names(year.labs) <- c("2021", "2022", "2023")
# 
# ggplot(sum_dmg, aes(x = treatment, y = sum, fill = treatment))+
#   geom_boxplot(alpha = 0.7)+
#   facet_wrap(~year, labeller = labeller(year = year.labs))+
#   geom_point(size = 2)+
#   scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "Early", "Late", "Green"))+
#   labs(title = 'Corn: Total Damage Score x Treatment and Year',
#        x = 'Termination termination',
#        y = 'Total damage score per plot (0-4)')+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=26),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 28),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.text = element_text(size = 32))

# 
# 
# ggplot(sum_dmg, aes(x = treatment, y = sum, fill = treatment))+
#   geom_violin(alpha = 0.5, fill = 'black')+
#   geom_boxplot(width = 0.1, fill = 'white')+
#   facet_wrap(~year, labeller = labeller(year = year.labs))+
#   geom_point()+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
#   labs(title = 'Corn: Total Damage Score x Treatment and Year',
#        x = 'Treatment',
#        y = 'Total Damage Score x Plot (0-4)',
#        caption = "DPP: Days pre plant
# DAP : Days after plant")+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=20),
#         axis.text.y = element_text(size = 26),
#         axis.title = element_text(size = 32),
#         plot.title = element_text(size = 28),
#         plot.subtitle = element_text(size = 24), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         strip.text = element_text(size = 24),
#         plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


# slug models and plot ####

slug_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, s)
unique(slug_model$treatment)

m0 <- glmer(s ~ +
        (1|year/block/plot_id), data = slug_model,
      family = binomial)

m1 <- glmer(s ~ treatment +
              (1|year/block/plot_id), data = slug_model,
            family = binomial)

m2 <- glmer(s ~ treatment+growth_stage +
              (1|year/block/plot_id), data = slug_model,
            family = binomial)

m3 <- glmer(s ~ treatment*growth_stage +
              (1|year/block/plot_id), data = slug_model,
            family = binomial)

rePCA(m3)
Anova(m3)
anova(m0,m1,m2,m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    4 8003.1 8030.3 -3997.5   7995.1                          
# m1    7 8007.7 8055.4 -3996.8   7993.7  1.4304  3     0.6984    
# m2    8 7958.6 8013.1 -3971.3   7942.6 51.0817  1  8.860e-13 ***
# m3   11 7923.7 7998.7 -3950.9   7901.7 40.8481  3  7.043e-09 ***
summary(m3)
res <- residuals(m3)
qqline(res)
plot(fitted(m3), res)
hist(residuals(m3))
plot(density(res))

cld(emmeans(m3, ~treatment|growth_stage, type = 'response'), Letters = letters)
# growth_stage = V3:
#   treatment  prob    SE  df asymp.LCL asymp.UCL .group
# 1         0.466 0.128 Inf     0.242     0.706  a    
# 3         0.508 0.129 Inf     0.273     0.739  a    
# 4         0.537 0.128 Inf     0.297     0.761  a    
# 2         0.609 0.123 Inf     0.362     0.811  a    
# 
# growth_stage = V5:
#   treatment  prob    SE  df asymp.LCL asymp.UCL .group
# 4         0.358 0.119 Inf     0.169     0.605  a    
# 2         0.409 0.125 Inf     0.201     0.656  a    
# 1         0.467 0.128 Inf     0.242     0.706  a    
# 3         0.472 0.128 Inf     0.246     0.710  a    



cld(emmeans(m3, ~growth_stage, type = 'response'), Letters = letters)
# growth_stage  prob    SE  df asymp.LCL asymp.UCL .group
# V5           0.426 0.120 Inf     0.220     0.661  a    
# V3           0.531 0.123 Inf     0.301     0.748   b 

slug_dmg_plot <- cld(emmeans(m3, ~treatment|growth_stage, type = 'response'), Letters = letters)



gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

num_labs <- data.frame(label = c('1)', '2)'),
                       growth_stage = c('V3', 'V5'))

slug_plot <- slug_dmg_plot %>% 
  ggplot(aes(x = treatment, y = prob))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = treatment, ymin = prob - SE, ymax = prob + SE, width = .5), data = slug_dmg_plot)+
  ylim(0, 0.78)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(
      title = 'Slug damage',
    #    subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Response damage incidence')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
           panel.grid.major.x = element_blank(),
           panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = num_labs, mapping = aes(x = 0.6, y = 0.76,label = label), size = 8)

slug_plot


raw_slug <- slug_model %>% 
  group_by(growth_stage, treatment, plot_id) %>% 
  summarise(mean = mean(s),
            sd = sd(s), 
            n = n(), 
            se = sd/sqrt(n))


ggplot(raw_slug, aes(color = treatment))+
  geom_point(aes(x = treatment, y = mean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+ 
  labs(
    title = "Corn: Slug Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Average damage incidence"
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
        strip.text.x = element_text(size = 32), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

ggplot(raw_slug, aes(x = treatment, y = mean, fill = treatment))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+ 
  labs(
    title = "Corn: Slug Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Average damage incidence"
  )+
  theme(legend.position = "none",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 32), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(title = 'Growth Stage'))+
  expand_limits(y=0)


# bcw models and plot ####
bcw_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, bcw) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

bm0 <- glmer(bcw ~ +
              (1|year/block/plot_id), data = bcw_model,
            family = binomial)

bm1 <- glmer(bcw ~ treatment +
              (1|year/block/plot_id), data = bcw_model,
            family = binomial)

bm2 <- glmer(bcw ~ treatment+growth_stage +
              (1|year/block/plot_id), data = bcw_model,
            family = binomial)

bm3 <- glmer(bcw ~ treatment*growth_stage +
              (1|year/block/plot_id), data = bcw_model,
            family = binomial)
Anova(bm3)
anova(bm0, bm1, bm2, bm3)


summary(bm3)
hist(residuals(bm3))
resb <- residuals(bm3)
qqnorm(resb)
r2_nakagawa(bm3)

cld(emmeans(bm3, ~growth_stage, type = 'response'), Letters = letters)
cld(emmeans(bm3, ~treatment, type = 'response'), Letters = letters)
cld(emmeans(bm3, ~treatment|growth_stage, type = 'response'), Letters = letters)

bcw_dm_plot <- cld(emmeans(bm3, ~treatment|growth_stage, type = 'response'), Letters = letters)


gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

num_labs <- data.frame(label = c('1)', '2)'),
                       growth_stage = c('V3', 'V5'))

bcw_plot <- bcw_dm_plot %>% 
  ggplot(aes(x = treatment, y = prob))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = treatment, ymin = prob - SE, ymax = prob + SE, width = .5), data = bcw_dm_plot)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(
      title = 'Black cutworm damage',
    #    subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Response damage incidence')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = bcw_dm_plot, aes(y = 0.14, label = trimws(.group)), size = 8)+
  geom_text(data = num_labs, mapping = aes(x = 0.6, y = 0.15,label = label), size = 8)
bcw_plot

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

raw_bcw <- bcw_model %>% 
  group_by(growth_stage, treatment, plot_id) %>% 
  summarise(mean = mean(bcw),
            sd = sd(bcw), 
            n = n(), 
            se = sd/sqrt(n))

ggplot(raw_bcw, aes(color = treatment))+
  geom_point(aes(x = treatment, y = mean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+ 
  labs(
    title = "Corn: Black Cutworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Average damage"
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
        strip.text.x = element_text(size = 32), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(x = treatment, y = 0.1, label = trimws(letters)), size = 10, color = "black")


ggplot(raw_bcw, aes(x = treatment, y = mean, fill = treatment))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_boxplot(alpha = 0.7)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+ 
  labs(
    title = "Corn: Black Cutworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Average damage incidence"
  )+
  theme(legend.position = "none",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 32), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(title = 'Growth Stage'))+
  scale_y_continuous(limits = c(0,.15))






# taw models and plot ####
taw_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, taw) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

tm0 <- glmer(taw ~ +
               (1|year/block/plot_id), data = taw_model,
             family = binomial)

tm1 <- glmer(taw ~ treatment +
               (1|year/block/plot_id), data = taw_model,
             family = binomial)

tm2 <- glmer(taw ~ treatment+growth_stage +
               (1|year/block/plot_id), data = taw_model,
             family = binomial)

tm3 <- glmer(taw ~ treatment*growth_stage +
               (1|year/block/plot_id), data = taw_model,
             family = binomial)

anova(tm0, tm1, tm2, tm3)
hist(residuals(tm3))
summary(tm3)
r2_nakagawa(tm3)
cld(emmeans(tm3, ~treatment|growth_stage), Letters = letters)



# taw.table <- as.data.frame(summary(tm3)$coefficients)
# #CI <- confint(m3)
# taw.table <-cbind(row.names(taw.table), taw.table)
# names(taw.table) <- c("Term", "B", "SE", "t", "p")
# taw.table <- as_tibble(taw.table) %>% 
#   mutate(Term = case_when(Term == 'treatment2' ~ '14-28 DPP',
#                           Term == 'treatment4' ~ '3-7 DPP',
#                           Term == 'treatment3' ~ '1-3 DAP',
#                           Term == 'growth_stageV5' ~ 'V5',
#                           Term == 'treatment2:growth_stageV5' ~ '14-28 DPP:V5',
#                           Term == 'treatment3:growth_stageV5' ~ '1-3 DAP:V5',
#                           Term == 'treatment4:growth_stageV5' ~ '3-7 DPP:V5',
#                           .default = as.character(Term)))
# taw.table <- flextable(taw.table)
# taw.table <- autofit(taw.table)
# taw.table <- add_header_lines(taw.table,
#                               values = 'True Armyworm: Summary')
# theme_zebra(taw.table) %>% 
#   save_as_docx(path = 'taw_summary_table.docx')

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")


ggplot(taw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75))+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: True Armyworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
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
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
  # geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")


ggplot(taw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = emmean), size = 10,
             position = position_dodge(width = .75), color = "black")+
  facet_wrap(~growth_stage)+
  geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: True Armyworm Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage Emmean",
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
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
  # geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")




# sb models and plot ####
sb_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, sb) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

sbm0 <- glmer(sb ~ +
               (1|year/block/plot_id), data = sb_model,
             family = binomial)

sbm1 <- glmer(sb ~ treatment +
               (1|year/block/plot_id), data = sb_model,
             family = binomial)

sbm2 <- glmer(sb ~ treatment+growth_stage +
               (1|year/block/plot_id), data = sb_model,
             family = binomial)

sbm3 <- glmer(sb ~ treatment*growth_stage +
               (1|year/block/plot_id), data = sb_model,
             family = binomial)

anova(sbm0, sbm1, sbm2, sbm3)
summary(sbm3)
r2_nakagawa(sbm3)
cld(emmeans(sbm3, ~treatment|growth_stage), Letters = letters)


ggplot(sb_mult, aes(x = treatment, y = mean, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  facet_wrap(~growth_stage)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(
    title = "Corn: sb Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Average damage"
  )+
  theme(legend.position = "none",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        # axis.line = element_line(size = 1.25),
        # axis.ticks = element_line(size = 1.25),
        # axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 32), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(title = 'Growth Stage'))
  geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")





# multiple models and plot ####

mult_model <- new_dmg %>% 
  dplyr::select(year, growth_stage, block, plot_id, treatment, multiple) %>% 
  mutate(plot_id = as.factor(plot_id))
unique(bcw_model$treatment)

mm0 <- glmer(multiple ~ +
                (1|year/block/plot_id), data = mult_model,
              family = binomial)

mm1 <- glmer(multiple ~ treatment +
                (1|year/block/plot_id), data = mult_model,
              family = binomial)

mm2 <- glmer(multiple ~ treatment+growth_stage +
                (1|year/block/plot_id), data = mult_model,
              family = binomial)

mm3 <- glmer(multiple ~ treatment*growth_stage +
                (1|year/block/plot_id), data = mult_model,
              family = binomial)

isSingular(mm3)
Anova(mm3)
anova(mm0, mm1, mm2, mm3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# mm0    4 2609.4 2636.7 -1300.7   2601.4                          
# mm1    7 2593.3 2641.0 -1289.7   2579.3 22.0783  3  6.283e-05 ***
# mm2    8 2582.0 2636.5 -1283.0   2566.0 13.3227  1  0.0002622 ***
# mm3   11 2583.3 2658.3 -1280.7   2561.3  4.6563  3  0.1987644 

summary(mm3)
hist(residuals(mm3))
resmm <- residuals(mm3)
qqnorm(resmm)
r2_nakagawa(mm3)

cld(emmeans(mm3, ~treatment, type = 'response'), Letters = letters)
cld(emmeans(mm3, ~growth_stage, type = 'response'), Letters = letters)
cld(emmeans(mm3, ~treatment|growth_stage, type = 'response'), Letters = letters)



mult_dmg_plot <- cld(emmeans(mm3, ~treatment|growth_stage, type = 'response'), Letters = letters)

gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

num_labs <- data.frame(label = c('1)', '2)'),
                       growth_stage = c('V3', 'V5'))

mm_plot <- mult_dmg_plot %>% 
  ggplot(aes(x = treatment, y = prob))+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  geom_point(size = 5)+
  geom_errorbar(aes(x = treatment, ymin = prob - SE, ymax = prob + SE, width = .5), data = mult_dmg_plot)+
  ylim(0,.135)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(
      title = 'Multiple pest damage',
    #    subtitle = "Years: 2021-2023",
       x = 'Treatment termination',
       y = 'Response damage incidence')+
  theme_bw()+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(size=22),
        axis.text.y = element_text(size = 26),
        axis.title  = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24),
        strip.text = element_text(size = 24),
        axis.ticks = element_blank())+
  geom_text(data = mult_dmg_plot, aes(y = 0.13, label = trimws(.group)), size = 8)+
  geom_text(data = num_labs, mapping = aes(x = 0.6, y = 0.135,label = label), size = 8)
mm_plot






gs.labs <- c("V3  a", "V5  b")
names(gs.labs) <- c("V3", "V5")

raw_mult <- mult_model %>% 
  group_by(growth_stage, treatment, plot_id) %>% 
  summarise(mean = mean(multiple),
            sd = sd(multiple), 
            n = n(), 
            se = sd/sqrt(n))


raw_mult %>% 
  mutate(group = case_when(
     growth_stage == 'V3' & treatment == '1' ~ 'a',
     growth_stage == 'V3' & treatment == '2' ~ 'a',
     growth_stage == 'V3' & treatment == '4' ~ 'a',
     growth_stage == 'V3' & treatment == '3' ~ 'b',
     growth_stage == 'V5' & treatment == '1' ~ 'a',
     growth_stage == 'V5' & treatment == '4' ~ 'a',
     growth_stage == 'V5' & treatment == '2' ~ 'a',
     growth_stage == 'V5' & treatment == '3' ~ 'b',
  )) %>% 
ggplot(aes(x = treatment, y = mean, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  facet_wrap(~growth_stage, labeller = labeller(growth_stage = gs.labs))+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs(
    title = "Corn: Multiple Damage x Treatment",
    subtitle = "Years: 2021-2023",
    x = "Treatment termination",
    y = "Average damage incidence"
  )+
  theme(legend.position = "none",
        legend.text = element_text(size = 24),
        legend.title = element_text(size = 24),
        axis.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24),
        plot.title = element_text(size = 28),
        axis.line = element_line(size = 1.25),
        axis.ticks = element_line(size = 1.25),
        axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        strip.text.x = element_text(size = 32), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  guides(fill = guide_legend(title = 'Growth Stage'))+
  scale_y_continuous(limits = c(0,.2))+
  geom_text(aes(label = group, y = .18), size = 10)


# Combing slug, bcw, and multiple pest plots into one ####


comb_plot <- ggarrange(slug_plot + rremove("ylab") + rremove("xlab")  + rremove("x.text"), 
          bcw_plot + rremove("ylab") + rremove("xlab"), 
          mm_plot +  rremove("ylab") + rremove("xlab"),
          labels = c('1','2','3'),
          font.label = list(size = 20, color = 'cornsilk4'),
          hjust = -1.5)
annotate_figure(comb_plot, bottom = text_grob("Treatment termination", size = 36), 
                left = text_grob("Response-scale damage incidence", rot = 90, size = 30))


# DO NOT USE: other models and plot ####

# do not use other #

# oth_model <- new_dmg %>% 
#   dplyr::select(year, growth_stage, block, plot_id, treatment, other) %>% 
#   mutate(plot_id = as.factor(plot_id))
# unique(oth_model$treatment)
# 
# om0 <- glmer(other ~ +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# om1 <- glmer(other ~ treatment +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# om2 <- glmer(other ~ treatment+growth_stage +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# om3 <- glmer(other ~ treatment*growth_stage +
#                (1|year/block/plot_id/growth_stage), data = oth_model,
#              family = binomial)
# 
# anova(om0, om1, om2, om3)
# summary(om3)
# r2_nakagawa(om3)
# om3_em <- emmeans(om3, ~treatment*growth_stage)
# 
# pwpm(mm3_em)
# 
# om_em <- as.data.frame(cld(om3_em, Letters = letters))
# 
# gs.labs <- c("V3  a", "V5  b")
# names(gs.labs) <- c("V3", "V5")
# 
# 
# ggplot(om_em, aes(color = treatment))+
#   geom_point(aes(x = treatment, y = emmean), size = 10,
#              position = position_dodge(width = .75))+
#   facet_wrap(~growth_stage)+
#   geom_errorbar(aes(x = treatment,ymin = emmean - SE, ymax = emmean + SE),
#                 color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
#   scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
#   labs(
#     title = "Corn: Other Pest Damage x Treatment",
#     subtitle = "Years: 2021-2023",
#     x = "Treatment",
#     y = "Damage Emmean",
#     caption = "DPP: Days pre plant
# DAP: Days after plant"
#   )+
#   theme(legend.position = 'none',
#         axis.title = element_text(size = 32),
#         plot.subtitle = element_text(size = 24),
#         plot.title = element_text(size = 28),
#         # axis.line = element_line(size = 1.25),
#         # axis.ticks = element_line(size = 1.25),
#         # axis.ticks.length = unit(.25, "cm"),
#         axis.text.x = element_text(size = 26),
#         axis.text.y = element_text(size = 26),
#         strip.text.x = element_text(size = 26), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank(),
#         plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))
#   #geom_text(aes(x = treatment, y = -1.8, label = trimws(.group)), size = 10, color = "black")


# old code beloooooowwwww 


# old model code ####




test_m1 <- glmer(s ~ treatment + growth_stage +
                   (1|year/block/plot_id), data = dmg_model,
                 family = binomial)
summary(test_m1)
r2_nakagawa(test_m1)
hist(residuals(test_m1))

#this is the one 
test_m2 <- glmer(s ~ treatment  +
                   (1|year/growth_stage/block), data = dmg_model,
                 family = binomial)
summary(test_m2)
r2_nakagawa(test_m2)
hist(residuals(test_m2))

# m3 = overfot/ singular. Removing plot 
test_m3 <- glmer(s ~ treatment  +
                   (1|year/growth_stage/block/plot_id), data = dmg_model,
                 family = binomial)
summary(test_m3)
r2_nakagawa(test_m3)
hist(residuals(test_m3))

###
##
#
# model other, slug, and taw (singularity in the loop)
other <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, other))
other_m1 <- glmer(other ~ treatment +
                   (1|year), data = dmg_model,
                 family = binomial)
other_saved <- emmeans(other_m1, pairwise ~ treatment, type = 'response')
other_saved <- as.data.frame(other_saved$emmeans)
summary(other_m1)
r2_nakagawa(other_m1)
model_performance(other_m1)

# slug model # 
slug <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, s))
slug_m1 <- glmer(s ~ treatment +
                   (1|year/block), data = dmg_model,
                 family = binomial)
slug_saved <- emmeans(slug_m1, pairwise ~ treatment, type = 'response')
slug_saved <- as.data.frame(slug_saved$emmeans)
summary(slug_m1)
r2_nakagawa(slug_m1)

taw <- subset(dmg_model, select =  c(year, growth_stage, block, plot_id, treatment, transect, plant_num, damage_score, taw))
taw_m1 <- glmer(taw ~ treatment+
                    (1|year/block), data = dmg_model,
                  family = binomial)
taw_saved <- emmeans(taw_m1, pairwise ~ treatment, type = 'response')
taw_saved <- as.data.frame(taw_saved$emmeans)
summary(taw_m1)
r2_nakagawa(taw_m1)

#
##
###
# models for damage type  ####
# test model to look at variables before we run the loop
dmg_model <- new_dmg



pest_columns <- c('s','taw', 'bcw','sb', 'multiple', 'other')
summary_list <- list()
r2_list <- list()
emms_mod <- list()
for (pest in 1:length(pest_columns)) {
  print(pest)
  new_col <- pest_columns[pest]
  new_df <- subset(dmg_model, select = c('year', 'growth_stage', 'block', 'plot_id', 'treatment', new_col))
  colnames(new_df) <- c('year', 'growth_stage', 'block','plot_id', 'treatment', 'new_col')
  model <- glmer(new_col ~ treatment*growth_stage +
                   (1|year/block/plot_id/growth_stage), data = new_df,
                 family = binomial)
  emms_mod[[pest]] <- emmeans(model, ~treatment*growth_stage,type = "response")
  summary_model <- summary(model)
  summary_list[[pest]] <- summary_model
  r2_model <- r2_nakagawa(model)
  r2_list[[pest]] <- r2_model
}

summary_list
r2_list
emms_mod
pairs(emms_mod[[1]], simple = "each") # slug
pairs(emms_mod[[2]], simple = "each") # taw
pairs(emms_mod[[3]], simple = "each") # bcw  ***
pairs(emms_mod[[4]], simple = "each") # stink bug
pairs(emms_mod[[5]], simple = "each") # multiple
pairs(emms_mod[[6]], simple = "each") # other

slug_em <- emms_mod[[1]]
slug_em <- as.data.frame(slug_em)
slug_em['pest'] <- ("slug")

taw_em <- emms_mod[[2]]
taw_em <- as.data.frame(taw_em)
taw_em['pest'] <- ("taw")

bcw_em <- emms_mod[[3]]
bcw_em <- as.data.frame(bcw_em)
bcw_em['pest'] <- ("bcw")

sb_em <- emms_mod[[4]]
sb_em <- as.data.frame(sb_em)
sb_em['pest'] <- ("sb")

m_em <- emms_mod[[5]]
m_em <- as.data.frame(m_em)
m_em['pest'] <- ("multiple")

other_em <- emms_mod[[6]]
other_em <- as.data.frame(other_em)
other_em['pest'] <- ("other")


all_emmeans <- rbind(slug_em, taw_em, bcw_em, sb_em, m_em, other_em) 
as_tibble(all_emmeans)
all_emmeans$pest <- as.factor(all_emmeans$pest)

# plot ####

# all years
#facet order
all_emmeans$pest_f <- factor(all_emmeans$pest, levels =c('slug', 'sb', 'bcw', 'taw',
                                               'multiple', 'other')) 

ggplot(all_emmeans, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 3,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0, linewidth = 1.5)+
  geom_errorbar(aes(x = treatment,ymin = asymp.LCL, ymax = asymp.UCL), 
                alpha = .6, width = 0, linewidth = 1)+
  facet_wrap(~pest_f + growth_stage, scales = "free")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
        axis.text.y = element_text(size = 12))

# each plot individually then added together 
# template will be slug

slug_em
pairs(emms_mod[[1]])
ggplot(slug_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Slug damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
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

bcw_em
pairs(emms_mod[[3]])
pwpm(emms_mod[[3]])
ggplot(bcw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Black Cutworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

taw_em
pairs(emms_mod[[2]])
ggplot(taw_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+
  labs(
    title = "True Armyworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
          axis.text.y = element_text(size = 18),legend.position = "none",
          strip.text = element_text(size = 16),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(s = 16), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

# stink bug not useful
sb_em
pairs(emms_mod[[4]])
ggplot(sb_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Stink bug damage",
    subtitle = "Years: 2022-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


m_em
pairs(emms_mod[[5]])
pwpm(emms_mod[[5]])
ggplot(m_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Multiple damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

other_em
pairs(emms_mod[[6]])
ggplot(other_em, aes(color = treatment))+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_color_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Other damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggarrange(slug_fig, bcw_fig, taw_fig, sb_fig, mult_fig, other_fig)

# black and white figs for pubs ####
slug_em
pairs(emms_mod[[1]])
ggplot(slug_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+ 
  labs(
    title = "Corn: Slug damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
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

bcw_em
pairs(emms_mod[[3]])
pwpm(emms_mod[[3]])
ggplot(bcw_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Black Cutworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

taw_em
pairs(emms_mod[[2]])
ggplot(taw_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+
  labs(
    title = "True Armyworm damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# stink bug not useful
sb_em
pairs(emms_mod[[4]])
ggplot(sb_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Stink bug damage",
    subtitle = "Years: 2022-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


m_em
pairs(emms_mod[[5]])
pwpm(emms_mod[[5]])
ggplot(m_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Multiple damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

other_em
pairs(emms_mod[[6]])
ggplot(other_em)+
  geom_point(aes(x = treatment, y = prob), size = 5,
             position = position_dodge(width = .75))+
  geom_errorbar(aes(x = treatment,ymin = prob - SE, ymax = prob + SE),
                color = "black", alpha = 1, width = 0.2, linewidth = 1.5)+
  facet_wrap(~growth_stage)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+ 
  labs(
    title = "Other damage",
    subtitle = "Years: 2021-2023",
    x = "Treatment",
    y = "Damage probability"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),legend.position = "none",
        strip.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



