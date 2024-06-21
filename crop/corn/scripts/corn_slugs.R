# Jared Adam
# first date unknown
#revisit : 2/14/2024

# slugs baby lettuce goooo 

# packages ####
library(tidyverse)
library(emmeans)
library(lme4)
library(lmtest)
library(MASS)
library(nlme)
library(plotly)
library(multcomp)
library(ggpubr)
library(corrplot)
library(ggcorrplot)

# data ####
slugs <- PSA_PA_slugs

# test ####
  
colnames(slugs)
test_slug <- slugs[1:200,]
test_slug$month <- gsub(" ", "", test_slug$month)
test_slug %>% 
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  dplyr::select(-date) %>% 
  group_by(season, year, month, plot_id, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count)) %>%  
  print(n = Inf)

# # precip 
# test_slug %>% 
#   dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
#   mutate(date = as.Date(date, "%m/%d/%Y"),
#          year = format(date, '%Y')) %>% 
#   rename(precip = "7day_precip") %>% 
#   group_by(season, year, month, block) %>% 
#   summarise(total_precip = sum(precip))



# whole data set ####

slug_clean <- slugs %>% 
  dplyr::select(-location, -shingle_id, -time, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  # dplyr::select(date) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>%
  mutate(season = case_when(season == "fall" ~ "Fall", 
                            season == "spring" ~ "Spring")) %>% 
  group_by(season, year, month, plot_id, treatment, block, precip, temp, date) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  replace(is.na(.),0) %>% 
  arrange(date, plot_id) %>% 
  print(n = Inf)

#subset by season


# cs_21 <- subset(slug_clean, year == "2021")
# cs_22 <- subset(slug_clean, year == "2022")
# cs_23 <- subset(slug_clean, year == "2023")
# explore the data ####

#explore
unique(slug_clean$date)
slug_clean <- slug_clean %>% 
  mutate_at(vars(1:6), as_factor) %>% 
  mutate(date = as.character(date)) %>% 
  mutate(date = as_factor(date))

cat_resp <- names(slug_clean[10])
cat_resp <- set_names(cat_resp)

cat_exp <- names(slug_clean[1:6])
cat_exp <- set_names(cat_exp)


box_plots <- function(x,y) {
  ggplot(slug_clean, aes(x = .data[[x]], y = .data[[y]]))+
    geom_boxplot()+
    theme_bw()
}

cat_plots <- map(cat_exp, ~box_plots(.x, 'total_slug'))
cat_plots
ggarrange(plotlist = cat_plots)



con_resp <- names(slug_clean[10])
con_resp <- set_names(con_resp)

con_exp <- names(slug_clean[7:8])
con_exp <- set_names(con_exp)

scatter_plots <- function(x,y){
  ggplot(slug_clean, aes(x = .data[[x]], y = .data[[y]]))+
    geom_point()+
    stat_smooth(method = 'loess', se = FALSE, color = 'grey75')+
    theme_bw()
}

con_plots <- map(con_exp, ~scatter_plots(.x, 'total_slug'))
ggarrange(plotlist = con_plots)


ggplot(slug_clean, aes(x = season, y = precip))+
  geom_boxplot()+
  theme_bw()
ggplot(slug_clean, aes(x = season, y = temp))+
  geom_boxplot()+
  theme_bw()


# correlation test 
slug_cor <- slug_clean %>% 
  ungroup() %>% 
  dplyr::select(total_slug, block, plot_id, season, treatment, year, date)

model.matrix(~0+., data = slug_cor) %>% 
  cor(use = 'pairwise.complete.obs') %>% 
  ggcorrplot(show.diag = FALSE, type = 'lower', lab = TRUE, lab_size = 2)

# model choice? ####

# look at overdispersion: variance > mean?
dispersion_stats <- slug_clean %>% 
  group_by(treatment) %>%
  summarise(
    mean = mean(total_slug, na.rm=TRUE),
    variances = var(total_slug, na.rm=TRUE),
    ratio = variances/mean) 
if(dispersion_stats$mean[1] > dispersion_stats$variances[1] & 
   dispersion_stats$mean[2] > dispersion_stats$variances[2] &
   dispersion_stats$mean[3] > dispersion_stats$variances[3] &
   dispersion_stats$mean[4] > dispersion_stats$variances[4]){
  print("run a poisson, probs")
  } else {
    print("these jawns overdispersed")
  }


# balance dates ####
# SPRING
# balance the number of weekly observations

spring_slugs <- subset(slug_clean, season == "Spring") %>% 
  mutate_at(vars(1:6), as.factor)

#2021
spring_slugs %>% 
  ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, year) %>% 
  subset(year == '2021') %>% 
  distinct(date)
# date      
# <date>    
# 1 2021-05-25
# 2 2021-06-01
# 3 2021-06-08
# 4 2021-06-15
# 5 2021-06-23
# 6 2021-06-30
# 7 2021-07-07

spring_slugs %>% 
  ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, year) %>% 
  subset(year == '2022') %>% 
  distinct(date)
# date      
# <date>    
# 1 2022-06-07
# 2 2022-06-13
# 3 2022-06-24
# 4 2022-06-29

spring_slugs %>% 
  ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, year) %>% 
  subset(year == '2023') %>% 
  distinct(date)
# 1 2023-05-31
# 2 2023-06-05
# 3 2023-06-12
# 4 2023-06-19
# 5 2023-06-26
# 6 2023-07-03

spring_slugs <- spring_slugs %>% 
  filter(!date %in% c('2021-06-23',
                      '2021-06-30',
                      '2021-07-07',
                      '2023-06-26',
                      '2023-07-03')) %>% 
  mutate(week = case_when(
    date == '2021-05-25' ~ '1',
    date == '2021-06-01' ~ '2',
    date == '2021-06-08' ~ '3',
    date == '2021-06-15' ~ '4',
    date == '2022-06-07' ~ '1',
    date == '2022-06-13' ~ '2',
    date == '2022-06-24' ~ '3',
    date == '2022-06-29' ~ '4',
    date == '2023-05-31' ~ '1',
    date == '2023-06-05' ~ '2',
    date == '2023-06-12' ~ '3',
    date == '2023-06-19' ~ '4'
  )) %>% 
  mutate(week = as.factor(week)) %>% 
  print(n = Inf)
unique(spring_slugs$date)


# FALL
fall_slugs <- subset(slug_clean, season == "Fall")%>% 
  mutate_at(vars(1:6), as.factor)
#2021
fall_slugs %>% 
  ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, year) %>% 
  subset(year == '2021') %>% 
  distinct(date)
# date      
# <date>    
# 1 2021-09-13
# 2 2021-09-24
# 3 2021-10-01
# 4 2021-10-07
# 5 2021-10-13
# 6 2021-10-22

fall_slugs %>% 
  ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, year) %>% 
  subset(year == '2022') %>% 
  distinct(date)
# date      
# <date>    
# 1 2022-09-16
# 2 2022-09-21
# 3 2022-09-28
# 4 2022-10-05
# 5 2022-10-12
# 6 2022-10-21
# 7 2022-10-26
# 8 2022-11-02

fall_slugs %>% 
  ungroup() %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(date, year) %>% 
  subset(year == '2023') %>% 
  distinct(date)
# date      
# <date>    
# 1 2023-09-18
# 2 2023-09-29
# 3 2023-10-03

fall_slugs <- fall_slugs %>% 
  filter(!date %in% c('2021-10-07',
                      '2021-10-13',
                      '2021-10-22',
                      '2022-10-05',
                      '2022-10-12',
                      '2022-10-21',
                      '2022-10-26',
                      '2022-11-02')) %>% 
  mutate(week = case_when(
    date == '2021-09-13' ~ '1',
    date == '2021-09-24' ~ '2',
    date == '2021-10-01' ~ '3',
    date == '2022-09-16' ~ '1',
    date == '2022-09-21' ~ '2',
    date == '2022-09-28' ~ '3',
    date == '2023-09-18' ~ '1',
    date == '2023-09-29' ~ '2',
    date == '2023-10-03' ~ '3'
  )) %>% 
  mutate(week = as.factor(week)) %>% 
  print(n = Inf)
unique(fall_slugs$date)


# model selection ####
spoisson_model <- glmer(total_slug ~ treatment*year*week + 
                          (week|block/plot_id), 
                        data = spring_slugs, 
                        family = poisson)

snb_model_trt <- glmer.nb(total_slug ~ treatment*year*week + 
                            (week|block/plot_id), 
                          data = spring_slugs) 

gaus_model <- lmer(total_slug ~ treatment*year*week + 
                      (week|block/plot_id), 
                    data = spring_slugs)

lrtest(spoisson_model, snb_model_trt, gaus_model)
# the negative binomial has the higher likelihood score, so we will use that


# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 
fpoisson_model <- glmer(total_slug ~ treatment*year + 
                         (1|block), 
                       data = fall_slugs, 
                       family = poisson)

fnb_model_trt <- glmer.nb(total_slug ~ treatment*year + 
                           (1|block), 
                         data = fall_slugs) 

lrtest(fpoisson_model, fnb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that



# NOT in thesis RM models ####
unique(fall_slugs$date)

# Fall

f0 <- glmer.nb(total_slug ~ +
                 (week|block/plot_id),
               data = fall_slugs)

f1 <- glmer.nb(total_slug ~ treatment +
                 (week|block/plot_id),
               data = fall_slugs)

f2 <- glmer.nb(total_slug ~ treatment+year+
                 (week|block/plot_id),
               data = fall_slugs)

f3 <- glmer.nb(total_slug ~ treatment+year+week+
                 (week|block/plot_id),
               data = fall_slugs)


f4 <- glmer.nb(total_slug ~ treatment*year+week+
                 (week|block/plot_id),
               data = fall_slugs)

f4.5 <- glmer.nb(total_slug ~ treatment + year * week +
                 (week|block/plot_id), data = fall_slugs) 

f5 <- glmer.nb(total_slug ~   year + treatment* week +
                 (week|block/plot_id), data = fall_slugs) 

f6 <- glmer.nb(total_slug ~ treatment*year*week +
                 (week|block/plot_id), data = fall_slugs) 

# p3 <- glmer.nb(total_slug ~ precip +
#                  (1|year/block), data = fall_slugs) 
anova(f3, p3)

rePCA(f3)

anova(f0, f1, f2, f3, f4 , f4.5, f5, f6)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# f0     14 893.10 937.80 -432.55   865.10                          
# f1     17 892.44 946.72 -429.22   858.44  6.6622  3   0.083482 .  
# f2     19 848.57 909.24 -405.29   810.57 47.8643  2  4.040e-11 *** #yr
# f3     21 839.53 906.59 -398.77   797.53 13.0380  2   0.001475 ** #week
# f4.5   25 820.55 900.37 -385.27   770.55 26.9864  4  2.000e-05 *** #trt*yr
# f4     27 828.05 914.26 -387.02   774.05  0.0000  2   1.000000    
# f5     27 846.01 932.22 -396.01   792.01  0.0000  0               
# f6     49 811.61 968.07 -356.81   713.61 78.3983 22  2.967e-08 *** #trt*yr*week
# ?waldtest
# waldtest(f1, test = 'F')

check_model(f6)
summary(f6)
hist(residuals(f6))
res <- residuals(f6)
qqnorm(res)
plot(fitted(f6), res)
check_singularity(f6)
r2_nakagawa(f6) 

cld(emmeans(f3, ~week, type = 'response'), Letters = letters)
# week response    SE  df asymp.LCL asymp.UCL .group
# 1        1.38 0.349 Inf      0.84      2.26  a    
# 2        2.83 0.410 Inf      2.13      3.76   b   
# 3        5.84 0.703 Inf      4.61      7.39    c  

cld(emmeans(f3, ~year, type = 'response'), Letters = letters)
# year response    SE  df asymp.LCL asymp.UCL .group
# 2021     1.43 0.259 Inf      1.01      2.04  a    
# 2022     3.09 0.509 Inf      2.24      4.27   b   
# 2023     5.14 0.814 Inf      3.77      7.01    c  

cld(emmeans(f3, ~treatment|year, type = 'response'), Letters = letters)
# year = 2021:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3             1.05 0.247 Inf     0.664      1.67  a    
# 4             1.35 0.297 Inf     0.881      2.08  ab   
# 2             1.39 0.299 Inf     0.908      2.12  ab   
# 1             2.13 0.447 Inf     1.415      3.22   b   
# 
# year = 2022:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3             2.27 0.510 Inf     1.459      3.53  a    
# 4             2.92 0.605 Inf     1.946      4.38  ab   
# 2             2.99 0.601 Inf     2.017      4.43  ab   
# 1             4.60 0.899 Inf     3.135      6.75   b   
# 
# year = 2023:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3             3.77 0.777 Inf     2.522      5.65  a    
# 4             4.86 0.964 Inf     3.294      7.17  ab   
# 2             4.98 1.019 Inf     3.331      7.43  ab   
# 1             7.65 1.534 Inf     5.167     11.33   b  



cld(emmeans(f3, ~treatment|year|week, type = 'response'), Letters = letters)
# year = 2021, week = 3:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            2.166 0.482 Inf     1.400      3.35  a    
# 4            2.788 0.584 Inf     1.850      4.20  ab   
# 2            2.856 0.590 Inf     1.904      4.28  ab   
# 1            4.392 0.869 Inf     2.980      6.47   b   
# 
# year = 2022, week = 3:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            4.671 0.977 Inf     3.099      7.04  a    
# 4            6.013 1.157 Inf     4.123      8.77  ab   
# 2            6.158 1.151 Inf     4.269      8.88  ab   
# 1            9.471 1.695 Inf     6.669     13.45   b   
# 
# year = 2023, week = 3:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            7.773 1.533 Inf     5.281     11.44  a    
# 4           10.006 1.923 Inf     6.865     14.58  ab   
# 2           10.247 2.049 Inf     6.925     15.16  ab   
# 1           15.761 3.049 Inf    10.788     23.03   b   
# 
# year = 2021, week = 1:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            0.512 0.164 Inf     0.273      0.96  a    
# 4            0.659 0.204 Inf     0.360      1.21  ab   
# 2            0.675 0.206 Inf     0.371      1.23  ab   
# 1            1.038 0.313 Inf     0.575      1.87   b   
# 
# year = 2022, week = 1:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            1.104 0.343 Inf     0.600      2.03  a    
# 4            1.421 0.422 Inf     0.794      2.54  ab   
# 2            1.455 0.425 Inf     0.821      2.58  ab   
# 1            2.238 0.646 Inf     1.271      3.94   b   
# 
# year = 2023, week = 1:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            1.837 0.537 Inf     1.036      3.26  a    
# 4            2.365 0.676 Inf     1.350      4.14  ab   
# 2            2.422 0.702 Inf     1.373      4.27  ab   
# 1            3.725 1.069 Inf     2.122      6.54   b   
# 
# year = 2021, week = 2:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            1.049 0.246 Inf     0.662      1.66  a    
# 4            1.351 0.296 Inf     0.880      2.07  ab   
# 2            1.383 0.298 Inf     0.907      2.11  ab   
# 1            2.128 0.447 Inf     1.409      3.21   b   
# 
# year = 2022, week = 2:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            2.263 0.527 Inf     1.433      3.57  a    
# 4            2.913 0.626 Inf     1.912      4.44  ab   
# 2            2.983 0.623 Inf     1.981      4.49  ab   
# 1            4.588 0.940 Inf     3.070      6.86   b   
# 
# year = 2023, week = 2:
#   treatment response    SE  df asymp.LCL asymp.UCL .group
# 3            3.765 0.802 Inf     2.480      5.72  a    
# 4            4.847 0.994 Inf     3.243      7.24  ab   
# 2            4.964 1.049 Inf     3.281      7.51  ab   
# 1            7.635 1.591 Inf     5.075     11.49   b 



# sl.table <- as.data.frame(summary(m1)$coefficients)
# #CI <- confint(m1)
# sl.table <-cbind(row.names(sl.table), sl.table)
# names(sl.table) <- c("Term", "B", "SE", "t", "p")
# nice_table(sl.table, highlight = TRUE)


# Spring

s0 <- glmer.nb(total_slug ~ +
                 (week|block/plot_id),
               data = spring_slugs)

s1 <- glmer.nb(total_slug ~ treatment +
                 (week|block/plot_id),
               data = spring_slugs)

s2 <- glmer.nb(total_slug ~ treatment+year+
                 (week|block/plot_id),
               data = spring_slugs)

s3 <- glmer.nb(total_slug ~ treatment+year+week+
                 (week|block/plot_id),
               data = spring_slugs)


s4 <- glmer.nb(total_slug ~ treatment*year+week+
                 (week|block/plot_id),
               data = spring_slugs)

s4.5 <- glmer.nb(total_slug ~ treatment + year * week +
                   (week|block/plot_id), data = spring_slugs) 

s5 <- glmer.nb(total_slug ~   year + treatment* week +
                 (week|block/plot_id), data = spring_slugs) 

s6 <- glmer.nb(total_slug ~ treatment*year*week +
                 (week|block/plot_id), data = spring_slugs) 
rePCA(s6)
isSingular(s6)

anova(s0, s1, s2, s3, s4, s4.5, s5, s6)
# npar     AIC     BIC  logLik deviance    Chisq Df Pr(>Chisq)    
# s0     22 1061.81 1138.39 -508.91  1017.81                           
# s1     25 1057.78 1144.79 -503.89  1007.78  10.0377  3   0.018249 * # trt  
# s2     27  856.26  950.24 -401.13   802.26 205.5147  2  < 2.2e-16 *** #yr
# s3     30  849.28  953.70 -394.64   789.28  12.9788  3   0.004683 ** #week
# s4     36  858.47  983.77 -393.23   786.47   2.8132  6   0.831903    
# s4.5   36  786.51  911.81 -357.25   714.51  71.9622  0               
# s5     39  857.92  993.67 -389.96   779.92   0.0000  3   1.000000   
# s6     69  785.36 1025.53 -323.68   647.36 132.5594 30  7.498e-15 *** #trt*yr*week

check_model(s6)
summary(s6)
hist(residuals(s6))
check_singularity(s6)
r2_nakagawa(s6) 

cld(emmeans(s6, ~year),Letters = letters)
# year emmean       SE  df asymp.LCL asymp.UCL .group
# 2022  -9.86 3503.929 Inf  -6877.43    6857.7  a    
# 2023  -9.26 2465.486 Inf  -4841.53    4823.0  a    
# 2021   1.90    0.156 Inf      1.59       2.2  a  

cld(emmeans(s6, ~treatment), Letters = letters)
# treatment emmean   SE  df asymp.LCL asymp.UCL .group
# 1          -8.18 3428 Inf     -6727      6711  a    
# 4          -5.76 2121 Inf     -4164      4152  a    
# 2          -4.55 2301 Inf     -4514      4505  a    
# 3          -4.47 3330 Inf     -6531      6522  a 

cld(emmeans(s6, ~week), Letters = letters)

cld(emmeans(s6, ~treatment|year|week), Letters = letters)


# YES in thesis models: with averaged data #####
s_test <- subset(slug_clean, season == "Spring") %>% 
  mutate_at(vars(1:6), as.factor)
s_t_m <- s_test %>% 
  group_by(year, treatment, block) %>% 
  summarise(average = mean(total_slug))

nb <- glmer.nb(average ~ treatment*year + (1|block), 
               data = s_t_m) 
summary(nb)
hist(residuals(nb))
ps <- glmer(average ~ treatment*year + (1|block), 
            data = s_t_m, family = poisson)

gs <- lmer(average ~ treatment*year + (1|block), 
           data = s_t_m)

anova(nb, ps, gs)

nb0 <- glmer.nb(average ~ + (1|block), 
                data = s_t_m) 

nb1 <- glmer.nb(average ~ treatment+ (1|block), 
                data = s_t_m) 

nb2 <- glmer.nb(average ~ treatment + year+ (1|block), 
                data = s_t_m) 

nb3 <- glmer.nb(average ~ treatment*year+ (1|block), 
                data = s_t_m) 

anova(nb0, nb1, nb2, nb3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# nb0    3 293.38 299.66 -143.69   287.38                          
# nb1    6 297.58 310.14 -142.79   285.58  1.8029  3     0.6143    
# nb2    8 227.36 244.11 -105.68   211.36 74.2179  2     <2e-16 ***
# nb3   14 235.76 265.09 -103.88   207.76  3.5931  6     0.7315 

cld(emmeans(nb3, ~treatment|year), Letters = letters)
# year = 2021:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1          1.421 0.234 Inf     0.962     1.881  a    
# 4          1.825 0.197 Inf     1.438     2.211  ab   
# 2          2.043 0.181 Inf     1.689     2.397  ab   
# 3          2.197 0.170 Inf     1.864     2.531   b   
# 
# year = 2022:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 1         -1.609 1.003 Inf    -3.576     0.357  a    
# 2         -1.386 0.898 Inf    -3.147     0.374  a    
# 4         -0.916 0.712 Inf    -2.311     0.479  a    
# 3         -0.799 0.672 Inf    -2.115     0.518  a    
# 
# year = 2023:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3          1.174 0.262 Inf     0.660     1.687  a    
# 1          1.184 0.261 Inf     0.673     1.695  a    
# 2          1.317 0.246 Inf     0.836     1.798  a    
# 4          1.386 0.238 Inf     0.920     1.853  a   


cld(emmeans(nb3, ~year), Letters = letters)
# year emmean     SE  df asymp.LCL asymp.UCL .group
# 2022  -1.18 0.4162 Inf     -1.99    -0.362  a    
# 2023   1.27 0.1259 Inf      1.02     1.512   b   
# 2021   1.87 0.0986 Inf      1.68     2.065    c  

f_test <- subset(slug_clean, season == "Fall") %>% 
  mutate_at(vars(1:6), as.factor)
f_t_m <- f_test %>% 
  group_by(year, treatment, block) %>% 
  summarise(average = mean(total_slug))

nb <- glmer.nb(average ~ treatment*year + (1|block), 
               data = f_t_m) 
summary(nb)
hist(residuals(nb))
ps <- glmer(average ~ treatment*year + (1|block), 
            data = f_t_m, family = poisson)

gs <- lmer(average ~ treatment*year + (1|block), 
           data = f_t_m)

anova(nb, ps, gs)

fnb0 <- glmer.nb(average ~ + (1|block), 
                data = f_t_m) 

fnb1 <- glmer.nb(average ~ treatment+ (1|block), 
                data = f_t_m) 

fnb2 <- glmer.nb(average ~ treatment + year+ (1|block), 
                data = f_t_m) 

fnb3 <- glmer.nb(average ~ treatment*year+ (1|block), 
                data = f_t_m) 

hist(residuals(fnb3))

anova(fnb0, fnb1, fnb2, fnb3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# fnb0    3 339.47 345.75 -166.73   333.47                          
# fnb1    6 341.56 354.13 -164.78   329.56  3.9052  3   0.271882    
# fnb2    8 278.15 294.91 -131.07   262.15 67.4119  2    2.3e-15 ***
# fnb3   14 268.54 297.86 -120.27   240.54 21.6078  6   0.001426 ** 

cld(emmeans(fnb3, ~treatment|year), Letters= letters)
# year = 2021:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3          0.210 0.386 Inf    -0.546     0.966  a    
# 4          0.427 0.345 Inf    -0.249     1.103  a    
# 2          0.550 0.340 Inf    -0.116     1.216  a    
# 1          0.916 0.249 Inf     0.428     1.404  a    
# 
# year = 2022:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 3          1.723 0.189 Inf     1.353     2.092  a    
# 4          2.028 0.162 Inf     1.711     2.345  ab   
# 2          2.544 0.125 Inf     2.298     2.789   bc  
# 1          2.696 0.115 Inf     2.470     2.922    c  
# 
# year = 2023:
#   treatment emmean    SE  df asymp.LCL asymp.UCL .group
# 2          1.317 0.231 Inf     0.864     1.770  a    
# 1          1.803 0.178 Inf     1.454     2.152  a    
# 4          1.835 0.178 Inf     1.486     2.184  a    
# 3          1.946 0.168 Inf     1.616     2.276  a  


cld(emmeans(fnb3, ~year), Letters= letters)
# year emmean     SE  df asymp.LCL asymp.UCL .group
# 2021  0.526 0.1710 Inf     0.191     0.861  a    
# 2023  1.725 0.0956 Inf     1.538     1.913   b   
# 2022  2.248 0.0754 Inf     2.100     2.395    c  


# plots corn slugs ####
# overall

slug_plot <- slug_clean %>% 
  group_by(treatment) %>% 
  summarise(mean = mean(total_slug), 
            sd =  sd(total_slug),
            n= n(), 
            se = sd/sqrt(n))

ggplot(slug_plot, aes(x = treatment, y = mean, fill = treatment))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Corn: Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023")+
#         caption = "DPP: Days pre plant
# DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# fall
fall_plot <- fall_slugs %>% 
  mutate(group = case_when(
    year == '2022' & treatment == '3' ~ 'a', 
    year == '2022' & treatment == '4' ~ 'ab', 
    year == '2022' & treatment == '2' ~ 'bc', 
    year == '2022' & treatment == '1' ~ 'c'
  ))

f.labs <- c('2021 a', '2022 b', '2023 c')
names(f.labs) <- c('2021', '2022', '2023')
ggplot(fall_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year, labeller = labeller(year = f.labs))+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Corn: Fall Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(label = group, y = 44), size = 10)


# spring 
spring_plot <- spring_slugs %>% 
  mutate(group = case_when(
    year == '2021' & treatment == '1' ~ 'a',
    year == '2021' & treatment == '4' ~ 'ab',
    year == '2021' & treatment == '2' ~ 'ab',
    year == '2021' & treatment == '3' ~ 'b'
  ))
# year emmean    SE  df asymp.LCL asymp.UCL .group
# 2022 -1.487 0.807 Inf    -3.069    0.0951  a    
# 2023 -0.118 0.675 Inf    -1.441    1.2050  ab   
# 2021  1.486 0.580 Inf     0.349    2.6223   b 
s.labs <- c('2021 a', '2022 b', '2023 ab')
names(s.labs) <- c('2021', '2022', '2023')
ggplot(spring_plot, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot(alpha = 0.7)+
  geom_point(size = 1.5)+
  facet_wrap(~year, labeller = labeller(year = s.labs))+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "Early", "Late", "Green"))+
  labs( x = 'Treatment termination',
        y = 'Average slug counts / trap', 
        title = "Corn: Spring Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 26),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  geom_text(aes(label = group, y = 44), size = 10)


# 
# ggplot(slug_clean, aes(x = treatment, y = total_slug, fill = season))+
#   geom_boxplot()+
#   facet_wrap(~year)+
#   scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
#   labs( x = 'Treatment',
#         y = 'Total Slug Counts', 
#         title = "Total Spring Slugs by Treatment")+
#   theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1),
#         axis.text.y = element_text(size = 12))
# 
# ggplot(fall_slugs, aes(x = treatment, y = total_slug, fill = year))+
#   geom_boxplot()+
#   facet_wrap(.~year)+
#   ggtitle("Total Slugs by Treatment")+
#   scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
#   ylab("Total slug counts")+
#   xlab("")+
#   theme(axis.text.x = element_text(size=12),
#         axis.text.y = element_text(size = 12))
# 
# # final figure by season and year
# slug_clean$szn <- factor(slug_clean$season, levels = c("Spring", "Fall"))
# ggplot(slug_clean, aes(x = treatment, y = total_slug, fill = treatment))+
#   geom_boxplot(alpha = 0.7)+
#   facet_wrap(year~szn, scales = "free_y", ncol = 2)+
#   scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
#   scale_x_discrete(limits = c("1", "2", "4", "3"),
#                    labels=c("No CC", "Early", "Late", "Green"))+
#   labs( x = 'Treatment termination',
#         y = 'Total Slug Counts', 
#         title = "Corn: Total Slugs by Treatment",
#         subtitle = " Years: 2021-2023")+
#   theme(legend.position = "none",
#         axis.text.x = element_text(size=18),
#         axis.text.y = element_text(size = 18),
#         strip.text = element_text(size = 16),
#         axis.title = element_text(size = 20),
#         plot.title = element_text(size = 20),
#         plot.subtitle = element_text(size = 16), 
#         panel.grid.major.y = element_line(color = "darkgrey"),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor = element_blank())
# 
# 
# 
# 
# 
# ggplotly(test)

# pub plot ####

ggplot(slug_plot, aes(x = treatment, y = mean))+
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7)+
  geom_errorbar(aes(x = treatment,ymin = mean - se, ymax = mean + se),
                color = "black", alpha = 1, width = 0.2, linewidth = 1)+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("No CC", "14-28 DPP", "3-7 DPP", "1-3 DAP"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Corn: Average Slug Counts / Trap x Treatment",
        subtitle = " Years: 2021-2023",
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
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))

# slugs and precip ####
precip_slug <- slug_clean %>% 
  group_by(season, year, treatment) %>% 
  summarise(precip_tot = sum(precip),
            slug_tot = sum(total_slug)) 

precip <- kruskal.test(precip_tot ~ year, data = precip_slug)
precip
pairwise.wilcox.test(precip_slug$precip_tot, precip_slug$year)
hist(residuals(precip))

ggplot(precip_slug, aes(x = precip_tot, y = slug_tot))+
  geom_point()+
  facet_wrap(~year + season)

ggplot(precip_slug, aes(x = year, y = precip_tot, fill = year))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~season)


