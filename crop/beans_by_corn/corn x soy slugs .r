# Jared Adam 
# to evalute the differnces in slugs between corn and soybeans 

# packages ####
library(tidyverse)
library(vegan)
library(vegan3d)
library(ggpubr)
library(RColorBrewer)
library(MASS)
library(emmeans)
library(ggpmisc)
library(lmtest)

# data ####
corn_slug <- PSA_PA_slugs
bean_slug <- slugs_beans_all

# slug wrangling ####
# KARN SLOOOOOG # 
cs <- corn_slug %>% 
  dplyr::select(-location, -shingle_id, -time, -row, -temp) %>% 
  rename(plot = plot_id) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
         treatment = as.factor(treatment),
         block = as.factor(block))%>% 
  group_by(season, year, month, plot, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  replace(is.na(.),0) %>% 
  mutate(crop = "corn") %>% 
  mutate(crop = as.factor(crop)) %>% 
  print(n = Inf)


# BEAN SLOOOOOG #
bs <- bean_slug %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  mutate(block = as.factor(block)) %>%
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y'))  %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
         treatment = as.factor(treatment))%>%
  rename(season = seaon) %>% 
  group_by(season, year, month, plot, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)
bs <- bs[1:160,]
bs <- bs %>% 
  replace(is.na(.),0) %>% 
  mutate(crop = "bean") %>% 
  mutate(crop = as.factor(crop)) %>% 
  print(n = Inf)



# corn 21 beans 22
cs21 <- cs %>% 
  filter(year == '2021') %>% 
  mutate(crop = 'corn',
         crop = as.factor(crop),
         season = as.factor(season)) %>% 
  print(n = Inf)


bs22 <- bs %>% 
  filter(year == '2022') %>% 
  mutate(crop = 'beans', 
         crop = as.factor(crop),
         season = as.factor(season)) %>% 
  print(n = Inf)


cbs2122 <- rbind(cs21, bs22)


# corn 22 beans 23
cs22 <- cs %>% 
  filter(year == '2022') %>% 
  mutate(crop = 'corn',
         crop = as.factor(crop),
         season = as.factor(season)) %>% 
  print(n = Inf)

bs23 <- bs %>% 
  filter(year == '2023') %>% 
  mutate(crop = 'beans', 
         crop = as.factor(crop),
         season = as.factor(season)) %>% 
  print(n = Inf)

cbs2223 <- rbind(cs22, bs23)


# stats ####
cbs2122

# dist check 
sp <- glm(total_slug ~ treatment*crop,
            data = cbs2122, 
            family = poisson)
nb <- glm.nb(total_slug ~ treatment*crop ,
               data = cbs2122)
lrtest(sp, nb)


m1 <- glm.nb(total_slug ~ treatment, 
               data = cbs2122)

m2 <- glm.nb(total_slug ~ treatment + crop, 
               data = cbs2122)

m3 <- glm.nb(total_slug ~ treatment*crop,
               data = cbs2122)

anova(m1,m2,m3)
hist(residuals(m3))
check_model(m3)
# r2_nakagawa(m3)
performance::check_singularity(m3)

cbs1 <- emmeans(m3, ~treatment + crop)
cld(cbs1, Letters = letters)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 4         beans  0.903 0.392 Inf     0.135      1.67  a    
# 3         beans  1.142 0.384 Inf     0.389      1.90  ab   
# 2         beans  1.419 0.378 Inf     0.679      2.16  abc  
# 1         beans  1.887 0.370 Inf     1.163      2.61  abc  
# 1         corn   2.175 0.284 Inf     1.619      2.73  abc  
# 4         corn   2.353 0.282 Inf     1.800      2.91  abc  
# 2         corn   2.556 0.281 Inf     2.005      3.11   bc  
# 3         corn   2.645 0.281 Inf     2.095      3.19    c  

cbs1_df <- as.data.frame(cld(cbs1, Letters = letters))

####
###
##
#


cbs2223

# dist check 
sp <- glm(total_slug ~ treatment*crop,
            data = cbs2223, 
            family = poisson)
nb <- glm.nb(total_slug ~ treatment*crop,
               data = cbs2223)
lrtest(sp, nb)


m1 <- glm.nb(total_slug ~ treatment , 
               data = cbs2223)

m2 <- glm.nb(total_slug ~ treatment + crop, 
               data = cbs2223)

m3 <- glm.nb(total_slug ~ treatment*crop , 
               data = cbs2223)

anova(m1,m2,m3)

check_model(m3)
hist(residuals(m3))
cbs2 <- emmeans(m3, ~treatment + crop)
cld(cbs2, Letters = letters)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 3         beans   2.02 0.278 Inf      1.47      2.56  a    
# 2         beans   2.20 0.276 Inf      1.66      2.74  ab   
# 1         beans   2.24 0.276 Inf      1.70      2.78  ab   
# 4         beans   2.27 0.276 Inf      1.73      2.81  ab   
# 3         corn    2.46 0.307 Inf      1.85      3.06  ab   
# 4         corn    2.75 0.305 Inf      2.15      3.34  ab   
# 2         corn    3.25 0.303 Inf      2.65      3.84  ab   
# 1         corn    3.40 0.302 Inf      2.80      3.99   b  

cbs2_df <- as.data.frame(cld(cbs2, Letters = letters))


# plots ####
ggplot(cbs1_df, aes())

