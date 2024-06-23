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


# stats 21-22 ####
cbs2122

# spring
s_cbs2122 <- cbs2122 %>% 
  subset(season == 'spring') %>% 
  mutate(plot = as.factor(plot)) %>% 
  group_by(crop, treatment, plot, block) %>% 
  summarise(average = mean(total_slug))

# dist check 
sp <- glmer(average ~ treatment*crop+
              (crop|block),
            data = s_cbs2122, 
            family = poisson)
nb <- glmer.nb(average ~ treatment*crop +
                 (crop|block),
               data = s_cbs2122)
lrtest(sp, nb)

m0<- glmer.nb(average ~ +
                 (crop|block),
             data = s_cbs2122)
m1 <- glmer.nb(average ~ treatment +
                 (crop|block), 
               data = s_cbs2122)

m2 <- glmer.nb(average ~ treatment + crop+
                 (crop|block), 
               data = s_cbs2122)

m3 <- glmer.nb(average ~ treatment*crop+
                 (crop|block),
               data = s_cbs2122)

anova(m0,m1,m2,m3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# m0    5 169.44 177.88 -79.718   159.44                          
# m1    8 154.74 168.25 -69.371   138.74 20.6957  3  0.0001218 ***
# m2    9 146.98 162.18 -64.488   128.98  9.7653  1  0.0017784 ** 
# m3   12 149.48 169.75 -62.740   125.48  3.4963  3  0.3212410  
hist(residuals(m3))
check_model(m3)
# r2_nakagawa(m3)
check_singularity(m3)

cld(emmeans(m3, ~treatment*crop),Letters = letters)

# 2         beans -18.87 6.808 Inf    -32.21  -5.52912  a    
# 1         beans  -2.65 1.125 Inf     -4.86  -0.44813  a    
# 3         beans  -2.65 1.351 Inf     -5.30  -0.00383  a    
# 4         beans  -1.96 1.209 Inf     -4.33   0.40986  a    
# 1         corn    2.25 0.161 Inf      1.94   2.56996   b   
# 4         corn    2.66 0.139 Inf      2.39   2.92898   bc  
# 2         corn    2.88 0.128 Inf      2.62   3.12774    c  
# 3         corn    3.03 0.122 Inf      2.79   3.26966    c 

cld(emmeans(m3, ~crop),Letters = letters)
# crop  emmean     SE  df asymp.LCL asymp.UCL .group
# beans  -6.53 2.0512 Inf    -10.55     -2.51  a    
# corn    2.70 0.0947 Inf      2.52      2.89   b 


f_cbs2122 <- cbs2122 %>% 
  subset(season == 'fall') %>% 
  group_by(crop, treatment, block) %>% 
  summarise(average = mean(total_slug))

# dist check 
sp <- glmer(average ~ treatment*crop+
              (crop|block),
            data = f_cbs2122, 
            family = poisson)
nb <- glmer.nb(average ~ treatment*crop +
                 (crop|block),
               data = f_cbs2122)
lrtest(sp, nb)

fm0<- glmer.nb(average ~ +
                (crop|block),
              data = f_cbs2122)
fm1 <- glmer.nb(average ~ treatment +
                 (crop|block), 
               data = f_cbs2122)

fm2 <- glmer.nb(average ~ treatment + crop+
                 (crop|block), 
               data = f_cbs2122)

fm3 <- glmer.nb(average ~ treatment*crop+
                 (crop|block),
               data = f_cbs2122)

anova(fm0,fm1,fm2,fm3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# fm0    5 206.10 214.54 -98.050   196.10                          
# fm1    8 194.53 208.04 -89.264   178.53 17.5709  3  0.0005392 ***
# fm2    9 196.07 211.28 -89.038   178.07  0.4533  1  0.5007557    
# fm3   12 199.93 220.20 -87.965   175.93  2.1444  3  0.5429730    

summary(fm3)
hist(residuals(fm3))

cld(emmeans(fm3, ~treatment*crop), Letters = letters)
# treatment crop  emmean     SE  df asymp.LCL asymp.UCL .group
# 4         beans -0.224 0.4660 Inf   -1.1377     0.689  a    
# 3         beans -0.224 0.4730 Inf   -1.1514     0.703  a    
# 2         beans  0.181 0.3911 Inf   -0.5854     0.948  a    
# 1         beans  0.641 0.2987 Inf    0.0553     1.226  a    
# 3         corn   2.700 0.1213 Inf    2.4619     2.938   b   
# 4         corn   3.005 0.1067 Inf    2.7959     3.214   b   
# 2         corn   3.521 0.0862 Inf    3.3516     3.689    c  
# 1         corn   3.673 0.0806 Inf    3.5152     3.831    c 
# 

####
###
##
#

# stats 22-23 ####
cbs2223
s_cbs2223 <- cbs2223 %>% 
  subset(season == 'spring') %>% 
  group_by(crop, treatment, block) %>% 
  summarise(average = mean(total_slug))

# dist check 
sp <- glmer(average ~ treatment*crop+
              (crop|block),
            data = s_cbs2223, 
            family = poisson)
nb <- glmer.nb(average ~ treatment*crop +
                 (crop|block),
               data = s_cbs2223)
lrtest(sp, nb)

sm0<- glmer.nb(average ~ +
                (crop|block),
              data = s_cbs2223)
sm1 <- glmer.nb(average ~ treatment +
                 (crop|block), 
               data = s_cbs2223)

sm2 <- glmer.nb(average ~ treatment + crop+
                 (crop|block), 
               data = s_cbs2223)

sm3 <- glmer.nb(average ~ treatment*crop+
                 (crop|block),
               data = s_cbs2223)
anova(sm0, sm1,sm2,sm3)
# npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)   
# sm0    5 194.93 203.37 -92.463   184.93                         
# sm1    8 199.10 212.61 -91.550   183.10  1.8255  3   0.609402   
# sm2    9 190.57 205.77 -86.286   172.57 10.5287  1   0.001175 **
# sm3   12 193.35 213.62 -84.675   169.35  3.2220  3   0.358649  

check_model(sm3)
hist(residuals(sm3))
cld(emmeans(sm3, ~crop), Letters = letters)
# crop  emmean    SE  df asymp.LCL asymp.UCL .group
# corn   -0.16 0.461 Inf     -1.06     0.743  a    
# beans   2.58 0.157 Inf      2.28     2.891   b 

cld(emmeans(sm3, ~treatment*crop), Letters = letters)
# treatment crop  emmean    SE  df asymp.LCL asymp.UCL .group
# 1         corn  -0.592 0.528 Inf    -1.627     0.443  a    
# 2         corn  -0.369 0.606 Inf    -1.556     0.818  a    
# 4         corn   0.101 0.551 Inf    -0.979     1.181  a    
# 3         corn   0.219 0.535 Inf    -0.829     1.267  a    
# 3         beans  2.431 0.194 Inf     2.050     2.811   b   
# 2         beans  2.604 0.187 Inf     2.237     2.970   b   
# 1         beans  2.608 0.187 Inf     2.242     2.975   b   
# 4         beans  2.689 0.184 Inf     2.328     3.050   b 

cbs2223
f_cbs2223 <- cbs2223 %>% 
  subset(season == 'fall') %>% 
  group_by(crop, treatment, block) %>% 
  summarise(average = mean(total_slug))

# dist check 
sp <- glmer(average ~ treatment*crop+
              (crop|block),
            data = f_cbs2223, 
            family = poisson)
nb <- glmer.nb(average ~ treatment*crop +
                 (crop|block),
               data = f_cbs2223)
lrtest(sp, nb)

fm0<- glmer.nb(average ~ +
                 (crop|block),
               data = f_cbs2223)
fm1 <- glmer.nb(average ~ treatment +
                  (crop|block), 
                data = f_cbs2223)

fm2 <- glmer.nb(average ~ treatment + crop+
                  (crop|block), 
                data = f_cbs2223)

fm3 <- glmer.nb(average ~ treatment*crop+
                  (crop|block),
                data = f_cbs2223)
anova(fm0, fm1,fm2,fm3)
# npar    AIC    BIC   logLik deviance   Chisq Df Pr(>Chisq)    
# fm0    5 235.22 243.66 -112.608   225.22                          
# fm1    8 205.05 218.56  -94.526   189.05 36.1643  3  6.913e-08 ***
# fm2    9 186.99 202.19  -84.497   168.99 20.0587  1  7.510e-06 ***
# fm3   12 192.45 212.72  -84.225   168.45  0.5442  3     0.9091 
hist(residuals(fm3))
summary(fm3)

cld(emmeans(fm3, ~treatment*crop), Letters = letters)
# treatment crop  emmean     SE  df asymp.LCL asymp.UCL .group
# 4         beans -0.224 0.4660 Inf   -1.1377     0.689  a    
# 3         beans -0.224 0.4730 Inf   -1.1514     0.703  a    
# 2         beans  0.181 0.3911 Inf   -0.5854     0.948  a    
# 1         beans  0.641 0.2987 Inf    0.0553     1.226  a    
# 3         corn   2.700 0.1213 Inf    2.4619     2.938   b   
# 4         corn   3.005 0.1067 Inf    2.7959     3.214   b   
# 2         corn   3.521 0.0862 Inf    3.3516     3.689    c  
# 1         corn   3.673 0.0806 Inf    3.5152     3.831    c  

cld(emmeans(fm3, ~crop), Letters = letters)
# crop  emmean     SE  df asymp.LCL asymp.UCL .group
# beans 0.0933 0.2159 Inf     -0.33     0.516  a    
# corn  3.2246 0.0609 Inf      3.11     3.344   b  
# 


# plots ####

ggplot(s_cbs2122, aes(treatment, average, fill = crop))+
  geom_boxplot()+
  scale_x_discrete(limits = c('1', '2', '4', '3'),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#D95F02","#1B9E77"), 
                    name = "Crop", labels = c("Corn", 'Soybean'))+
  geom_point(position = position_dodge(width = 0.75))+
  labs(title = "Spring Slug Counts x Treatment and Crop",
       subtitle = "Years: 2021 Corn - 2022 Soybeans",
       x = "Treatment termination", 
       y = "Slug count"
#        caption = "DPP: Days pre plant
# DAP: Days after plant"
       )+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())+
  annotate('text', x =.815, y = 32, label = 'b', size = 10)+ 
  annotate('text', x =1.19, y = 32, label = 'a', size = 10)+ 
  annotate('text', x =1.815, y = 32, label = 'c', size = 10)+ 
  annotate('text', x =2.19, y = 32, label = 'a', size = 10)+ 
  annotate('text', x =2.815, y = 32, label = 'bc', size = 10)+ 
  annotate('text', x =3.19, y = 32, label = 'a', size = 10)+ 
  annotate('text', x =3.815, y = 32, label = 'c', size = 10)+
  annotate('text', x =4.19, y = 32, label = 'a', size = 10)

ggplot(f_cbs2122, aes(treatment, average, fill = crop))+
  geom_boxplot()+
  scale_x_discrete(limits = c('1', '2', '4', '3'),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#D95F02","#1B9E77"), 
                    name = "Crop", labels = c("Corn", 'Soybean'))+
  geom_point(position = position_dodge(width = 0.75))+
  labs(title = "Spring Slug Counts x Treatment and Crop",
       subtitle = "Years: 2021 Corn - 2022 Soybeans",
       x = "Treatment termination", 
       y = "Slug count"
       #        caption = "DPP: Days pre plant
       # DAP: Days after plant"
  )+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x =.815, y = 17, label = 'c', size = 10)+ 
  annotate('text', x =1.19, y = 17, label = 'a', size = 10)+ 
  annotate('text', x =1.815, y = 17, label = 'c', size = 10)+ 
  annotate('text', x =2.19, y = 17, label = 'a', size = 10)+ 
  annotate('text', x =2.815, y = 17, label = 'b', size = 10)+ 
  annotate('text', x =3.19, y = 17, label = 'a', size = 10)+ 
  annotate('text', x =3.815, y = 17, label = 'b', size = 10)+
  annotate('text', x =4.19, y = 17, label = 'a', size = 10)



# 22 - 23 
# spring 
ggplot(s_cbs2223, aes(treatment,average, fill = crop))+
  geom_boxplot()+
  scale_x_discrete(limits = c('1', '2', '4', '3'),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#D95F02","#1B9E77"), 
                    name = "Crop", labels = c("Corn", 'Soybean'))+
  geom_point(position = position_dodge(width = 0.75))+
  labs(title = "Slug Counts x Treatment and Crop",
       subtitle = "Years: 2022 Corn - 2023 Soybeans",
       x = "Treatment termination", 
       y = "Slug count"
#        caption = "DPP: Days pre plant
# DAP: Days after plant"
       )+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x =.815, y = 30, label = 'a', size = 10)+ 
  annotate('text', x =1.19, y = 30, label = 'b', size = 10)+ 
  annotate('text', x =1.815, y = 30, label = 'a', size = 10)+ 
  annotate('text', x =2.19, y = 30, label = 'b', size = 10)+ 
  annotate('text', x =2.815, y = 30, label = 'a', size = 10)+ 
  annotate('text', x =3.19, y = 30, label = 'b', size = 10)+ 
  annotate('text', x =3.815, y = 30, label = 'a', size = 10)+
  annotate('text', x =4.19, y = 30, label = 'b', size = 10)

# fall
ggplot(f_cbs2223, aes(treatment, average, fill = crop))+
  geom_boxplot()+
  scale_x_discrete(limits = c('1', '2', '4', '3'),
                   labels = c('No CC', 'Early', 'Late', 'Green'))+
  scale_fill_manual(values = c("#D95F02","#1B9E77"), 
                    name = "Crop", labels = c("Corn", 'Soybean'))+
  geom_point(position = position_dodge(width = 0.75))+
  labs(title = " Fall Slug Counts x Treatment and Crop",
       subtitle = "Years: 2022 Corn - 2023 Soybeans",
       x = "Treatment termination", 
       y = "Slug count"
       #        caption = "DPP: Days pre plant
       # DAP: Days after plant"
  )+
  theme(legend.position = "bottom",
        legend.key.size = unit(.50, 'cm'),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        axis.text.x = element_text(size=32),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 28),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))+
  annotate('text', x =.815, y = 51, label = 'c', size = 10)+ 
  annotate('text', x =1.19, y = 51, label = 'a', size = 10)+ 
  annotate('text', x =1.815, y = 51, label = 'c', size = 10)+ 
  annotate('text', x =2.19, y = 51, label = 'a', size = 10)+ 
  annotate('text', x =2.815, y = 51, label = 'b', size = 10)+ 
  annotate('text', x =3.19, y = 51, label = 'a', size = 10)+ 
  annotate('text', x =3.815, y = 51, label = 'b', size = 10)+
  annotate('text', x =4.19, y = 51, label = 'a', size = 10)
