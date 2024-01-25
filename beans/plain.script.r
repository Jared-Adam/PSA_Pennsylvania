# Jared Adam
# 1/25/2024
# Plain script
# going to work through as much bean stuff as I can
# this script will be a bit unorganized 


# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)
library(lmtest)


# 2022 data ####
# 2022: did this in a hurry but then I ended up paying for the wifi anywho
# bean scripts 
one <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentintelPrey.ALLtimes.2022.csv")
two <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/SentPreyandPF.Beans.2022.csv")
three <- read.csv("https://raw.githubusercontent.com/Jared-Adam-PSU/535/main/Pitfalls.ALLtimings.2022.csv")

sent_beans <- one

pf <- two

pf_raw <- three


# SLUGS 2022 and 2023 data ####
slugs = slugs_beans_all %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = as.factor(block)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
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
slugs <- slugs[1:160,]
unique(slugs$treatment)


#subset by season

fall_slugs <- subset(slugs, season == "fall")
spring_slugs <- subset(slugs, season == "spring")

# models ####

# look at overdispersion: variance > mean?
dispersion_stats <- slugs %>% 
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



# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 
poisson_model <- glmer(total_slug ~ treatment + 
                         (1|year/block), 
                       data = slugs, 
                       family = poisson)

nb_model_trt <- glmer.nb(total_slug ~ treatment + 
                           (1|year/block), 
                         data = slugs) 

lrtest(poisson_model,nb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that

#actual model: TBD


# plots ###
ggplot(slugs, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~year + season)+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Total Spring Slugs by Treatment")+
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))



