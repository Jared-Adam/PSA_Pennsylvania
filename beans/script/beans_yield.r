# Jared Adam  
# soybean yield 2022 and 2023

# packages ####
library(tidyverse)

# data ####
beans_yield <- PA_PSA_beans_yield_all
weather <- PSA_PA_weather
cc <- beans_cc_biomasss

# wrangling ####
beans_yield$plot <- gsub('-[0-9.]','', beans_yield$plot) # remove - and all numbers following


yield_clean <- beans_yield %>% 
  group_by(year, plot) %>% 
  mutate(lb_pass_mean = mean(lb_pass_moisture), 
         lb_ac_mean = mean(lb_ac),
         bu_ac_mean = mean(bu_ac)) %>% 
  dplyr::select(-lb_pass_moisture, -lb_ac, -bu_ac) %>% 
  mutate(year = as.factor(year)) %>% 
  distinct(plot, .keep_all = TRUE) %>% 
  print(n = Inf)
yield_clean <- yield_clean[1:40,]


###

# going to make a new df to regress weather data by yield
yield_for_weather <- beans_yield %>% 
  group_by(year, plot) %>% 
  dplyr::select(-lb_pass_moisture, -lb_ac) %>% 
  mutate(year = as.factor(year),
         trt = as.factor(trt)) %>% 
  print(n = Inf)
yield_for_weather <- yield_for_weather[1:80,]

# no plot here, just year and trt
overall_yield <- yield_for_weather %>% 
  mutate(trt = as.factor(trt)) %>% 
  group_by(trt, year) %>%
  summarise(overall_yield_mean = mean(bu_ac), 
            yield_sd = sd(bu_ac),
            yield_se = yield_sd/sqrt(n())) %>% 
  arrange(year)

# cc data: to add to over_yield ####
overall_yield
cc
cc_start <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_g),
            cc_sd = sd(cc_g),
            cc_se = cc_sd/sqrt(n())) 
new_checks <- as_tibble(year = c('2022','2023'),
                         trt = c('check','check'),
                         cc_mean = c('NA','NA'),
                         cc_sd = c('NA', 'NA'),
                         cc_se = c('NA','NA'))
                         
new_cc <- rbind(as.data.frame(cc_start), new_checks)

new_cc <- as_tibble(new_cc)
cc_clean <- new_cc %>% 
  mutate_at(vars(1:2), as.factor)%>%
  mutate_at(vars(3:5), as.numeric) %>% 
  arrange(year, factor(trt, c("check", "green", "brown", "gr-br")))



# visuals ####


ggplot(overall_yield, aes(x= trt, y = overall_yield_mean, fill = trt))+
  geom_bar(position = 'dodge' , stat = 'identity')+
  facet_wrap(~year)+
   geom_errorbar( aes(x=trt, ymin=overall_yield_mean-yield_se, ymax=overall_yield_mean+yield_se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
# green outcompeted other treatments in both 2022 and 2023
  # this is important!