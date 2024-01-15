# 1/7/2024

# is there a difference in yield by year?
  # likely 2023 is much lower becuase of the drought, but let's see

# Packages ####
library(tidyverse)

# Data ####
yield <- PSA_PA_yield

# Test of averaging every two rows ####

test <- yield[1:40,]
?gsub
test$plot <- gsub('-[0-9.]','', test$plot)

#obtaining mean values 
test_mean <- test %>% 
  group_by(year, plot) %>% 
  mutate(lb_pass_mean = mean(lb_pass_moisture), 
         lb_ac_mean = mean(lb_ac),
         bu_ac_mean = mean(bu_ac)) %>% 
  select(-lb_pass_moisture, -lb_ac, -bu_ac)
  
# removing duplicates: base
duplicated(test_mean)
x <- test_mean[!duplicated(test_mean$plot),]

# removing duplicates:dplyr
#I like this more 
test_mean %>% 
  group_by(year) %>% 
  distinct(plot, .keep_all = TRUE) %>% 
  print(n = Inf)

# all data cleaning ####
yield

yield$plot <- gsub('-[0-9.]','', yield$plot) # remove - and all numbers following


yield_clean <- yield %>% 
  group_by(year, plot) %>% 
  mutate(lb_pass_mean = mean(lb_pass_moisture), 
         lb_ac_mean = mean(lb_ac),
         bu_ac_mean = mean(bu_ac)) %>% 
  select(-lb_pass_moisture, -lb_ac, -bu_ac) %>% 
  mutate(year = as.factor(year)) %>% 
  distinct(plot, .keep_all = TRUE) %>% 
  print(n = Inf)
as_tibble(yield_clean)

###

# going to make a new df to regress weather data by yield
yield_for_weather <- yield %>% 
  group_by(year, plot) %>% 
  select(-lb_pass_moisture, -lb_ac) %>% 
  mutate(year = as.factor(year),
         trt = as.factor(trt)) %>% 
  print(n = Inf)

overall_yield <- yield_for_weather %>% 
  mutate(trt = as.factor(trt)) %>% 
  group_by(trt, year) %>%
  mutate(overall_mean = mean(bu_ac)) %>%
  select(-crop, -trt_num, -block, -bu_ac, -plot) %>% 
  distinct(trt, .keep_all = TRUE) %>% 
  print(n = 20)

?geom_bar
ggplot(overall_yield, aes(x= year, y = overall_mean, fill = trt))+
  geom_bar(position = 'dodge' , stat = 'identity')

# now going to add weather data 

# Anova ####
anova_one <- aov(bu_ac_mean ~ trt + year, yield_clean)
summary(anova_one)
plot(residuals(anova_one))
hist(residuals(anova_one))
TukeyHSD(anova_one)

anova_two <- aov(bu_ac_mean ~ year, yield_clean)
summary(anova_two)
hist(residuals(anova_two))
TukeyHSD(anova_two)

ggplot(yield_clean, aes(x = year, y = bu_ac_mean, fill = trt))+
  geom_boxplot()

