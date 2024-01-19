# 1/7/2024

# is there a difference in yield by year?
  # likely 2023 is much lower becuase of the drought, but let's see

# Packages ####
library(tidyverse)

# Data ####
yield <- PSA_PA_yield
weather <- PSA_PA_weather

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
  summarise(overall_mean = mean(bu_ac), 
            sd = sd(bu_ac),
            se = sd/sqrt(n())) 

# %>%
#   dplyr::select(-crop, -trt_num, -block, -bu_ac, -plot) %>% 
#   distinct(trt, .keep_all = TRUE) 
#   

?geom_bar
ggplot(overall_yield, aes(x= trt, y = overall_mean, fill = trt))+
  geom_bar(position = 'dodge' , stat = 'identity')+
  facet_wrap(~year)+
   geom_errorbar( aes(x=trt, ymin=overall_mean-se, ymax=overall_mean+se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
# green outcompeted other treatments in both 2022 and 2023
  # this is important!

### 
# by year: yield

#2021 
yield_21 <- overall_yield %>% 
  filter(year %in% '2021')

ggplot(yield_21, aes(x = trt, y = overall_mean, fill = trt)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar( aes(x=trt, ymin=overall_mean-se, ymax=overall_mean+se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

#2022
yield_22 <- overall_yield %>% 
  filter(year %in% '2022')
ggplot(yield_22 , aes(x = trt, y = overall_mean, fill = trt)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar( aes(x=trt, ymin=overall_mean-se, ymax=overall_mean+se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

#2023 
yield_23 <- overall_yield %>% 
  filter(year %in% '2023')
ggplot(yield_23 , aes(x = trt, y = overall_mean, fill = trt)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar( aes(x=trt, ymin=overall_mean-se, ymax=overall_mean+se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))



# now going to add weather data
?lubridate
weather_clean <- weather %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  mutate(month = format(date, "%m"),
         year = format(date, "%y"),
         month_name = month.name[month(date)]) %>% 
  rename(avg_air = 'Avg Air Temp (?F)',
         max_temp = 'Max Air Temp (?F)', 
         min_temp = 'Min Air Temp (?F)',
         tot_precip = 'Total Precipitation') %>% 
  mutate(month = as.factor(month),
         year = as.factor(year),
         month_name = as.factor(month_name)) %>% 
  group_by(month, year) %>% 
  mutate(avg_precip = mean(tot_precip)) %>% 
  distinct(year, .keep_all = TRUE) %>% 
  select(-date, -max_temp, -min_temp) %>% 
  relocate(month_name, month, year, avg_air, avg_precip) %>% 
  arrange(year) %>% 
  print( n = Inf)

ggplot(weather_clean, aes(x = year, y = tot_precip, fill = month_name))+
  geom_bar(position = 'dodge', stat = 'identity')

# combining weather and yield 
new_df <- cbind(overall_yield, weather_clean)
weather_yield <- new_df %>% 
  rename(year = year...1) %>% 
  select(-year...6)
ggplot(weather_yield, aes(x = avg_precip, y = overall_mean, color = trt, shape = year))+
         geom_point(size = 4)

###

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

# yield ~ precip
anova_three <- aov(overall_mean ~ avg_precip, weather_yield)
summary(anova_three)
hist(residuals(anova_three))
shapiro.test(residuals(anova_three))
