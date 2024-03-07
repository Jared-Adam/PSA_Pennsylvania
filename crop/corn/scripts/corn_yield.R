# 1/7/2024

# is there a difference in yield by year?
  # likely 2023 is much lower becuase of the drought, but let's see

# Packages ####
library(tidyverse)
library(lme4)
library(emmeans)
library(nlme)


# Data ####
yield <- PSA_PA_yield
weather <- PSA_PA_weather
cc <- PSA_PAcc_biomass

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

# yield data cleaning ####
yield

yield$plot <- gsub('-[0-9.]','', yield$plot) # remove - and all numbers following


yield_clean <- yield %>% 
  group_by(year, plot, trt) %>% 
  mutate(lb_pass_mean = mean(lb_pass_moisture), 
         lb_ac_mean = mean(lb_ac),
         bu_ac_mean = mean(bu_ac)) %>% 
  dplyr::select(-lb_pass_moisture, -lb_ac, -bu_ac) %>% 
  mutate(year = as.factor(year)) %>% 
  distinct(plot, .keep_all = TRUE) %>% 
  print(n = Inf)
as_tibble(yield_clean)

###

# df for stats 
yield_stats <- yield_clean

##

# 3/7/2024: unsure if this weather df is needed 

# going to make a new df to regress weather data by yield
yield_for_weather <- yield %>% 
  group_by(year, plot) %>% 
  dplyr::select(-lb_pass_moisture, -lb_ac) %>% 
  mutate(year = as.factor(year),
         trt = as.factor(trt)) %>% 
  print(n = Inf)

# df for plots
overall_yield <- yield_for_weather %>% 
  mutate(trt = as.factor(trt)) %>% 
  group_by(trt, year) %>%
  summarise(overall_yield_mean = mean(bu_ac), 
            yield_sd = sd(bu_ac),
            yield_se = yield_sd/sqrt(n())) %>% 
  arrange(year)

# yield stats ####
yield_stats <- yield_stats %>% 
  mutate_at(vars(1:5), as.factor)

m0 <- lme(bu_ac_mean,
          random = ~1|year/block,
          data = yield_stats)
summary(m0)

m1 <- lme(bu_ac_mean ~ trt,
          random = ~1|year/block,
          data = yield_stats)
summary(m1)





# early plots ####
yield_plot <- yield %>% 
  group_by(trt) %>%
  summarise(bu_ac_mean = mean(bu_ac),
            sd = sd(bu_ac),
            n = n(), 
            se = sd/sqrt(n))


ggplot(yield_plot, aes(x= trt, y = bu_ac_mean, fill = trt))+
  geom_bar(position = 'dodge' , stat = 'identity', alpha = .7)+
  scale_x_discrete(limits = c("Check", "Brown", "Gr-Br", "Green"),
                   labels =c("No CC", "14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#E7298A", "#D95F02",  "#7570B3","#1B9E77"))+
   geom_errorbar( aes(x=trt, ymin=bu_ac_mean-se, ymax=bu_ac_mean+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  ylab(bquote("Mean"(bu / ac ^-1)))+
  labs(x = 'Treatment',
       title = 'Corn: Yield x treatment',
       subtitle = "Years: 2021-2023",
       caption = "DPP: Days pre plant
DAP : Days after plant")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=26),
        axis.text.y = element_text(size = 26),
        axis.title = element_text(size = 32),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 24), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = 20, color = "grey25"))


### 
# by year: yield

#2021 
yield_21 <- overall_yield %>% 
  filter(year %in% '2021')

ggplot(yield_21, aes(x = trt, y = overall_yield_mean, fill = trt)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar( aes(x=trt, ymin=overall_yield_mean-yield_se, ymax=overall_yield_mean+yield_se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

#2022
yield_22 <- overall_yield %>% 
  filter(year %in% '2022')
ggplot(yield_22 , aes(x = trt, y = overall_yield_mean, fill = trt)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar( aes(x=trt, ymin=overall_yield_mean-se, ymax=overall_yield_mean+yield_se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac) by treatment",
       x = 'Treatment',
       title = 'Rough plot of yield by year and treatment with standard error bars')+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))

#2023 
yield_23 <- overall_yield %>% 
  filter(year %in% '2023')
ggplot(yield_23 , aes(x = trt, y = overall_yield_mean, fill = trt)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  geom_errorbar( aes(x=trt, ymin=overall_yield_mean-yield_se, ymax=overall_yield_mean+yield_se), width=0.4, 
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
         year = format(date, "%Y"),
         month_name = month.name[month(date)]) %>% 
  rename(avg_air = 'Avg Air Temp (?F)',
         max_temp = 'Max Air Temp (?F)', 
         min_temp = 'Min Air Temp (?F)',
         tot_precip = 'Total Precipitation') %>% 
  mutate(month = as.factor(month),
         year = as.factor(year),
         month_name = as.factor(month_name)) %>% 
  group_by(year, month) %>% 
  mutate(avg_precip = mean(tot_precip)) %>% 
  distinct(year, .keep_all = TRUE) %>% 
  select(-date, -max_temp, -min_temp) %>% 
  relocate(month_name, month, year, avg_air, avg_precip) %>% 
  arrange(year) %>% 
  print( n = Inf)

ggplot(weather_clean, aes(x = month, y = tot_precip, fill = month_name))+
  geom_bar(position = 'dodge', stat = 'identity')+
  facet_wrap(~year)

### this needs work: 1/19/2024
# dfs are not the same and are binding, but not correctly ...
# combining weather and yield 
new_df <- cbind(overall_yield, weather_clean)
weather_yield <- new_df %>% 
  rename(year = year...2) %>% 
  select(-year...8)
ggplot(filter(weather_yield, year  %in% '2022'), aes(x = overall_yield$overall_yield_mean, y = weather_clean$avg_precip, color = trt, shape = year))+
         geom_point(size = 4)
####


# cover crop biomass ####

  
cc_clean <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year, trt) %>% 
  summarise(cc_mean = mean(cc_biomass_g),
            cc_sd = sd(cc_biomass_g),
            cc_se = cc_sd/sqrt(n())) %>%
  arrange(year, factor(trt, c("check", "green", "brown", "gr-br")))

ggplot(filter(cc_clean, trt != "check"), aes(x = trt, y = cc_mean, fill = trt))+
  facet_wrap(~year)+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DPP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.75)+
  geom_errorbar( aes(x=trt, ymin=cc_mean-cc_se, ymax=cc_mean+cc_se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Average cover crop biomass by treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment",
       y = "Mean cover crop (g/m2)")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


cc_mg_ha <- cc %>% 
  mutate_at(vars(1:4), as.factor) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g))%>% 
  group_by(year, trt, plot) %>% 
  summarise(mean_cc = mean(cc_biomass_g)) %>% 
  mutate(mg_ha = mean_cc*0.04) %>% 
  group_by(year, trt) %>% 
  summarise(mean_mg = mean(mg_ha),
            sd = sd(mg_ha), 
            n = n(), 
            se = sd/sqrt(n))

ggplot(filter(cc_mg_ha, trt != "check"), aes(x = trt, y = mean_mg, fill = trt))+
  facet_wrap(~year)+
  scale_x_discrete(labels = c("14-21 DPP", "3-7 DPP", "1-3 DAP"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7)+
  geom_errorbar( aes(x=trt, ymin=mean_mg-se, ymax=mean_mg+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Corn: Average cover crop biomass by treatment",
       subtitle = "Years: 2021-2023",
       x = "Treatment")+
  ylab(bquote("Mean cover crop" (Mg / ha ^-1)))+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16), 
        panel.grid.major.y = element_line(color = "darkgrey"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank())





# not enough df for this anova or lm p values 
# will need to do a t test
cc_20 <- cc %>% 
  mutate(plot = as.factor(plot)) %>% 
  mutate(cc_biomass_g = as.numeric(cc_biomass_g)) %>% 
  group_by(year,plot, trt) %>% 
  summarise(cc_sum = sum(cc_biomass_g)) %>% 
  print(n = Inf)

cc_aov <- filter(cc_20, trt != "check")
cc_21 <- filter(cc_aov, year == "2021")

aov_cc21 <- aov(cc_sum ~ trt, cc_21)
summary(aov_cc21)
TukeyHSD(aov_cc21)

cc_22 <- filter(cc_aov, year == "2022")

aov_cc22 <- aov(cc_sum ~ trt, cc_22)
summary(aov_cc22)
TukeyHSD(aov_cc22)

cc_23 <- filter(cc_aov, year == "2023")

aov_cc23 <- aov(cc_sum ~ trt, cc_23)
summary(aov_cc23)
TukeyHSD(aov_cc23)

cc_aov$year <- as.factor(cc_aov$year)
aov_cc20 <- aov(cc_sum ~ year, cc_aov)
TukeyHSD(aov_cc20)

# add cc to weather and yield df
cc_bind <- cc_clean 
cc_yield <- cbind(cc_bind, overall_yield)
cc_yield <- cc_yield %>% 
  dplyr::select(-'year...7') %>% 
  rename(year = year...1) %>% 
  dplyr::select(-trt...2) %>% 
  rename(trt = trt...6) %>% 
  relocate(year, trt)

ggplot(filter(cc_yield, trt != 'Check'), aes(x = overall_yield_mean, y = cc_mean, shape = trt, color = trt))+
  geom_point(stat = 'identity', position = 'identity')+
  facet_wrap(~year)

###

# Anova ####
anova_one <- aov(bu_ac_mean ~ trt, yield_clean)
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
