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
  mutate(plot = as.factor(plot)) %>% 
  mutate(trt = as.factor(trt)) %>% 
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
  arrange(year,factor(trt, c("check", "green", "brown", "gr-br")))

# cc data: to add to over_yield ####
# this is for the binding process
overall_yield <- overall_yield %>% 
  ungroup()
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
  arrange(year, factor(trt, c("check", "gr", "br", "grbr")))

cc_to_bind <- cc_clean %>% 
  select(-year, -trt)

cc_bind <- cbind(cc_to_bind, overall_yield)
cc_bind <- as_tibble(cc_bind)

cc_yield <- cc_bind %>% 
  relocate(trt, year)
  



# visuals ####

# yield 

ggplot(overall_yield, aes(x= trt, y = overall_yield_mean, fill = trt))+
  geom_bar(position = 'dodge' , stat = 'identity')+
  scale_x_discrete(limits = c("Check", "Brown", "Gr-Br", "Green"),
                   labels = c("Check", "Brown", "GrBr", "Green"))+
  scale_fill_manual(values = c("#E7298A", "#1B9E77","#D95F02",  "#7570B3"))+
  facet_wrap(~year)+
   geom_errorbar( aes(x=trt, ymin=overall_yield_mean-yield_se, ymax=overall_yield_mean+yield_se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(y = "Mean (bu/ac)",
       x = 'Treatment',
       title = 'Soybean: Yield by treatment',
       subtitle = "Years: 2022-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  # theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 12))
# green outcompeted other treatments in both 2022 and 2023
  # this is important!


# cc
beans_cc <- ggplot(filter(cc_clean, trt != 'check'), aes(x = trt, y = cc_mean, fill = trt))+
  facet_wrap(~year)+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar( aes(x=trt, ymin=cc_mean-cc_se, ymax=cc_mean+cc_se), width=0.4, 
                 colour="orange", alpha=0.9, size=1.3)

# cc x yield
beans_cc_x_yield <- ggplot(filter(cc_yield, trt != 'Check'), aes(x = overall_yield_mean, y = cc_mean, shape = trt, color = trt))+
  geom_point(stat = 'identity', position = 'identity', size = 8)+
  facet_wrap(~year)+
  scale_color_manual(values = c("Green" = 'green', "Brown" = 'brown', "Gr-Br" = "orange"))

# Anovas of yield ####
an_b1 <- aov(bu_ac_mean ~ trt, yield_clean)
summary(an_b1)
plot(residuals(an_b1))
hist(residuals(an_b1))
TukeyHSD(an_b1)

# sig by year, not by trt = GOOD

an_b2 <- aov(bu_ac_mean ~ year, yield_clean)
summary(an_b2)
TukeyHSD(an_b2)

# cc ####

cc_b <- cc %>% 
  mutate_at(vars(1:3), as.factor) %>% 
  group_by(year, plot, trt) %>% 
  summarise(sum = sum(cc_g))

cc_b_mean <- cc_b %>% 
  ungroup() %>% 
  group_by(trt, year) %>% 
  summarise(mean = mean(sum), 
            sd = sd(sum), 
            n = n(), 
            se = sd/sqrt(n))

ggplot(cc_b_mean, aes(x = trt, y = mean, fill = trt))+
  facet_wrap(~year)+
  scale_x_discrete(labels = c("Brown", "GrBr", "Green"))+
  scale_fill_manual(values = c("#D95F02",  "#7570B3","#1B9E77"))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_errorbar( aes(x=trt, ymin=mean-se, ymax=mean+se), width=0.4, 
                 colour="black", alpha=0.9, size=1.3)+
  labs(title = "Soybean: Average cover crop biomass by treatment",
       subtitle = "Years: 2022-2023",
       x = "Treatment",
       y = "Mean cover crop (g/m2)")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




cc_b_22 <- filter(cc_b, year == "2022")
cc_b_aov_22 <- aov(sum ~ trt, cc_b_22)
TukeyHSD(cc_b_aov_22)

cc_b_23 <- filter(cc_b, year == "2023")
cc_b_aov_23 <- aov(sum ~ trt, cc_b_23)
TukeyHSD(cc_b_aov_23)

cc_b_aov <- aov(sum ~ year, cc_b)
TukeyHSD(cc_b_aov)
