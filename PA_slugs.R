# slugs baby lettuce goooo 

# packages ####
library(tidyverse)
library(emmeans)
library(lme4)
# data ####
slugs <- PSA_PA_slugs

# clean this jawn ####
slugs

# cleaning_the_slug <-
  
colnames(slugs)
test_slug <- slugs[1:200,]
test_slug %>% 
  select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  select(-date, -precip) %>% 
  group_by(season, year, month, plot_id, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count)) %>% 
  print(n = Inf)
?summarise
# it is by month, so the numbers may seem larges

# whole data set 

slug_clean <- slugs %>% 
  select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>% 
  group_by(season, year, month, plot_id, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)

#subset by season

fall_slugs <- subset(slug_clean, season == "fall")
spring_slugs <- subset(slug_clean, season == "spring")

# models ####

#over dispersion check: yes
mean(fall_slugs$total_slug, na.rm = TRUE)
var(fall_slugs$total_slug, na.rm = TRUE)
mean(spring_slugs$total_slug, na.rm = TRUE)
var(spring_slugs$total_slug, na.rm = TRUE)

# spring models 
# test model: 
test_spring_model <- glmer.nb(total_slug ~ treatment+
                                (1|block/plot_id)+ (1|month), data = spring_slugs)
summary(test_spring_model)

# the random of effects of this model are extremely low, this means I likely do not need to include them
spring_model <- glmer.nb(total_slug ~ treatment +
                           (1|block) + (1|month), 
                         data = spring_slugs)
summary(spring_model)
hist(residuals(spring_model))
spring_emm <- emmeans(spring_model, pairwise ~ treatment, type = "response")
pairs(spring_emm) # will print the contrasts
plot(spring_emm$emmeans)

ggplot(spring_slugs, aes(x = treatment, y = total_slug, fill = year))+
  geom_boxplot()+
  facet_wrap(~year)+
  ggtitle("Total Spring Slugs by Treatment")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
  ylab("Total slug counts")+
  xlab("")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size = 12))






ggplot(fall_slugs, aes(x = treatment, y = total_slug, fill = year))+
  geom_boxplot()+
  facet_wrap(~year)+
  ggtitle("Total Spring Slugs by Treatment")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
  ylab("Total slug counts")+
  xlab("")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size = 12))
