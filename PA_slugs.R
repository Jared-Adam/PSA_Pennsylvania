# slugs baby lettuce goooo 

# packages ####
library(tidyverse)
library(emmeans)
library(lme4)
library(MASS)
# data ####
slugs <- PSA_PA_slugs

# clean this jawn ####
slugs

# cleaning_the_slug <-
  
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

# glm.nb from MASS
glm_test <- glm(total_slug ~ treatment, family = poisson, data = spring_slugs)
summary(glm_test)
hist(residuals(glm_test))

glm.nb_test <-glm.nb(total_slug ~ treatment + year, data = spring_slugs)
summary(glm.nb_test)
hist(residuals(glm.nb_test))
shapiro.test(residuals(glm.nb_test))


# 1/16/2023
# I am unsure which model to run. the random effects model is out because the random effects has little weight. 
# i then tried a glm and glm.nb, but the residuals are ass. 
# I refuse to transform, so what is next? 










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
