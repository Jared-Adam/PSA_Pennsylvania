# slugs baby lettuce goooo 

# packages ####
library(tidyverse)
library(emmeans)
library(lme4)
library(lmtest)
library(MASS)
library(nlme)
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
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>% 
  group_by(season, year, month, plot_id, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)

#subset by season

fall_slugs <- subset(slug_clean, season == "fall")
spring_slugs <- subset(slug_clean, season == "spring")

# models ####

# look at overdispersion: variance > mean?
dispersion_stats <- slug_clean %>% 
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




# spring models 
# test model: 
test_spring_model <- glmer.nb(total_slug ~ treatment+
                                (1|block/plot_id)+ (1|month), data = spring_slugs)
summary(test_spring_model)

# # the random of effects of this model are extremely low, this means I likely do not need to include them
# spring_model <- glmer.nb(total_slug ~ treatment +
#                            (1|block) + (1|month), 
#                          data = spring_slugs)
# summary(spring_model)
# spring_emm <- emmeans(spring_model, pairwise ~ treatment, type = "response")
# pairs(spring_emm) # will print the contrasts
# plot(spring_emm$emmeans)

# glm.nb from MASS
glm_test <- glm(total_slug ~ treatment, family = poisson, data = spring_slugs)
summary(glm_test)

glm.nb_test <-glm.nb(total_slug ~ treatment + year, data = spring_slugs)
summary(glm.nb_test)


# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 
poisson_model <- glmer(total_slug ~ treatment + 
                         (1|year/block), 
                       data = slug_clean, 
                       family = poisson)

nb_model_trt <- glmer.nb(total_slug ~ treatment + 
                           (1|year/block), 
                         data = slug_clean) 

lrtest(poisson_model,nb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that


# with random of block, plot, and season: the models are singular 
m1 <- glmer.nb(total_slug ~ treatment +
                 (1|year), data = slug_clean) 


summary(m1)
check_singularity(m1)
r2_nakagawa(m1)
binned_residuals(m1)
m1_r <- binned_residuals(m1)
plot(m1_r)


# jw model attempt 

lme_df <- na.omit(slug_clean)
jw_m <- nlme::lme(total_slug ~ treatment, random = ~ 1|year, 
             correlation = nlme::corCAR1(form=~1|year), 
             data = lme_df)

summary(jw_m)
check_singularity(jw_m)
r2_nakagawa(jw_m)
binned_residuals(jw_m)
jw_m_r <- binned_residuals(jw_m)
plot(jw_m_r)










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
