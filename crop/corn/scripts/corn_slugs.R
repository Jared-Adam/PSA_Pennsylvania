# Jared Adam
# first date unknown
#revisit : 2/14/2024

# slugs baby lettuce goooo 

# packages ####
library(tidyverse)
library(emmeans)
library(lme4)
library(lmtest)
library(MASS)
library(nlme)
library(plotly)
# data ####
slugs <- PSA_PA_slugs

# test ####
  
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



# whole data set ####

slug_clean <- slugs %>% 
  dplyr::select(-location, -shingle_id, -time, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y')) %>% 
  rename(precip = "7day_precip") %>% 
  dplyr::select(-date) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>%
  mutate(season = case_when(season == "fall" ~ "Fall", 
                            season == "spring" ~ "Spring")) %>% 
  group_by(season, year, month, plot_id, treatment, block, precip, temp) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  replace(is.na(.),0) %>% 
  print(n = Inf)

#subset by season

fall_slugs <- subset(slug_clean, season == "fall")
spring_slugs <- subset(slug_clean, season == "spring")
cs_21 <- subset(slug_clean, year == "2021")
cs_22 <- subset(slug_clean, year == "2022")
cs_23 <- subset(slug_clean, year == "2023")
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
                 (1|year) + (1|precip), data = slug_clean) 


summary(m1)
check_singularity(m1)
r2_nakagawa(m1)
#  Conditional R2: 0.813
#  Marginal R2: 0.002

# without precip: 
# Conditional R2: 0.035
# Marginal R2: 0.007

binned_residuals(m1)
m1_r <- binned_residuals(m1)
plot(m1_r)

# checking for trt differences in slug populations 
# none
m2 <- kruskal.test(total_slug ~ treatment, data = cs_21)
m3 <- kruskal.test(total_slug ~ treatment, data  = cs_22)
m4 <- kruskal.test(total_slug ~ treatment, data = cs_23)


# plots corn slugs ####
ggplot(slug_clean, aes(x = treatment, y = total_slug, fill = season))+
  geom_boxplot()+
  facet_wrap(~year)+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Total Spring Slugs by Treatment")+
  theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12))

ggplot(fall_slugs, aes(x = treatment, y = total_slug, fill = year))+
  geom_boxplot()+
  facet_wrap(.~year)+
  ggtitle("Total Slugs by Treatment")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
  ylab("Total slug counts")+
  xlab("")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size = 12))

test <- ggplot(slug_clean, aes(x = treatment, y = total_slug, fill = treatment))+
  geom_boxplot()+
  facet_wrap(year~season, scales = "free_y", ncol = 2)+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Corn: Total Slugs by Treatment",
        subtitle = " Years: 2021-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




ggplotly(test)
# slugs and precip ####
precip_slug <- slug_clean %>% 
  group_by(season, year, treatment) %>% 
  summarise(precip_tot = sum(precip),
            slug_tot = sum(total_slug)) 

precip <- kruskal.test(precip_tot ~ year, data = precip_slug)
precip
pairwise.wilcox.test(precip_slug$precip_tot, precip_slug$year)
hist(residuals(precip))

ggplot(precip_slug, aes(x = precip_tot, y = slug_tot))+
  geom_point()+
  facet_wrap(~year + season)

ggplot(precip_slug, aes(x = year, y = precip_tot, fill = year))+
  geom_bar(position = "dodge", stat = "identity")+
  facet_wrap(~season)


