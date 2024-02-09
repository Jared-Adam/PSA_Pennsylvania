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
  group_by(season, year, month, plot_id, treatment, block, precip, temp) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  replace(is.na(.),0)
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
  facet_wrap(~year)+
  ggtitle("Total Spring Slugs by Treatment")+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "Gr-Br"))+
  ylab("Total slug counts")+
  xlab("")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size = 12))

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




# slugs X predators data ####
slug_clean %>% 
replace(is.na(.),0)%>% 
  group_by(year, treatment) %>% 
  summarise(slug_total_trt = sum(total_slug))

slug_tot_corn <- slug_clean %>% 
  filter(year != "2021") %>% 
  replace(is.na(.),0)%>% 
  group_by(year, treatment) %>% 
  summarise(slug_total_trt = sum(total_slug))

# abundance df code 

pf <- corn_pf

pf_wide <- pf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(pf_wide)
pf_wider <- pf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lyn = Liniphiidae + Lyniphiidae + Linyphiidae, 
         Staph= Staphylinidae + Staphylinidaa) %>% 
  dplyr::select(-Liniphiidae, -Lyniphiidae, -Linyphiidae, -Staphylinidae, -Staphylinidaa, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(pf_wide)

cpf_clean <- pf_wider %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                         plot %in% c(103,204,302,403,501) ~ 2,
                         plot %in% c(102,201,303,402,502) ~ 3, 
                         plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%
  print(n = Inf)
colnames(pf_clean)

pred_tot_corn <- cpf_clean %>% 
  mutate(Predators = Lycosidae + Formicidae + Opiliones + Carabidae + Thomisidae +
           Staph+ Gryllidae + Gyrillidae + Cicindelidae + Tetrgnathidae + Chilopoda +
           Cicindelidae + Gnaphosidae+ Lyn + Araneae + Salticidae) %>% 
  group_by(year, trt) %>% 
  summarise(total = sum(Predators))


c_slug_pres <- cbind(pred_tot_corn, slug_tot_corn) %>% 
  dplyr::select(year...1, trt, total, slug_total_trt) %>% 
  rename(year = year...1, 
         pred_tot = total) %>% 
  mutate(pred_tot = as.numeric(pred_tot))


# slugs x predators models ####
# 
# pred_tot_corn.model <- cpf_clean %>% 
#   mutate(Predators = Lycosidae + Formicidae + Opiliones + Carabidae + Thomisidae +
#            Staph+ Gryllidae + Gyrillidae + Cicindelidae + Tetrgnathidae + Chilopoda +
#            Cicindelidae + Gnaphosidae+ Lyn + Araneae + Salticidae)
# colnames(pred_tot_corn.model)
# 
# corn_slugs_clean.model<- slugs %>% 
#   replace(is.na(.),0)
# colnames(slugs_clean.model)
# slugs_preds



poisson_model.3 <- glm(slug_total_trt ~ pred_tot,
                       data = c_slug_pres,
                       family = poisson)

nb_model_trt.3 <- glm.nb(slug_total_trt ~ pred_tot, 
                         data = c_slug_pres) 

lrtest(poisson_model.3,nb_model_trt.3)
# the negative binomial has the higher likelihood score, so we will use that

pred_slug.2 <- glm.nb(slug_total_trt ~ pred_tot, 
                      data = c_slug_pres)
summary(pred_slug.2)
hist(residuals(pred_slug.2))

# weird relationship with slugs 


# slugs x predators plots ####

test <- ggplot(c_slug_pres, aes(x = pred_tot, y = slug_total_trt, color = trt, shape = year))+
  geom_point(size = 6)+
  geom_smooth(method = 'lm')+
  facet_wrap(~year, scales = "free")+
  labs(title = "Corn Slug x Predator Population",
       x = "Predator Total", 
       y = "Slug Population")


ggplotly(test)
