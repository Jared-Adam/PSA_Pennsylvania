# Jared Adam 
# beans slugs = 2022 and 2023
# started on the plane 
# adding here slug populations models and figs
# slug x predator regressions and plots 

# packages ####
library(tidyverse)
library(MASS)
library(performance)
library(lme4)
library(emmeans)
library(lmtest)


# data ####
slugs <- slugs_beans_all %>% 
  mutate(slug_count = as.numeric(slug_count)) %>% 
  rename(precip = '7_day_precip_in') %>% 
  mutate(temp = as.numeric(temp)) %>% 
  mutate(treatment = case_when(plot %in% c(101,203,304,401,503) ~ 1,
                               plot %in% c(103,204,302,403,501) ~ 2,
                               plot %in% c(102,201,303,402,502) ~ 3, 
                               plot %in% c(104,202,301,404,504) ~ 4)) %>% 
  mutate(block = case_when(plot %in% c(101,102,103,104) ~ 1,
                           plot %in% c(201,202,203,204) ~ 2,
                           plot %in% c(301,302,303,304) ~ 3,
                           plot %in% c(401,402,403,404) ~ 4,
                           plot %in% c(501,502,503,504) ~ 5)) %>% 
  mutate(block = as.factor(block)) %>%
  dplyr::select(-location, -shingle_id, -time, -temp, -row) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         year = format(date, '%Y'))  %>% 
  dplyr::select(-date, -precip) %>% 
  mutate(year = as.factor(year), 
       treatment = as.factor(treatment))%>% 
  rename(season = seaon) %>% 
  mutate(season = case_when(season == "fall" ~ "Fall",
                            season == "spring" ~ "Spring"))%>% 
  group_by(season, year, month, plot, treatment, block) %>% 
  summarise(total_slug =  sum(slug_count))%>% 
  print(n = Inf)
slugs <- slugs[1:160,]
slugs <- slugs %>% 
  replace(is.na(.),0) %>% 
  print(n = Inf)
unique(slugs$treatment)
unique(slugs$season)


#subset by season

fall_slugs <- subset(slugs, season == "fall")
spring_slugs <- subset(slugs, season == "spring")

# models ####

# look at overdispersion: variance > mean?
dispersion_stats <- slugs %>% 
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



# let's see which is better, poisson or nb? 
# run one of each where the only difference is the family 
poisson_model <- glmer(total_slug ~ treatment + 
                         (1|year/block), 
                       data = slugs, 
                       family = poisson)

nb_model_trt <- glmer.nb(total_slug ~ treatment + 
                           (1|year/block), 
                         data = slugs) 

lrtest(poisson_model,nb_model_trt)
# the negative binomial has the higher likelihood score, so we will use that

#actual model: 
unique(slugs$season)
is.factor(slugs$season)
slugs <- slugs %>% 
  mutate(season = as.factor(season))
model_1 <- glmer.nb(total_slug ~ treatment + (1|year/block), 
                    data = slugs)
summary(model_1)
r2_nakagawa(model_1)
binned_residuals(model_1)
br_1 <- binned_residuals(model_1)
plot(br_1)


# glm for trt*szn no random 
model_2 <- MASS::glm.nb(total_slug ~ treatment + season, data = slugs)
summary(model_2)
hist(residuals(model_2))

# spring sig diff than fall and trt 4 sig diff from 1
total_slug_22 <- dplyr::filter(slugs, year == "2022")
model_3 <- MASS::glm.nb(total_slug ~ treatment + season,
                        data = total_slug_22)
summary(model_3)
hist(residuals(model_3))

# sping sig from fall 
total_slug_23 <- filter(slugs, year == "2023")
model_4 <- MASS::glm.nb(total_slug ~ treatment + season, 
                       data = total_slug_23)
summary(model_4)
hist(residuals(model_4))




# significance between fall and spring, but not year or treatment

# plots for slug populations  ####
# add sig values in ppt: confusing with two factor facets
ggplot(slugs, aes(x = as.character(treatment), y = total_slug, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~year + season, scales = "free_y")+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(labels=c("Check", "Brown", "Green", "GrBr"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Beans: Total Spring Slugs by Treatment",
        subtitle = " Years: 2022-2023")+
  theme(legend.position = "none",
        axis.text.x = element_text(size=18, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(s = 16), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# slugs x predators data ####
colnames(slugs)
slugs_tot_beasn <- slugs %>% 
  replace(is.na(.),0) %>% 
  group_by(year, treatment) %>% 
  summarise(slug_total_trt = sum(total_slug))


# abundance df code 
bpf <- bean_pf

bpf_wide <- bpf %>% 
  dplyr::select(-split, -life_stage, -sp, -genus) %>% 
  group_by(date, plot) %>% 
  pivot_wider(names_from = family, 
              values_from = family,
              values_fn = list(family = length)) %>% 
  print(n = Inf)

colnames(bpf_wide)
bpf_wider <- bpf_wide  %>% 
  replace(is.na(.),0) %>% 
  mutate(Lin = Liniphiide + Lyniphiidae + Linyphiidae) %>% 
  dplyr::select(-Liniphiide, -Lyniphiidae, -Linyphiidae, -na) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"), 
         year = format(date, "%Y")) %>% 
  relocate(year) %>% 
  mutate(year = as.factor(year)) %>% 
  mutate_at(3:5, as.factor) 
colnames(bpf_wide)

bpf_clean <- bpf_wider %>% 
  mutate(trt = as.factor(case_when(plot %in% c(101,203,304,401,503) ~ 1,
                         plot %in% c(103,204,302,403,501) ~ 2,
                         plot %in% c(102,201,303,402,502) ~ 3, 
                         plot %in% c(104,202,301,404,504) ~ 4))) %>% 
  na.omit() %>%  
  dplyr::select(-crop) %>% 
  mutate(crop = 'beans',
         crop = as.factor(crop)) %>% 
  relocate(year, date, crop) %>% 
  print(n = Inf)
colnames(bpf_clean)

unique(bpf_clean$date)
pred_tot_beans <- bpf_clean %>% 
  mutate(Predators = Lycosidae + Formicidae + Carabidae + Thomisidae + Coleoptera +
           Staphylinidae + Gryllidae + Pterostichus + Tetragnathidae + Chilopoda +
           Cicindelidae + Gnaphosidae + Agelenidae + Lin + Opiliones) %>% 
  group_by(year, trt) %>% 
  summarise(total = sum(Predators))


slugs_preds <- cbind(pred_tot_beans, slugs_tot_beasn) %>% 
  dplyr::select(year...1, trt, total, slug_total_trt) %>% 
  rename(year = year...1, 
         pred_total = total) %>% 
  mutate(pred_total = as.numeric(pred_total))

# slug X predators model ####
# pred_tot_beans.model <- bpf_clean %>% 
#   mutate(Predators = Lycosidae + Formicidae + Carabidae + Thomisidae + Coleoptera +
#            Staphylinidae + Gryllidae + Pterostichus + Tetragnathidae + Chilopoda +
#            Cicindelidae + Gnaphosidae + Agelenidae + Lin)
# colnames(pred_tot_beans.model)
# 
# slugs_clean.model<- slugs %>% 
#   replace(is.na(.),0)
# colnames(slugs_clean.model)
# slugs_preds

poisson_model.2 <- glm(slug_total_trt ~ pred_total,
                       data = slugs_preds,
                       family = poisson)

nb_model_trt.2 <- glm.nb(slug_total_trt ~ pred_total, 
                         data = slugs_preds) 

lrtest(poisson_model.2,nb_model_trt.2)
# the negative binomial has the higher likelihood score, so we will use that

pred_slug.1 <- glm.nb(slug_total_trt ~ pred_total, 
                      data = slugs_preds)
summary(pred_slug.1)
hist(residuals(pred_slug.1))


# slug X predators plot ####
ggplot(slugs_preds, aes(x = pred_total, y = slug_total_trt, color = trt, shape = year))+
  geom_point(size = 6)+
  geom_smooth(method = 'lm')+
  facet_wrap(~year, scales = "free")+
  labs(title = "Soybean Slug x Predator Population",
       x = "Predator Total", 
       y = "Slug Population")+
  theme(strip.text = element_text(size = 10))
  
































