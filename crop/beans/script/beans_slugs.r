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
bs_22 <- subset(slugs, year == "2022")
bs_23 <- subset(slugs, year == "2023")

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
model_1 <- glmer.nb(total_slug ~ treatment+ (1|year/block), 
                    data = slugs)
summary(model_1)
r2_nakagawa(model_1)
binned_residuals(model_1)
br_1 <- binned_residuals(model_1)
plot(br_1)
s_emm <- emmeans(model_1, ~treatment, type = "response")
pairs(s_emm)

# confirming trt differences 
m3 <- kruskal.test(total_slug ~ treatment, data  = bs_22)
m5 <- kruskal.test(total_slug ~ season, data  = bs_22)

m4 <- kruskal.test(total_slug ~ treatment, data = bs_23)
m6 <- kruskal.test(total_slug ~ season, data = bs_23)


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
slugs$szn <- factor(slugs$season, levels = c("Spring", "Fall"))
ggplot(slugs, aes(x = as.character(treatment), y = total_slug, fill = treatment))+
  geom_boxplot()+
  facet_wrap(~year + szn, scales = "free_y")+
  scale_fill_manual(values = c("#E7298A", "#D95F02", "#1B9E77", "#7570B3"))+
  scale_x_discrete(limits = c("1", "2", "4", "3"),
                   labels=c("Check", "Brown", "GrBr", "Green"))+
  labs( x = 'Treatment',
        y = 'Total Slug Counts', 
        title = "Beans: Total Slugs by Treatment",
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






























